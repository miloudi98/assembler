#include "lib/frontend/parser.hh"
#include "lib/frontend/lexer.hh"
#include "lib/common/base.hh"

namespace fiska::assembler::frontend {
namespace {

struct Parser {
    Ctx* ctx_{};
    u16 fid_{};
    i32 tok_idx_{};
    TokStream tok_stream_;
    StrRef section_;

    explicit Parser(Ctx* ctx, u16 fid) :
        ctx_(ctx), fid_(fid), tok_stream_(lex(ctx, fid)) 
    {}
};


#define track_span(expr)                                         \
    i32 span_start_idx = p->tok_idx_;                            \
    defer {                                                      \
        expr->span_ = span_from(p, span_start_idx);              \
        expr->section_ = p->section_;                            \
    } 

constexpr auto prefix_prec_of_tok_kind(TK tk) -> i8 {
    switch (tk) {
    case TK::Plus:
    case TK::Minus:
        return 1;
    default: return 0;
    } // switch
}

constexpr auto infix_prec_of_tok_kind(TK tk) -> i8 {
    switch (tk) {
    case TK::Plus:
    case TK::Minus:
        return 1;
    default: return 0;
    } // switch
}

auto parse_i64(Ctx* ctx, const Tok& tok) -> i64 {
    assert(tok.kind_ == TK::Num);

    StrRef num_lxm = tok.str_;
    u8 radix{};
    if (num_lxm.starts_with("0x")) { radix = 16; num_lxm.remove_prefix(2); }
    else if (num_lxm.starts_with("0b")) { radix = 2; num_lxm.remove_prefix(2); }
    else { radix = 10; }

    StrRef curr = num_lxm;
    u64 ret{};

    while (not curr.empty()) {
        u8 ord{};
        if (curr[0] >= '0' and curr[0] <= '9') {
            ord = u8(curr[0] - '0');
        } else if (curr[0] >= 'a' and curr[0] <= 'z') {
            ord = u8(curr[0] - 'a');
        } else if (curr[0] >= 'A' and curr[0] <= 'Z') {
            ord = u8(curr[0] - 'A');
        } else {
            break;
        }

        if (ord >= radix) { break; }

        u64 old_ret = ret;
        ret = ret * radix + ord;
        
        // Overflow.
        if ((ret / radix) != old_ret) { break; }

        curr.remove_prefix(1);
    }

    if (not curr.empty()) {
        Diagnostic {
            ctx,
            fmt::format("Invalid number: '{}'.", tok.str_),
            tok.span_
        };
    }
    return i64(ret);
}

auto tok(Parser* p) -> const Tok& {
    return p->tok_stream_[p->tok_idx_];
}

auto peek(Parser* p, i32 idx = 0) -> const Tok& {
    return p->tok_stream_[p->tok_idx_ + idx];
}

auto at(Parser* p, std::same_as<TK> auto... tk) -> i1 {
    return ((tok(p).kind_ == tk) or ...);
}

auto advance(Parser* p) -> void {
    p->tok_idx_++;
}

auto span_from(Parser* p, i32 starting_idx) -> Span {
    Span ret = peek(p).span_;
    while (starting_idx < p->tok_idx_) {
        ret.include(p->tok_stream_[starting_idx++].span_);
    }
    return ret;
}

auto consume(Parser* p, std::same_as<TK> auto... tk) -> i1 {
    if (not at(p, tk...)) { return false; }
    advance(p);
    return true;
}

auto ingest(Parser* p, std::same_as<TK> auto... tk) -> void {
    auto check = [&](TK tk) {
        if (consume(p, tk)) { return; }
        Diagnostic {
            p->ctx_,
            fmt::format("Expected '{}', found '{}'.", tk, tok(p).kind_),
            tok(p).span_
        }; 
    };
    (check(tk), ...);
}

auto match(Parser* p, std::same_as<TK> auto... tk) -> i1 {
    i32 idx = 0;
    return ((peek(p, idx++).kind_ == tk) and ...);
}

auto parse_expr(Parser* p, i8 prec = 0) -> Box<Expr> {
    Box<Expr> lhs = nullptr;
    track_span(lhs);

    // Mem.
    if (match(p, TK::At, TK::BitWidth, TK::LBracket)) {
        ingest(p, TK::At, TK::BitWidth);

        MemExpr mem {
            .bw_ = X86Info::bit_width(peek(p, -1).str_)
        };

        // Base register.
        ingest(p, TK::LBracket);
        if (consume(p, TK::Reg)) {
            mem.bri_ = X86Info::register_id(peek(p, -1).str_);
        }
        ingest(p, TK::RBracket);

        // Scale and index register.
        if (consume(p, TK::LBracket)) {
            // Scale.
            mem.scale_ = parse_expr(p);
            ingest(p, TK::RBracket);

            ingest(p, TK::LBracket);
            ingest(p, TK::Reg);
            mem.iri_ = X86Info::register_id(peek(p, -1).str_);
            ingest(p, TK::RBracket);
        }

        // Displacement.
        if (consume(p, TK::Plus, TK::Minus)) {
            mem.disp_ = std::make_unique<Expr>(
                BinaryOpExpr {
                    .op_ = peek(p, -1),
                    .lhs_ = std::make_unique<Expr>(IntExpr{}),
                    .rhs_ = parse_expr(p)
                }
            );
            // TODO: Bad hack to fix the phatom span bug.
            mem.disp_->span_ = mem.disp_->as<BinaryOpExpr>().rhs_->span_;
        }


        lhs = std::make_unique<Expr>(std::move(mem));
        return lhs;
    }

    // Moffs.
    if (match(p, TK::At, TK::BitWidth)) {
        ingest(p, TK::At, TK::BitWidth);

        lhs =  std::make_unique<Expr>(
            MoffsExpr {
                .bw_ = X86Info::bit_width(peek(p, -1).str_),
                .addr_ = parse_expr(p)
            }
        );
        return lhs;
    }

    // Register.
    if (match(p, TK::BitWidth, TK::Reg)) {
        ingest(p, TK::BitWidth, TK::Reg);
        
        lhs = std::make_unique<Expr>(
            RegisterExpr {
                .bw_ = X86Info::bit_width(peek(p, -2).str_),
                .ri_ = X86Info::register_id(peek(p, -1).str_)
            }
        );
        return lhs;
    }

    // Immediate.
    if (match(p, TK::BitWidth)) {
        ingest(p, TK::BitWidth);

        lhs = std::make_unique<Expr>(
            ImmExpr {
                .bw_ = X86Info::bit_width(peek(p, -1).str_),
                .value_ = parse_expr(p)
            }
        );
        return lhs;
    }

    switch (peek(p).kind_) {
    case TK::Ident: {
        ingest(p, TK::Ident);
        
        lhs = std::make_unique<Expr>(
            LabelExpr {
                .name_ = peek(p, -1).str_
            }
        );
        break;
    }

    case TK::Num: {
        ingest(p, TK::Num);

        lhs = std::make_unique<Expr>(
            IntExpr {
                .value_ = parse_i64(p->ctx_, peek(p, -1))
            }
        );
        break;
    }

    // String literal.
    case TK::StrLit: {
        ingest(p, TK::StrLit);

        StrRef lit = peek(p, -1).str_;
        lhs = std::make_unique<Expr>(
            StrExpr {
                // Trim the surrounding '"'s.
                .lit_ = lit.substr(1, lit.size() - 1)
            }
        );
        break;
    }

    case TK::Plus:
    case TK::Minus: {
        consume(p, TK::Plus, TK::Minus);

        const Tok& op = peek(p, -1);
        i8 pref_prec = prefix_prec_of_tok_kind(op.kind_);
        lhs = std::make_unique<Expr>(
            UnaryOpExpr {
                .op_ = op,
                .inner_ = parse_expr(p, pref_prec)
            }
        );
        break;
    }

    case TK::LBracket: {
        Vec<Box<Expr>> values;

        ingest(p, TK::LBracket);
        while (not at(p, TK::RBracket)) {
            values.push_back(parse_expr(p));
            if (not at(p, TK::RBracket)) { ingest(p, TK::Comma); }
        }
        ingest(p, TK::RBracket);

        lhs = std::make_unique<Expr>(
            ArrayExpr {
                .values_ = std::move(values)
            }
        );
        break;
    }

    default:
        Diagnostic {p->ctx_, "Invalid expression.", peek(p).span_};
    } // switch

    while (infix_prec_of_tok_kind(peek(p).kind_) > prec) {
        Tok op = peek(p);
        consume(p, op.kind_);

        lhs = std::make_unique<Expr>(
            BinaryOpExpr {
                .op_ = op,
                .lhs_ = std::move(lhs),
                .rhs_ = parse_expr(p, infix_prec_of_tok_kind(op.kind_))
            }
        );
    }

    return lhs;
}

auto parse_x86_instruction(Parser* p) -> Box<Expr> {
    Box<Expr> x86instr_expr = nullptr;
    X86InstrExpr x86i;
    track_span(x86instr_expr);

    // Check if the instruction starts with a valid mnemonic.
    ingest(p, TK::Ident);
    if (not X86Info::kMnemonics.contains(peek(p, -1).str_)) {
        Diagnostic {
            p->ctx_,
            fmt::format("Unrecognized mnemonic '{}'.", peek(p, -1).str_),
            peek(p, -1).span_
        };
    }
    x86i.mnemonic_ = X86Info::mnemonic(peek(p, -1).str_);

    // Parse the operands.
    ingest(p, TK::LParen);
    while (not at(p, TK::RParen)) {
        x86i.ops_.push_back(parse_expr(p));

        if (not at(p, TK::RParen)) { ingest(p, TK::Comma); }
    }
    ingest(p, TK::RParen, TK::SemiColon);

    x86instr_expr = std::make_unique<Expr>(std::move(x86i));
    return x86instr_expr;
}

// Parsing Expressions.
auto parse_proc(Parser* p) -> Box<Expr> {
    Box<Expr> proc_expr = nullptr;
    ProcExpr proc;
    track_span(proc_expr);

    ingest(p, TK::Fn, TK::Ident);
    proc.name_ = peek(p, -1).str_;

    ingest(p, TK::LBrace);
    while (not at(p, TK::RBrace)) {
        proc.body_.push_back(parse_x86_instruction(p));
    }
    ingest(p, TK::RBrace);

    proc_expr = std::make_unique<Expr>(std::move(proc));
    return proc_expr;
}

// Global variable declarations.
auto parse_var(Parser* p) -> Box<Expr> {
    Box<Expr> var_expr = nullptr;
    VarExpr var;
    track_span(var_expr);

    ingest(p, TK::Let, TK::Ident, TK::Colon, TK::At, TK::BitWidth, TK::Eq);
    var_expr = std::make_unique<Expr>(
        VarExpr {
            .type_ = X86Info::bit_width(peek(p, -2).str_),
            .name_ = peek(p, -5).str_,
            .value_ = parse_expr(p)
        }
    );
    ingest(p, TK::SemiColon);

    return var_expr;
}

} // namespace
} // namespace fiska::assembler::frontend

auto fiska::assembler::frontend::parse(Ctx* ctx, u16 fid) -> Vec<Box<Expr>> {
    Parser p(ctx, fid);
    Vec<Box<Expr>> ast;

    while (not at(&p, TK::Eof)) {
        ingest(&p, TK::Section, TK::Ident, TK::LBrace);
        p.section_ = peek(&p, -3).str_;

        while (not at(&p, TK::RBrace)) {
            switch (peek(&p).kind_) {
            case TK::Fn:
                ast.push_back(parse_proc(&p));
                break;
            case TK::Let:
                ast.push_back(parse_var(&p));
                break;
            default:
                Diagnostic {
                    ctx,
                    fmt::format("Expected a function or a variable, found '{}'.", peek(&p).kind_),
                    peek(&p).span_
                };
            } // switch
        }
        ingest(&p, TK::RBrace);
    }
    return ast;
}

#undef track_span
