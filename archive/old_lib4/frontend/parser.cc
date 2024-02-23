#include "lib/frontend/parser.hh"
#include "lib/frontend/lexer.hh"
#include "lib/common/base.hh"

namespace fiska::assembler::frontend {
namespace {

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

auto parse_i64(StrRef num_lxm) -> u64 {
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

    // Error parsing the number.
    if (not curr.empty()) { 
        return std::nullopt;
    }

    return ret;
}

#define track_span(expr)                                         \
    i32 span_start_idx = p->tok_idx_;                            \
    defer {                                                      \
        expr->span_ = span_from(span_start_idx);                 \
    } 

struct Parser {
    Ctx* ctx_{};
    u16 fid_{};
    i32 tok_idx_{};
    TokStream tok_stream_;

    explicit Parser(Ctx* ctx, u16 fid) :
        ctx_(ctx), fid_(fid), tok_stream(lex(ctx, fid))
    {}

    auto tok() const -> const Tok& { return tok_stream_[tok_idx_]; }

    auto tok_kind() const -> TK { return tok().kind_; }

    auto peek(i32 idx = 0) const -> const Tok& {
        // Bounds checking happens inside the |TokStream| class.
        return tok_stream_[tok_idx_ + idx];
    }

    auto peek_kind(i32 idx = 0) const -> TK { return peek(idx).kind_; }

    auto peek_str(i32 idx = 0) const -> StrRef { return peek(idx).str_; }

    auto peek_span(i32 idx = 0) const -> Span { return peek(idx).span_; }

    auto at(std::same_as<TK> auto... tk) -> i1 {
        return (tok_kind() == tk or ...);
    }

    auto advance() -> void { tok_idx_++; }

    auto consume(std::same_as<TK> auto... tk) -> i1 {
        if (at(tk...)) {
            advance();
            return true;
        }
        return false;
    }

    auto ingest(std::same_as<TK> auto... tk) -> void {
        auto check = [&](TK tk) {
            if (not cosume(tk)) Error<UnexpectedToken>(tk, tok().kind_);

            //if (consume(tk)) { return; }
            //Diagnostic {
            //    ctx_,
            //    fmt::format("Expected '{}', found '{}'.", tk, tok_kind()),
            //    tok_span()
            //}; 
        };
        (check(tk), ...);
    }

    auto match(std::same_as<TK> auto... tk) -> i1 {
        i32 idx = 0;
        return ((peek_kind(idx++) == tk) and ...);
    }

    auto parse_expression(i8 prec = 0) -> Expr* {
        Expr* lhs = nullptr;
        auto start_loc = tok_span();

        switch (peek_kind()) {
        case TK::Ident: {
            lhs = new (ctx_) LabelExpr(tok().str_, tok().span_);
            consume(TK::Ident);
            break;
        }
        case TK::Num: {
            Opt<u64> int_value = parse_i64(tok().str_);
            if (not value) Error<InvalidNumber>(tok().str_, tok().span_);

            lhs = new (ctx_) IntExpr(int_value, tok().span_);
            break;
        }
        case TK::
        } // switch
    }

    auto parse_x86_operand() -> X86Op* {
        todo();
    }

    auto parse_proc() -> Proc* {
        todo();
    }

    auto parse_var() -> Var* {
        todo();
    }

    auto parse_section() -> Section {
        todo();
    }

    auto parse_x86_instruction() -> X86Inst {
        todo();
    }
};

#undef track_span

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

    // need a parse section function here.
    while (not at(&p, TK::Eof)) {
        ingest(&p, TK::Section, TK::Ident, TK::LBrace);
        p.section_ = peek(&p, -2).str_;

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

