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

auto parse_i64(Parser* p, const Tok& tok) -> i64 {
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
            p->ctx_,
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
    return ((peek(p, idx).kind_ == tk) and ...);
}

auto parse_expr(Parser* p, i8 prec = 0) -> Expr {
    todo();
}

auto parse_x86_instruction(Parser* p) -> Box<X86InstrExpr> {
    auto x86i = std::make_unique<X86InstrExpr>();

    // Check if the instruction starts with a valid mnemonic.
    ingest(p, TK::Ident);
    if (not X86Info::kMnemonics.contains(peek(p, -1).str_)) {
        Diagnostic {
            p->ctx_,
            fmt::format("Unrecognized mnemonic '{}'.", peek(p, -1).str_),
            peek(p, -1).span_
        };
    }
    x86i->mnemonic_ = X86Info::mnemonic(peek(p, -1).str_);

    // Parse the operands.
    ingest(p, TK::LParen);
    while (not at(p, TK::RParen)) {
        x86i->ops_.push_back(parse_expr(p));

        if (not at(p, TK::RParen)) { ingest(p, TK::Comma); }
    }
    ingest(p, TK::RParen, TK::SemiColon);

    return x86i;
}

// Parsing Expressions.
auto parse_proc(Parser* p) -> ProcExpr {
    ProcExpr proc;

    ingest(p, TK::Fn, TK::Ident);
    proc.name_ = peek(p, -1).str_;

    ingest(p, TK::LBrace);
    while (not at(p, TK::RBrace)) {
        proc.body_.push_back(parse_x86_instruction(p));
    }
    ingest(p, TK::RBrace);

    return proc;
}

} // namespace
} // namespace fiska::assembler::frontend

fiska::assembler::frontend::Parser::Parser(Ctx* ctx, u16 fid) :  
    ctx_(ctx), fid_(fid), tok_stream_(Lexer::lex(ctx, fid)) 
{}

auto fiska::assembler::frontend::Parser::parse(Ctx* ctx, u16 fid) -> Vec<Expr> {
    Parser p(ctx, fid);
    parse_proc(&p);
    fmt::print("i64 = {}\n", parse_i64(&p, p.tok_stream_[p.tok_idx_]));
    todo();
}
