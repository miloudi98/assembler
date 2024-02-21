#include "lib/front_end/parser.hh"
#include "lib/support/core.hh"
#include "lib/front_end/ctx.hh"
#include "lib/codegen/instrs/shard0.hh"

namespace {

constexpr auto prefix_prec_of_tok_kind(fiska::x86::fe::TK tk) -> i8 {
    using fiska::x86::fe::TK;

    switch (tk) {
    case TK::Plus:
    case TK::Minus:
        return 1;
    default: return 0;
    } // switch
}

constexpr auto infix_prec_of_tok_kind(fiska::x86::fe::TK tk) -> i8 {
    using fiska::x86::fe::TK;

    switch (tk) {
    case TK::Plus:
    case TK::Minus:
        return 1;
    default: return 0;
    } // switch
}

constexpr auto parse_i64(StrRef num_lxm) -> i64 {
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

    assert(curr.empty(), "Invalid number encountered: '{}'.", num_lxm);
    return i64(ret);
}

} // namespace

auto fiska::x86::fe::Parser::parse_expr(i8 prec) -> Expr* {
    Expr* lhs = nullptr;

    Tok curr_tok = tsv_.peek();
    auto span_start_it = tsv_.current();
    defer {
        lhs->span_ = curr_tok.loc_;
        // Set the span of the expression.
        std::for_each(
            span_start_it,
            tsv_.current(),
            [lhs](const Tok& tok) { lhs->expand(tok); }
        );
    };
    
    switch (curr_tok.kind_) {
    case TK::Ident: {
        ingest(TK::Ident);
        lhs = new (ctx_) LabelExpr(tsv_.peek(-1).str_);
        break;
    }
    case TK::Num: {
        ingest(TK::Num);
        lhs = new (ctx_) IntLitExpr(parse_i64(tsv_.peek(-1).str_)); 
        break;
    }
    case TK::Plus:
    case TK::Minus: {
        tsv_.consume(TK::Plus, TK::Minus);

        i8 infix_prec = prefix_prec_of_tok_kind(curr_tok.kind_);
        Expr* inner = parse_expr(infix_prec);
        lhs = new (ctx_) UnaryOpExpr(curr_tok.kind_, inner);
        break;
    }
    default:
        goto invalid_expression;

    } // switch

    while (infix_prec_of_tok_kind(tsv_.peek().kind_) > prec) {
        TK binop = tsv_.peek().kind_;
        ingest(binop);

        i8 infix_prec = infix_prec_of_tok_kind(binop);
        Expr* rhs = parse_expr(infix_prec);
        lhs = new (ctx_) BinaryOpExpr(binop, lhs, rhs);
    }

    return lhs;

invalid_expression:
    ErrorSpan::emit(
        ErrorSpan::from(
            ctx_,
            tsv_.tok().loc_,
            "Token '{}' does not start a valid expression.",
            Lexer::str_of_tk(tsv_.peek().kind_)
        )
    );
}

auto fiska::x86::fe::Parser::ingest(std::same_as<TK> auto... tk) -> void {
    auto [mmatch, diff] = tsv_.mismatch(tk...);
    if (not mmatch) { return; }

    ErrorSpan::emit(ErrorSpan::from(
        ctx_,
        tsv_.tok().loc_,
        "Expected Token: '{}' but found '{}' instead.",
        diff.first,
        diff.second
    ));
}

auto fiska::x86::fe::Parser::parse_x86_instruction() -> X86InstrExpr* {
    ingest(TK::Mnemonic, TK::LParen);

    todo();
}

auto fiska::x86::fe::Parser::parse_proc() -> ProcExpr* {
    auto proc = new (ctx_) ProcExpr;

    ingest(TK::Fn, TK::Ident);

    proc->name_ = tsv_.peek(-1).str_;


    ingest(TK::LBrace);
    while (not tsv_.at(TK::RBrace)) {
        proc->body_.push_back(parse_x86_instruction());
    }
    ingest(TK::RBrace);

    return proc;
}

auto fiska::x86::fe::Parser::parse_section() -> Expr::List {
    Expr::List ast;
    while (not tsv_.at(TK::Eof)) {

        ingest(TK::Section, TK::Ident);
        section_ = tsv_.peek(-1).str_;

        ingest(TK::LBrace);
        while (not tsv_.at(TK::RBrace)) {
            ast.push_back(parse_proc());
        }
        ingest(TK::RBrace);
    }
    return ast;
}


//auto fiska::x86::fe::Parser::parse_proc() -> ProcExpr* {
//    auto proc = new (ctx_, curr_section_) ProcExpr;
//    proc->span_ = peek_tok().loc_;
//
//    auto span_start_it = tok_stream_it_;
//    defer {
//        // Set the span of the expression.
//        std::for_each(
//            span_start_it,
//            tok_stream_it_,
//            [proc](const Tok& tok) { proc->expand(tok); }
//        );
//    };
//
//    expect_all(TK::Fn, TK::Ident, TK::LBrace);
//
//    proc->name_ = peek_tok_str(-2);
//
//    while (not at(TK::RBrace)) {
//        proc->body_.push_back(parse_x86_instr_expr());
//    }
//    expect(TK::RBrace);
//
//    return proc;
//}
//
//auto fiska::x86::fe::Parser::parse_x86_instr_expr() -> X86InstrExpr* {
//    auto x86_instr = new (ctx_, curr_section_) X86InstrExpr;
//    x86_instr->span_ = peek_tok().loc_;
//
//    auto span_start_it = tok_stream_it_;
//    defer {
//        // Set the span of the expression.
//        std::for_each(
//            span_start_it,
//            tok_stream_it_,
//            [x86_instr](const Tok& tok) { x86_instr->expand(tok); }
//        );
//    };
//
//    expect_all(TK::Mnemonic, TK::LParen);
//
//    x86_instr->mmic_ = Lexer::mmic_of_str(peek_tok_str(-2));
//
//    while (not at(TK::RParen)) {
//        x86_instr->ops_.push_back(parse_expr());
//        assert(at(TK::RParen) or consume(TK::Comma));
//    }
//    expect_all(TK::RParen, TK::SemiColon);
//
//    return x86_instr;
//}
//
//
//auto fiska::x86::fe::Parser::parse_file() -> Expr::List {
//    Expr::List ast;
//    while (not at(TK::Eof)) {
//        expect_all(TK::Section, TK::Ident, TK::LBrace);
//        curr_section_ = peek_tok_str(-2);
//
//        while (not at(TK::RBrace)) {
//            ast.push_back(parse_proc());
//        }
//        expect(TK::RBrace);
//    }
//    return ast;
//}
//
//    case TK::At:
//    case TK::BitWidth: {
//        // Register.
//        if (match(TK::BitWidth, TK::Reg)) {
//            auto reg = new (ctx_, curr_section_) RegLitExpr;
//
//            expect_all(TK::BitWidth, TK::Reg);
//
//            reg->bw_ = Lexer::bw_of_str(peek_tok_str(-2));
//            reg->id_ = Lexer::rid_of_str(peek_tok_str(-1));
//
//            lhs = reg;
//            break;
//        }
//
//        // Memory reference.
//        if (match(TK::At, TK::BitWidth, TK::LBracket)) {
//            auto mem = new (ctx_, curr_section_) MemRefLitExpr;
//
//            expect_all(TK::At, TK::BitWidth);
//
//            mem->bw_ = Lexer::bw_of_str(peek_tok_str(-1));
//
//            // Base register. The brackets enclosing the base register should always
//            // be present to distinguish the expression from an absolute memory offset.
//            expect(TK::LBracket);
//            if (consume(TK::Reg)) {
//                mem->brid_ = Lexer::rid_of_str(peek_tok_str(-1));
//            }
//            expect(TK::RBracket);
//
//            // Scale and index register.
//            if (at(TK::LBracket)) {
//                // Scale.
//                expect_all(TK::LBracket);
//                mem->scale_ = parse_expr();
//                expect(TK::RBracket);
//
//                // Index register.
//                expect_all(TK::LBracket, TK::Reg);
//                mem->irid_ = Lexer::rid_of_str(peek_tok_str(-1));
//                expect(TK::RBracket);
//            }
//
//            lhs = mem;
//            break;
//        }
//        
//        // Memory offset.
//        if (match(TK::At, TK::BitWidth)) {
//            expect_all(TK::At, TK::BitWidth);
//
//            auto moffs = new (ctx_, curr_section_) MoffsLitExpr;
//            moffs->bw_ = Lexer::bw_of_str(peek_tok_str(-1));
//            moffs->addr_ = parse_expr();
//            lhs = moffs;
//            break;
//        }
//
//        // Immediate
//        if (match(TK::BitWidth)) {
//            expect(TK::BitWidth);
//            
//            auto imm = new (ctx_, curr_section_) ImmLitExpr;
//            imm->bw_ = Lexer::bw_of_str(peek_tok_str(-1));
//            imm->value_ = parse_expr();
//            lhs = imm;
//            break;
//        }
//
//        goto invalid_expression;
//    }
//struct Parser {
//    Ctx* ctx_{};
//    u16 fid_{};
//    TokStream::Iterator tok_stream_it_;
//    StrRef curr_section_;
//
//    explicit Parser(Ctx* ctx, u16 fid);
//
//    auto next_tok() -> void;
//    auto tok() -> const Tok&;
//    auto peek_tok(i32 idx = 0) -> const Tok&;
//    auto peek_tok_kind(i32 idx = 0) -> TK { return peek_tok(idx).kind_; }
//    auto peek_tok_str(i32 idx = 0) -> StrRef { return peek_tok(idx).str_; }
//
//    auto parse_file() -> Expr::List;
//    auto parse_proc() -> ProcExpr*;
//    auto parse_x86_instr_expr() -> X86InstrExpr*;
//    auto parse_expr(i8 prec = 0) -> Expr*;
//    auto perform_constant_folding(Expr*) -> Expr*;
//
//    static auto parse_i64(StrRef str) -> i64;
//    static auto prefix_prec_of_tok_kind(TK tk) -> i8;
//    static auto infix_prec_of_tok_kind(TK tk) -> i8;
//
//    // Helper methods.
//    auto at(std::same_as<TK> auto... tk) -> i1 {
//        return ((tok().kind_ == tk) or ...);
//    }
//
//    auto consume(std::same_as<TK> auto... tk) -> i1 {
//        if (not at(tk...)) { return false; }
//        next_tok();
//        return true;
//    }
//
//    // TODO(miloudi): Have proper error handling please.
//    auto expect_any(std::same_as<TK> auto... tk) -> void {
//        if (consume(tk...)) { return; }
//
//        ErrorSpan::emit(
//            ErrorSpan::from(
//                ctx_,
//                tok().loc_,
//                "Expected one of the following tokens: '{}' but found '{}' instead.",
//                fmt::format("[{}]", fmt::join(Vec<StrRef>{Lexer::str_of_tk(tk)...}, " ")),
//                Lexer::str_of_tk(tok().kind_)
//            )
//        );
//    }
//
//    auto expect_all(std::same_as<TK> auto... tk) -> void {
//        auto match_helper = [&](TK tk) {
//            if (consume(tk)) { return; }
//
//            ErrorSpan::emit(
//                ErrorSpan::from(
//                    ctx_,
//                    tok().loc_,
//                    "Expected Token: '{}' but found '{}' instead.",
//                    Lexer::str_of_tk(tk),
//                    Lexer::str_of_tk(tok().kind_)
//                )
//            );
//        };
//
//        (match_helper(tk), ...);
//    }
//
//    auto expect(TK tk) -> void { expect_all(tk); }
//
//    auto match(std::same_as<TK> auto... tk) -> i1 {
//        i32 idx = 0;
//        return ((peek_tok_kind(idx++) == tk) and ...);
//    }
//};

