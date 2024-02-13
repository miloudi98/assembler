#include "lib/front_end/parser.hh"
#include "lib/support/core.hh"
#include "lib/front_end/ctx.hh"

auto fiska::x86::fe::Expr::operator new(usz sz, Ctx* ctx, StrRef section) -> void* {
    auto expr = static_cast<Expr*>(::operator new(sz)); 
    expr->section_ = section;
    ctx->ast_.push_back(expr);
    return expr;
}

fiska::x86::fe::Parser::Parser(Ctx* ctx, u16 fid)
    : ctx_(ctx), fid_(fid) 
{
    // Lex the file with id |fid|.
    Lexer lxr{ctx, fid};
    while (lxr.tok().kind_ != TK::Eof) { lxr.next_tok(); }
    // Intialize the iterator.
    tok_stream_it_ = ctx_->tok_streams_[fid_].begin();

}

auto fiska::x86::fe::Parser::tok() -> const Tok& {
    return *tok_stream_it_;
}

auto fiska::x86::fe::Parser::next_tok() -> void {
    // Return EOF token when we are out of tokens.
    if (tok_stream_it_ == ctx_->tok_streams_[fid_].end()) { return; }

    ++tok_stream_it_;
}

auto fiska::x86::fe::Parser::peek_tok(i32 idx) -> const Tok& {
    auto ptok = tok_stream_it_ + idx;
    // Bounds checking.
    assert(ctx_->tok_streams_[fid_].begin() <= ptok and ptok < ctx_->tok_streams_[fid_].end());
    return *ptok;
}

auto fiska::x86::fe::Parser::parse_proc() -> ProcExpr* {
    expect_all(TK::Fn, TK::Ident, TK::LBrace);

    auto proc = new (ctx_, curr_section_) ProcExpr;
    proc->name_ = peek_tok_str(-2);

    while (not at(TK::RBrace)) {
        proc->body_.push_back(parse_x86_instr_expr());
    }
    expect(TK::RBrace);

    return proc;
}

auto fiska::x86::fe::Parser::parse_x86_instr_expr() -> X86InstrExpr* {
    expect_all(TK::Mnemonic, TK::LParen);

    auto x86_instr = new (ctx_, curr_section_) X86InstrExpr;
    x86_instr->mmic_ = utils::strmap_get(Lexer::mnemonics, peek_tok_str(-2));

    while (not at(TK::RParen)) {
        x86_instr->ops_.push_back(parse_expr());
        assert(at(TK::RParen) or consume(TK::Comma));
    }
    expect_all(TK::RParen, TK::SemiColon);

    return x86_instr;
}

auto fiska::x86::fe::Parser::parse_expr(i8 prec) -> Expr* {
    Expr* lhs = nullptr;
    
    switch (peek_tok_kind()) {
    case TK::At:
    case TK::BitWidth: {
        // Register.
        if (match(TK::BitWidth, TK::Reg)) {
            expect_all(TK::BitWidth, TK::Reg);

            auto reg = new (ctx_, curr_section_) RegLitExpr;
            reg->bw_ = utils::strmap_get(Lexer::bws, peek_tok_str(-2));
            reg->id_ = utils::strmap_get(Lexer::rids, peek_tok_str(-1));

            lhs = reg;
            break;
        }

        // Memory reference.
        if (match(TK::At, TK::BitWidth, TK::LBracket)) {
            expect_all(TK::At, TK::BitWidth);
            
            auto mem = new (ctx_, curr_section_) MemRefLitExpr;
            mem->bw_ = utils::strmap_get(Lexer::bws, peek_tok_str(-1));

            // Base register. The brackets enclosing the base register should always
            // be present to distinguish the expression from an absolute memory offset.
            expect(TK::LBracket);
            if (consume(TK::Reg)) {
                mem->brid_ = utils::strmap_get(Lexer::rids, peek_tok_str(-1));
            }
            expect(TK::RBracket);

            // Scale and index register.
            if (at(TK::LBracket)) {
                // Scale.
                expect_all(TK::LBracket, TK::Num);
                i64 scale = parse_i64(peek_tok_str(-1));
                assert((isa<1, 2, 4, 8>(scale)), "Illegal index scale.");
                mem->scale_ = i8(scale);

                // Index register.
                expect_all(TK::LBracket, TK::Reg);
                mem->irid_ = utils::strmap_get(Lexer::rids, peek_tok_str(-1));
                expect(TK::RBracket);
            }

            lhs = mem;
            break;
        }
        
        // Memory offset.
        if (match(TK::At, TK::BitWidth)) {
            expect_all(TK::At, TK::BitWidth);

            auto moffs = new (ctx_, curr_section_) MoffsLitExpr;
            moffs->bw_ = utils::strmap_get(Lexer::bws, peek_tok_str(-1));
            moffs->addr_ = parse_expr();
            
            assert((isa<Expr::Kind::Label, Expr::Kind::IntLit>(moffs->addr_->kind_)), "Invalid Memory offset address");

            lhs = moffs;
            break;
        }

        // Immediate
        if (match(TK::BitWidth)) {
            expect(TK::BitWidth);
            
            auto imm = new (ctx_, curr_section_) ImmLitExpr;
            imm->bw_ = utils::strmap_get(Lexer::bws, peek_tok_str(-1));
            imm->value_ = parse_expr();
            assert((isa<Expr::Kind::Label, Expr::Kind::IntLit>(imm->value_->kind_)), "Invalid Immediate value.");

            lhs = imm;
            break;
        }
        unreachable("Unkown start of an expression.");
    }
    case TK::Ident: {
        consume(TK::Ident);
        lhs = new (ctx_, curr_section_) LabelExpr(peek_tok_str(-1));
        break;
    }
    case TK::Num: {
        consume(TK::Num);
        lhs = new (ctx_, curr_section_) IntLitExpr(Parser::parse_i64(peek_tok_str(-1))); 
        break;
    }
    case TK::Plus:
    case TK::Minus: {
        consume(TK::Plus, TK::Minus); 

        i8 infix_prec = Parser::prefix_prec_of_tok_kind(peek_tok_kind());
        Expr* inner = parse_expr(infix_prec);
        assert(inner->kind_ == Expr::Kind::IntLit
                and (isa<TK::Plus, TK::Minus>(peek_tok_kind())) , "Invalid unary operation.");

        i64 result = (peek_tok_kind() == TK::Plus ? 1 : -1) * static_cast<IntLitExpr*>(inner)->value_;
        lhs = new (ctx_, curr_section_) IntLitExpr(result);
        break;
    }
    default: unreachable("Unkown start of an expression.");
    } // switch

    while (Parser::infix_prec_of_tok_kind(peek_tok_kind()) > prec) {
        TK binop = peek_tok_kind();
        i8 infix_prec = Parser::infix_prec_of_tok_kind(peek_tok_kind());
        consume(binop);

        Expr* binop_lhs = parse_expr(infix_prec);
        Expr* binop_rhs = parse_expr(infix_prec);

        if (binop_lhs->kind_ == Expr::Kind::IntLit and binop_rhs->kind_ == Expr::Kind::IntLit) {
            i64 lhs_val = static_cast<IntLitExpr*>(binop_lhs)->value_;
            i64 rhs_val = static_cast<IntLitExpr*>(binop_rhs)->value_;

            i64 result = [&] {
                switch (binop) {
                case TK::Plus:
                    // TODO: Make sure this is the right way to check for i64 overflow.
                    assert(lhs_val + rhs_val >= std::max(lhs_val, rhs_val), "Integer overflow.");
                    return lhs_val + rhs_val;
                case TK::Minus:
                    return lhs_val - rhs_val;

                default: unreachable("Invalid binop expression.");
                } // switch
            }();

            lhs = new (ctx_, curr_section_) IntLitExpr(result);
            break;
        }

        assert((binop_lhs->kind_ == Expr::Kind::MemRefLit 
                and isa<Expr::Kind::ImmLit, Expr::Kind::Label>(binop_rhs->kind_)),
                "Invalid binary expression.");

        lhs = new (ctx_, curr_section_) BinaryOpExpr(binop, binop_lhs, binop_rhs);
    }

    return lhs;
}

auto fiska::x86::fe::Parser::prefix_prec_of_tok_kind(TK tk) -> i8 {
    switch (tk) {
    case TK::Plus:
    case TK::Minus:
        return 1;
    default: return 0;
    } // switch
}

auto fiska::x86::fe::Parser::infix_prec_of_tok_kind(TK tk) -> i8 {
    switch (tk) {
    case TK::Plus:
    case TK::Minus:
        return 1;
    default: return 0;
    } // switch
}

auto fiska::x86::fe::Parser::parse_i64(StrRef num_lxm) -> i64 {
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

auto fiska::x86::fe::Parser::parse_file() -> Expr::List {
    Expr::List ast;
    while (not at(TK::Eof)) {
        // TODO: Maybe in the future parse section attributes here first.
        // for instance, you can have an attribute specifying the permissions on a given section.
        expect_all(TK::Section, TK::Ident, TK::LBrace);
        curr_section_ = peek_tok_str(-2);
        while (not at(TK::RBrace)) {
            // TODO: This should be parse declaration.
            ast.push_back(parse_proc());
        }
        expect(TK::RBrace);
    }
    return ast;
}

auto fiska::x86::fe::Parser::parse_top_level_expr() -> Expr* {
    switch (peek_tok_kind()) {
    default: unreachable("Invalid top level expr.");
    case TK::Fn: return parse_proc();
    case TK::Let: return parse_var_expr();
    } // switch
}
