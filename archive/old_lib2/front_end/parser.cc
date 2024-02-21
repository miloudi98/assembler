#include "lib/front_end/parser.hh"
#include "lib/core.hh"
#include "lib/front_end/context.hh"

#include <cctype>

namespace {

constexpr auto prefix_prec_of_tk(fiska::x86::fe::TK tk) -> i8 {
    using fiska::x86::fe::TK;

    switch (tk) {
    case TK::Plus:
    case TK::Minus:
        return 1;
    default: return 0;
    } // switch
    unreachable();
}

constexpr auto infix_prec_of_tk(fiska::x86::fe::TK tk) -> i8 {
    using fiska::x86::fe::TK;

    switch (tk) {
    case TK::Plus:
    case TK::Minus:
        return 1;
    default: return 0;
    } // switch
    unreachable();
}

} // namespace


auto fiska::x86::fe::Expr::operator new(usz sz, Ctx* ctx) -> void* {
    auto expr = static_cast<Expr*>(::operator new(sz)); 
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
    ++tok_stream_it_;
}

auto fiska::x86::fe::Parser::peek_tok(i32 idx) -> const Tok& {
    auto ptok = tok_stream_it_ + idx;
    // Bounds checking.
    assert(ctx_->tok_streams_[fid_].begin() <= ptok and ptok < ctx_->tok_streams_[fid_].end());
    return *ptok;
}

auto fiska::x86::fe::Parser::parse_type() -> TypeExpr* {
    auto type = new (ctx_) TypeExpr;
    type->is_ptr_ = consume(TK::At);

    if (consume(TK::BitWidth)) {
        type->bw_ = utils::strmap_get(Lexer::bws, peek_tstr(-1));
    }
    return type;
}

auto fiska::x86::fe::Parser::parse_reg_lit() -> RegLitExpr* {
    expect(TK::Reg);
    RI rid = utils::strmap_get(Lexer::rids, peek_tstr(-1));
    return new (ctx_) RegLitExpr(rid);
}

auto fiska::x86::fe::Parser::parse_mem_ref_lit() -> MemRefLitExpr* {
    auto mem_ref = new (ctx_) MemRefLitExpr;

    // Base register.
    expect(TK::LBracket);
    if (consume(TK::Reg)) {
        mem_ref->brid_ = utils::strmap_get(Lexer::rids, peek_tstr(-1));
    }
    expect(TK::RBracket);

    // Scale.
    expect(TK::LBracket);
    if (auto num = parse_expr()) {
        assert(num->kind_ == ExprKind::IntLit);
        mem_ref->scale_ = [&] {
            switch (static_cast<IntLitExpr*>(num)->value_) {
            case 1: return MemIndexScale::One;
            case 2: return MemIndexScale::Two;
            case 4: return MemIndexScale::Four;
            case 8: return MemIndexScale::Eight;
            default: unreachable("Illegal index scale");
            } // switch
        }();
    }
    expect(TK::RBracket);

    // Index register.
    expect(TK::LBracket);
    if (consume(TK::Reg)) {
        mem_ref->irid_ = utils::strmap_get(Lexer::rids, peek_tstr(-1));
    }
    expect(TK::RBracket);

    if (at(TK::Plus, TK::Minus)) {
        mem_ref->disp_ = parse_expr();
        assert(mem_ref->disp_, "Failed to parse disp");
    }

    return mem_ref;
}

auto fiska::x86::fe::Parser::parse_int_lit() -> IntLitExpr* {
    expect(TK::Num);

    StrRef num_lxm = peek_tstr(-1);

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
    return new (ctx_) IntLitExpr(i64(ret));
}

auto fiska::x86::fe::Parser::parse_expr(i8 prec) -> Expr* {
    Expr* lhs = nullptr;

    TK curr_tk = peek_tk();
    switch (curr_tk) {
    case TK::At:
    case TK::BitWidth: {
        // TODO(miloudi): Don't consider a type as an expression.
        // Call parse_x86_op_expr here instead.
        lhs = parse_type();
        break;
    }
    case TK::LBracket: {
        lhs = parse_mem_ref_lit();
        break;
    }
    case TK::Reg: {
        lhs = parse_reg_lit();
        break;
    }
    case TK::Ident: {
        lhs = new (ctx_) LabelExpr(peek_tstr()); break;
    }
    case TK::Num: {
        lhs = parse_int_lit();
        break;
    }
    case TK::Plus:
    case TK::Minus: {
        consume(TK::Plus, TK::Minus);
        i8 prefix_prec = prefix_prec_of_tk(curr_tk);
        Expr* inner = parse_expr(prefix_prec);
        lhs = new (ctx_) UnaryOpExpr(curr_tk, inner);
        break;
    }
    default: unreachable("expected expression");
    } // switch

    TK op = peek_tk();
    i8 infix_prec = infix_prec_of_tk(op);
    if (infix_prec > prec) {
        consume(op);
        Expr* rhs = parse_expr(infix_prec);
        lhs = new (ctx_) BinaryOpExpr(op, lhs, rhs);
    }

    return lhs;
}

auto fiska::x86::fe::Parser::parse_x86_op_expr() -> X86OpExpr* {
    return new (ctx_) X86OpExpr(parse_type(), parse_expr());
}

auto fiska::x86::fe::Parser::parse_x86_instruction() -> X86InstructionExpr* {
    expect_all(TK::Mnemonic, TK::LParen);

    //X86Mnemonic mnic = utils::strmap_get(Lexer::mnemonics, peek_tstr(-2));
    X86IK ik = utils::strmap_get(Lexer::mnemonics, peek_tstr(-2));

    Vec<X86OpExpr*> ops;
    while (not at(TK::RParen)) {
        ops.push_back(parse_x86_op_expr());
        consume(TK::Comma);
    }
    expect(TK::RParen);

    return new (ctx_) X86InstructionExpr(ik, std::move(ops));
}

auto fiska::x86::fe::Parser::parse_section() -> void {
    expect_all(TK::Section, TK::Ident, TK::LBracket);
    curr_section_ = peek_tstr(-2);
}

auto fiska::x86::fe::Parser::parse_proc() -> ProcExpr* {
    todo();
}


