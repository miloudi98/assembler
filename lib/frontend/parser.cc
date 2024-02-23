#include "lib/frontend/parser.hh"
#include "lib/common/base.hh"
#include "lib/common/support.hh"

#include "lib/frontend/ast.hh"
#include "lib/frontend/lexer.hh"
#include "lib/frontend/ctx.hh"
#include "lib/frontend/token.hh"

namespace fiska::assembler::frontend {
namespace {

constexpr auto prefix_op_weight(TK tk) -> i8 {
    switch (tk) {
    case TK::Plus:
    case TK::Minus:
        return 1;
    default: return 0;
    } // switch
}

constexpr auto infix_op_weight(TK tk) -> i8 {
    switch (tk) {
    case TK::Plus:
    case TK::Minus:
        return 1;
    default: return 0;
    } // switch
}

auto parse_i64(StrRef num_lxm) -> Opt<u64> {
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

struct Parser {
    Ctx* ctx_{};
    u16 fid_{};
    i32 tok_idx_{};
    TokStream tok_stream_;

    explicit Parser(Ctx* ctx, u16 fid) :
        ctx_(ctx), fid_(fid), tok_stream_(lex(ctx, fid))
    {}

    template <typename ErrorKind, typename... Args>
    [[noreturn]] auto Error(Span span, Args&&... args) -> void {
        Diag<ErrorKind>(ctx_, span, FWD(args)...);
    }
    
    template <typename ErrorKind, typename... Args>
    [[noreturn]] auto Error(Args&&... args) -> void {
        Diag<ErrorKind>(ctx_, tok().span_, FWD(args)...);
    }

    auto tok() const -> const Tok& { return tok_stream_[tok_idx_]; }

    auto last_tok() const -> const Tok& { return tok_stream_[tok_idx_ - 1]; }

    auto peek(i32 idx = 0) const -> const Tok& {
        return tok_stream_[tok_idx_ + idx];
    }

    auto at(std::same_as<TK> auto... tk) -> i1 {
        return ((tok().kind_ == tk) or ...);
    }

    auto advance(std::same_as<TK> auto... tk) -> void {
        if constexpr (sizeof...(tk) == 0) {
            tok_idx_++;

        } else {
            auto check = [&](TK tk) {
                if (not consume(tk)) Error<UnexpectedToken>(tk, tok().kind_);
            };
            (check(tk), ...);
        }
    }

    auto next(std::same_as<TK> auto... tk) -> const Tok& {
        const Tok& cur = tok();
        advance(tk...);
        return cur;
    }

    auto consume(std::same_as<TK> auto... tk) -> i1 {
        if (at(tk...)) {
            advance();
            return true;
        }
        return false;
    }

    auto match(std::same_as<TK> auto... tk) -> i1 {
        i32 idx = 0;
        return ((peek(idx++).kind_ == tk) and ...);
    }

    auto parse_expression(i8 prec = 0) -> Expr* {
        Expr* lhs = nullptr;
        auto start_span = tok().span_;

        switch (peek().kind_) {
        default: Error<InvalidStartOfExpr>(tok().kind_);

        case TK::Ident: {
            Tok ident = next();
            lhs = new (ctx_) LabelExpr(ident.str_, ident.span_);
            break;
        }
        case TK::Num: {
            Opt<u64> num = parse_i64(tok().str_);
            if (not num) Error<InvalidNumber>(tok().str_);
            lhs = new (ctx_) IntExpr(*num, next().span_);
            break;
        }
        case TK::StrLit: {
            Tok str_lit = next();
            // Trim the enclosing '"'.
            str_lit.str_.remove_prefix(1);
            str_lit.str_.remove_suffix(1);
            lhs = new (ctx_) StrExpr(str_lit.str_, str_lit.span_);
            break;
        }
        case TK::Plus:
        case TK::Minus: {
            Tok op = next();
            i8 prefix_prec = prefix_op_weight(op.kind_);
            Expr* inner = parse_expression(prefix_prec);
            lhs = new (ctx_) UnaryOpExpr(op, inner, {start_span, inner->span_});
            break;
        }
        case TK::LBracket: {
            Vec<Expr*> values; 
            advance(TK::LBracket);
            while (not at(TK::RBracket)) {
                values.push_back(parse_expression());
                // Expect a ',' separating the array elements.
                if (not consume(TK::Comma) and not at(TK::RBracket)) { Error<ExpectedComma>(); }
            }
            advance(TK::RBracket);
            lhs = new (ctx_) ArrayExpr({start_span, last_tok().span_});
            break;
        }
        } // switch

        while (infix_op_weight(peek().kind_) > prec) {
            Tok op = next();
            i8 infix_prec = infix_op_weight(op.kind_);

            Expr* rhs = parse_expression(infix_prec);
            lhs = new (ctx_) BinaryOpExpr(op, lhs, rhs, {lhs->span_, rhs->span_});
        }
        return lhs;
    }

    auto parse_x86_operand() -> X86Op* {
        // Mem.
        if (match(TK::At, TK::BitWidth, TK::LBracket)) {
            auto mem = new (ctx_) MemOp;
            auto start_span = next(TK::At).span_;
            // Consume the bit width.
            mem->bw_ = X86Info::bit_width(next(TK::BitWidth).str_);
            // Consume the base register if present.
            advance(TK::LBracket);
            if (at(TK::Reg)) {
                mem->bri_ = X86Info::register_id(next(TK::Reg).str_);
            }
            advance(TK::RBracket);

            // Scale and index register.
            if (consume(TK::LBracket)) {
                // Scale.
                mem->scale_ = parse_expression();
                advance(TK::RBracket);
                // Index register.
                advance(TK::LBracket);
                mem->iri_ = X86Info::register_id(next(TK::Reg).str_);
                advance(TK::RBracket);
            }

            // Displacement.
            if (at(TK::Plus, TK::Minus)) {
                mem->disp_ = parse_expression();
            }

            // Fix the span.
            mem->span_ = {start_span, last_tok().span_};
            return mem;
        }

        // Mem offset.
        if (match(TK::At, TK::BitWidth)) {
            auto moffs = new (ctx_) MoffsOp;
            auto start_span = next(TK::At).span_;

            moffs->bw_ = X86Info::bit_width(next(TK::BitWidth).str_);
            moffs->addr_ = parse_expression();
            // Fix the span.
            moffs->span_ = {start_span, last_tok().span_};
            return moffs;
        }

        // Register.
        if (match(TK::BitWidth, TK::Reg)) {
            auto reg = new (ctx_) RegisterOp;
            auto start_span = tok().span_;

            reg->bw_ = X86Info::bit_width(next(TK::BitWidth).str_);
            reg->ri_ = X86Info::register_id(next(TK::Reg).str_);
            reg->span_ = {start_span, last_tok().span_};
            return reg;
        }

        // Immediate.
        if (match(TK::BitWidth)) {
            auto imm = new (ctx_) ImmOp;
            auto start_span = tok().span_;

            imm->bw_ = X86Info::bit_width(next(TK::BitWidth).str_);
            imm->value_ = parse_expression();
            imm->span_ = {start_span, last_tok().span_};
            return imm;
        }

        Error<InvalidX86Op>(tok().kind_);
    }

    auto parse_proc() -> Proc* {
        auto start_span = next(TK::Fn).span_;
        auto proc = new (ctx_) Proc(next(TK::Ident).str_, {start_span, last_tok().span_});
        advance(TK::LBrace);
        while (not at(TK::RBrace)) {
            proc->body_.push_back(parse_x86_instruction());
        }
        advance(TK::RBrace);
        return proc;
    }

    auto parse_var() -> Var* {
        advance(TK::Let);
        Tok label = next(TK::Ident, TK::Eq);
        auto var = new (ctx_) Var(label.span_, label.str_, parse_expression());
        advance(TK::SemiColon);
        return var;
    }

    auto parse_section() -> Section {
        advance(TK::Section);
        Section s(next(TK::Ident).str_);
        advance(TK::LBrace);
        while (not at(TK::RBrace)) {
            s.symbols_.push_back(parse_symbol());
        }
        advance(TK::RBrace);
        return s;
    }

    auto parse_x86_instruction() -> X86Inst* {
        auto inst = new (ctx_) X86Inst;
        auto start_span = tok().span_;

        if (X86Info::kMnemonics.find(tok().str_) == X86Info::kMnemonics.end()) {
            Error<InvalidX86Mnemonic>(tok().str_);
        }

        inst->mnemonic_ = X86Info::mnemonic(next(TK::Ident).str_);
        advance(TK::LParen);
        while (not at(TK::RParen)) {
            inst->ops_.push_back(parse_x86_operand());
            // Expect ',' between the x86 operands.
            if (not consume(TK::Comma) and not at(TK::RParen)) { Error<ExpectedComma>(); }
        }
        advance(TK::RParen, TK::SemiColon);
        inst->span_ = {start_span, last_tok().span_};
        return inst;
    }

    auto parse_symbol() -> Symbol* {
        if (at(TK::Fn)) { return parse_proc(); }
        if (at(TK::Let)) { return parse_var(); }
        Error<IllegalSymbolStart>(tok().kind_);
    }
};

} // namespace 

auto parse(Ctx* ctx, u16 fid) -> Vec<Section> {
    Parser p(ctx, fid);
    Vec<Section> ret;
    while (not p.at(TK::Eof)) {
        ret.push_back(p.parse_section());
    }
    return ret;
}

} // namespace fiska::assembler::frontend

