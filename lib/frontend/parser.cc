#include "lib/frontend/parser.hh"
#include "lib/common/base.hh"
#include "lib/common/support.hh"

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

    template <typename ErrorKind>
    [[noreturn]] auto Error(Span span, auto&&... args) -> void {
        Diag<ErrorKind>(ctx_, span, FWD(args)...);
    }

    auto tok() const -> const Tok& { return tok_stream_[tok_idx_]; }

    auto peek(i32 idx = 0) const -> const Tok& {
        // Bounds checking happens inside the |TokStream| class.
        return tok_stream_[tok_idx_ + idx];
    }

    auto at(std::same_as<TK> auto... tk) -> i1 {
        return ((tok().kind_ == tk) or ...);
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
            if (not consume(tk)) Error<UnexpectedToken>(tok().span_, tk, tok().kind_);
        };
        (check(tk), ...);
    }

    auto match(std::same_as<TK> auto... tk) -> i1 {
        i32 idx = 0;
        return ((peek(idx++).kind_ == tk) and ...);
    }

    auto parse_expression(i8 prec = 0) -> Expr* {
        Expr* lhs = nullptr;
        auto start_span = tok().span_;

        switch (peek().kind_) {
        default: Error<InvalidStartOfExpr>(tok().span_, tok().kind_);

        case TK::Ident: {
            lhs = new (ctx_) LabelExpr(tok().str_, tok().span_);
            advance();
            break;
        }
        case TK::Num: {
            Opt<u64> num = parse_i64(tok().str_);
            if (not num) Error<InvalidNumber>(tok().span_, tok().str_);
            lhs = new (ctx_) IntExpr(*num, tok().span_);
            advance();
            break;
        }
        case TK::StrLit: {
            StrRef lit = tok().str_;
            // Trim the enclosing '"'.
            lit.remove_prefix(1);
            lit.remove_suffix(1);

            lhs = new (ctx_) StrExpr(lit, tok().span_);
            advance();
            break;
        }
        case TK::Plus:
        case TK::Minus: {
            i8 prefix_prec = prefix_op_weight(tok().kind_);
            // Consume the unary operator.
            advance();
            Expr* inner = parse_expression(prefix_prec);
            lhs = new (ctx_) UnaryOpExpr(tok(), inner, {start_span, inner->span_});
            break;
        }
        case TK::LBracket: {
            Vec<Expr*> values; 

            ingest(TK::LBracket);
            while (not at(TK::RBracket)) {
                values.push_back(parse_expression());
                assert(at(TK::RBracket) or consume(TK::Comma));
            }
            lhs = new (ctx_) ArrayExpr({start_span, tok().span_});
            ingest(TK::RBracket);
            break;
        }
        } // switch

        while (infix_op_weight(peek().kind_) > prec) {
            Tok op = peek();
            i8 infix_prec = infix_op_weight(op.kind_);
            advance();

            Expr* rhs = parse_expression(infix_prec);
            lhs = new (ctx_) BinaryOpExpr(op, lhs, rhs, {lhs->span_, rhs->span_});
        }
        return lhs;
    }

    auto parse_x86_operand() -> X86Op* {
        // Mem.
        if (match(TK::At, TK::BitWidth, TK::LBracket)) {
            auto mem = new (ctx_) MemOp;
            auto start_span = tok().span_;
            // Consume '@'.
            advance();
            mem->bw_ = X86Info::bit_width(tok().str_);
            // Consume the bit width.
            advance();
            // Consume the '['.
            advance();
            if (at(TK::Reg)) {
                mem->bri_ = X86Info::register_id(tok().str_);
                // Consume the base register.
                advance();
            }
            ingest(TK::RBracket);

            // Scale and index register.
            if (consume(TK::LBracket)) {
                // Scale.
                mem->scale_ = parse_expression();
                ingest(TK::RBracket);
                // Index register.
                ingest(TK::LBracket, TK::Reg);
                mem->iri_ = X86Info::register_id(peek(-1).str_);
                ingest(TK::RBracket);
            }

            // Displacement.
            if (at(TK::Plus, TK::Minus)) {
                mem->disp_ = parse_expression();
            }

            // Fix the span.
            mem->span_ = {start_span, peek(-1).span_};
            return mem;
        }

        // Mem offset.
        if (match(TK::At, TK::BitWidth)) {
            auto moffs = new (ctx_) MoffsOp;
            auto start_span = tok().span_;
            // Consume '@'.
            advance();
            moffs->bw_ = X86Info::bit_width(tok().str_);
            // Consume the bitwidth.
            advance();
            moffs->addr_ = parse_expression();
            // Fix the span.
            moffs->span_ = {start_span, peek(-1).span_};
            return moffs;
        }
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

} // namespace 
} // namespace fiska::assembler::frontend

