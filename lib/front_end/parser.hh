#ifndef __X86_ASSEMBLER_LIB_FRONT_END_PARSER_HH__
#define __X86_ASSEMBLER_LIB_FRONT_END_PARSER_HH__

#include "lib/support/core.hh"
#include "lib/x86/common.hh"
#include "lib/front_end/lexer.hh"
#include "lib/front_end/ctx.hh"
#include "lib/codegen/patterns.hh"

namespace fiska::x86::fe {

template <typename Result>
struct ParseResult {
    using ValueType = std::conditional_t<std::is_void_v<Result>, std::monostate, Result>;
    Pair<ValueType, TokStreamView> result_;

    /*implicit*/ParseResult(const TokStreamView& tsv) : 
        result_({std::monostate{}, tsv}) {}

    /*implicit*/ParseResult(Result r, const TokStreamView tsv) :
        result_({std::move(r), tsv}) {}

    auto ok() const -> i1 { return std::holds_alternative<Result>(result_.first); }

    auto value() const -> Result { assert(ok()); return result_.first; }

    auto tsv() const -> TokStreamView { return result_.second; }

    auto operator or(const ParseResult<Result>& other) -> ParseResult<Result> {
        if (ok()) { return *this; }
        return other;
    }

    template <typename Callable>
    auto operator >>=(Callable&& c) -> ParseResult<Result> {
        if (not ok()) { *this; }
        return c(tsv());
    }
};

struct Parser {
    static auto parse_expr(Ctx* ctx, TokStreamView* tsv, i8 prec = 0) -> Expr*;
    static auto prefix_prec_of_tok_kind(TK tk) -> i8;
    static auto infix_prec_of_tok_kind(TK tk) -> i8;
    static auto parse_i64(StrRef num) -> i64;
};

using namespace codegen;

template <IsIRX86OpClass... X86OpClass>
struct X86Parser;

//=====================================================
// Parser for register classes.
//=====================================================
template <auto... args>
struct X86Parser<r<args...>>  {
    static auto parse(Ctx* ctx, TokStreamView tsv) -> ParseResult<Expr*> {
        if (not tsv.match(TK::BitWidth, TK::Reg)) {
            return tsv;
        }
        tsv.ingest(ctx, TK::BitWidth, TK::Reg);

        auto reg = new (ctx) RegLitExpr(
            X86Info::bit_width(tsv.peek(-2).str_),
            X86Info::reg_id(tsv.peek(-1).str_));

        return { reg, tsv };
    }
};

//=====================================================
// Parser for memory reference classes.
//=====================================================
template <auto... args>
struct X86Parser<m<args...>> {
    static auto parse(Ctx* ctx, TokStreamView tsv) -> ParseResult<Expr*> {
        if (not tsv.match(TK::At, TK::BitWidth, TK::LBracket)) {
            return tsv;
        }
        tsv.ingest(ctx, TK::At, TK::BitWidth);

        auto mem = new (ctx) MemRefLitExpr;
        mem->bw_ = X86Info::bit_width(tsv.peek(-2).str_);

        tsv.ingest(ctx, TK::LBracket);
        if (tsv.consume(TK::Reg)) {
            mem->brid_ = X86Info::reg_id(tsv.peek(-1).str_);
        }
        tsv.ingest(ctx, TK::RBracket);

        // Scale and index register.
        if (tsv.consume(TK::LBracket)) {
            // Scale.
            mem->scale_ = Parser::parse_expr(ctx, &tsv);
            tsv.ingest(ctx, TK::RBracket);

            // Index register.
            if (tsv.consume(TK::LBracket)) {
                tsv.ingest(ctx, TK::Reg);
                mem->irid_ = X86Info::reg_id(tsv.peek(-1).str_);
            }
            tsv.ingest(ctx, TK::RBracket);
        }

        // Displacement
        if (tsv.consume(TK::Plus, TK::Minus)) {
            TK binop = tsv.peek().kind_;
            mem->disp_ = new (ctx) BinaryOpExpr(binop, new (ctx) IntLitExpr(0), Parser::parse_expr(ctx, &tsv));
        }

        return { mem, tsv };
    }
};

//=====================================================
// Parser for immediate classes.
//=====================================================
template <auto... args>
struct X86Parser<i<args...>> {
    static auto parse(Ctx* ctx, TokStreamView tsv) -> ParseResult<Expr*> {
        if (not tsv.match(TK::BitWidth)) {
            return tsv;
        }
        tsv.ingest(ctx, TK::BitWidth);

        auto imm =  new (ctx) ImmLitExpr(
            X86Info::bit_width(tsv.peek(-1).str_),
            Parser::parse_expr(ctx, &tsv));

        return { imm, tsv };
    }
};

//=====================================================
// Parser for memory offset classes.
//=====================================================
template <auto... args>
struct X86Parser<mo<args...>> {
    static auto parse(Ctx* ctx, TokStreamView tsv) -> ParseResult<Expr*> {
        if (not tsv.match(TK::At, TK::BitWidth)) {
            return tsv;
        }
        tsv.ingest(ctx, TK::At, TK::BitWidth);

        auto moffs =  new (ctx) MoffsLitExpr(
            X86Info::bit_width(tsv.peek(-1).str_),
            Parser::parse_expr(ctx, &tsv));

        return { moffs, tsv };
    }
};
//=====================================================
// Parser for Alternative op classes.
//=====================================================
template <IsIRX86OpClass... IROpClass>
struct X86Parser<Alt<IROpClass...>> {
    static auto parse(Ctx* ctx, TokStreamView tsv) -> ParseResult<Expr*> {
        return (X86Parser<IROpClass>::parse(ctx, tsv) or ...);
    }
};

//=====================================================
// Parser for patterns.
//=====================================================
template <OpSz opsz, IsIRX86OpClass... IROpClass>
struct X86Parser<Pat<opsz, IROpClass...>> {
    static auto parse(Ctx* ctx, TokStreamView tsv) -> ParseResult<Expr::List> {
        Expr::List ops;
        TokStreamView curr_tsv = tsv;

        auto bind_func = [&](TokStreamView tsv) {
            fmt::format("bismillah");
        };

        return ((X86Parser<IROpClass>::parse(ctx, tsv) >>= bind_func) >>= ...);

        auto parse_ir_op_class = [&]<typename IROpC>() {
            ParseResult<Expr*> op = X86Parser<IROpC>::parse(ctx, curr_tsv);
            if (not op.ok()) { return false; }

            ops.push_back(op.value());
            curr_tsv = op.tsv();

            return true;
        };

        if ((parse_ir_op_class.template operator()<IROpClass>() and ...)) {
            return { std::move(ops), curr_tsv };
        }
        return tsv;
    }
};

template <IsX86PatternClass... Pattern>
struct X86Parser<Or<Pattern...>> {
    static auto parse(Ctx* ctx, TokStreamView tsv) -> ParseResult<Expr::List> {
        return (X86Parser<Pattern>::parse(ctx, tsv) or ...);
    }
};

} // namespace fiska::x86::fe

#endif // __X86_ASSEMBLER_LIB_FRONT_END_PARSER_HH__
