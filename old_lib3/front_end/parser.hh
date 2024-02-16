#ifndef __X86_ASSEMBLER_LIB_FRONT_END_PARSER_HH__
#define __X86_ASSEMBLER_LIB_FRONT_END_PARSER_HH__

#include "lib/support/core.hh"
#include "lib/x86/common.hh"
#include "lib/front_end/lexer.hh"
#include "lib/front_end/ctx.hh"
#include "lib/codegen/patterns.hh"
#include "lib/codegen/assembler.hh"

namespace fiska::x86::fe {

enum struct ParseError {
    InvalidSyntax,
};

//template <typename Result>
//struct ParseResult {
//    // TODO: fix this mess.
//    using ValueType = std::conditional_t<std::is_void_v<Result>, std::monostate, Result>;
//    std::variant<ParseError, Result> result_;
//
//    /*implicit*/ParseResult(ParseError err) : 
//        result_(err) {}
//
//    /*implicit*/ParseResult(Result r) :
//        result_(std::move(r)) {}
//
//    auto ok() const -> i1 { return std::holds_alternative<Result>(result_); }
//
//    auto value() const -> Result { assert(ok()); return std::get<Result>(result_); }
//    // auto unwrap() const -> Result {}
//};

struct Parser {
    Ctx* ctx_{};
    TokStreamView tsv_;
    StrRef section_;

    explicit Parser(Ctx* ctx, const TokStreamView& tsv) :
        ctx_(ctx), tsv_(tsv) {}

    auto parse_section() -> Expr::List;
    auto parse_proc() -> ProcExpr*;
    auto parse_expr(i8 prec = 0) -> Expr*;
    auto parse_x86_instruction() -> X86InstrExpr*;
    auto ingest(std::same_as<TK> auto... tk) -> void;
};

//using namespace codegen;
//
//template <typename... Fragment>
//struct X86Parser;
//
////=====================================================
//// Parser for register classes.
////=====================================================
//template <auto... args>
//struct X86Parser<r<args...>>  {
//    static auto parse(Parser* p) -> ParseResult<Expr*> {
//        if (not p->tsv_.match(TK::BitWidth, TK::Reg)) {
//            return ParseError::InvalidSyntax;
//        }
//        p->ingest(TK::BitWidth, TK::Reg);
//
//        auto reg = new (p->ctx_) RegLitExpr(
//            X86Info::bit_width(p->tsv_.peek(-2).str_),
//            X86Info::reg_id(p->tsv_.peek(-1).str_)
//        );
//
//        return reg;
//    }
//};
//
////=====================================================
//// Parser for memory reference classes.
////=====================================================
//template <auto... args>
//struct X86Parser<m<args...>> {
//    // Pass Parser* as the argument. X86Parser<Mov>::parse(this);
//    static auto parse(Parser* p) -> ParseResult<Expr*> {
//        if (not p->tsv_.match(TK::At, TK::BitWidth, TK::LBracket)) {
//            return ParseError::InvalidSyntax;
//        }
//        p->ingest(TK::At, TK::BitWidth);
//
//        auto mem = new (p->ctx_) MemRefLitExpr;
//        mem->bw_ = X86Info::bit_width(p->tsv_.peek(-1).str_);
//
//        p->ingest(TK::LBracket);
//        if (p->tsv_.consume(TK::Reg)) {
//            mem->brid_ = X86Info::reg_id(p->tsv_.peek(-1).str_);
//        }
//        p->ingest(TK::RBracket);
//
//        // Scale and index register.
//        if (p->tsv_.consume(TK::LBracket)) {
//            // Scale.
//            mem->scale_ = p->parse_expr();
//            p->ingest(TK::RBracket);
//
//            // Index register.
//            if (p->tsv_.consume(TK::LBracket)) {
//                p->ingest(TK::Reg);
//                mem->irid_ = X86Info::reg_id(p->tsv_.peek(-1).str_);
//            }
//            p->ingest(TK::RBracket);
//        }
//
//        // Displacement
//        if (p->tsv_.consume(TK::Plus, TK::Minus)) {
//            TK binop = p->tsv_.peek(-1).kind_;
//            mem->disp_ = new (p->ctx_) BinaryOpExpr(binop, new (p->ctx_) IntLitExpr(0), p->parse_expr());
//        }
//
//        return mem;
//    }
//};
//
////=====================================================
//// Parser for immediate classes.
////=====================================================
//template <auto... args>
//struct X86Parser<i<args...>> {
//    static auto parse(Parser* p) -> ParseResult<Expr*> {
//        if (not p->tsv_.match(TK::BitWidth)) {
//            return ParseError::InvalidSyntax;
//        }
//        p->ingest(TK::BitWidth);
//
//        auto imm =  new (p->ctx_) ImmLitExpr(
//            X86Info::bit_width(p->tsv_.peek(-1).str_),
//            p->parse_expr()
//        );
//
//        return imm;
//    }
//};
//
////=====================================================
//// Parser for memory offset classes.
////=====================================================
//template <auto... args>
//struct X86Parser<mo<args...>> {
//    static auto parse(Parser* p) -> ParseResult<Expr*> {
//        if (not p->tsv_.match(TK::At, TK::BitWidth)) {
//            return ParseError::InvalidSyntax;
//        }
//        p->ingest(TK::At, TK::BitWidth);
//
//        auto moffs =  new (p->ctx_) MoffsLitExpr(
//            X86Info::bit_width(p->tsv_.peek(-1).str_),
//            p->parse_expr()
//        );
//        return moffs;
//    }
//};
//
////=====================================================
//// Parser for Alternative op classes.
////=====================================================
//template <IsIRX86OpClass... IROpClass>
//struct X86Parser<Alt<IROpClass...>> {
//    static auto parse(Parser* p) -> ParseResult<Expr*> {
//        Expr* ret = nullptr;
//        auto parse_op_class = [&]<typename OpClass>() {
//            p->tsv_.snapshot();
//            ParseResult<Expr*> op = X86Parser<OpClass>::parse(p);
//
//            if (op.ok()) {
//                ret = op.value();
//            } else {
//                p->tsv_.rollback();
//            }
//
//            return op.ok();
//        };
//
//        // Parse succeeded.
//        if ((parse_op_class.template operator()<IROpClass>() or ...)) {
//            return ret;
//        }
//
//        return ParseError::InvalidSyntax;
//    }
//};
//
////=====================================================
//// Parser for patterns.
////=====================================================
//template <OpSz opsz, IsIRX86OpClass... IROpClass>
//struct X86Parser<Pat<opsz, IROpClass...>> {
//    static auto parse(Parser* p) -> ParseResult<Expr::List> {
//        Expr::List out;
//        p->tsv_.snapshot();
//
//        auto parse_op_class = [&]<typename OpClass>() {
//            ParseResult<Expr*> op = X86Parser<OpClass>::parse(p);
//            if (op.ok()) {
//                out.push_back(op.value());
//            } else {
//                p->tsv_.rollback();
//            }
//
//            return op.ok();
//        };
//
//        if ((parse_op_class.template operator()<IROpClass>() and ...)) {
//            return out;
//        }
//
//        return ParseError::InvalidSyntax;
//    }
//};
//
//template <IsX86PatternClass... Pattern>
//struct X86Parser<Or<Pattern...>> {
//    static auto parse(Parser* p) -> ParseResult<Expr::List> {
//        Expr::List out;
//
//        auto parse_pat = [&]<typename Pat>() {
//            p->tsv_.snapshot();
//            ParseResult<Expr::List> pat = X86Parser<Pat>::parse(p);
//
//            if (pat.ok()) {
//                out = pat.value();
//            } else {
//                p->tsv_.rollback();
//            }
//
//            return pat.ok();
//        };
//
//        if ((parse_pat.template operator()<Pattern>() or ...)) {
//            return out;
//        }
//        return ParseError::InvalidSyntax;
//    }
//};
//
////=====================================================
//// Parser for Entire expressions.
////=====================================================
//template <X86Mnemonic mnemonic, typename... InstrExpr>
//struct X86Parser<InstrExprList<mnemonic, InstrExpr...>> {
//    static auto parse(Parser* p) -> ParseResult<X86InstrExpr*> {
//        auto x86_instr = new (p->ctx_) X86InstrExpr;
//
//        p->ingest(TK::Mnemonic, TK::LParen);
//        x86_instr->mmic_ = X86Info::mnemonic(p->tsv_.peek(-2).str_);
//
//        if (x86_instr->mmic_ != mnemonic) {
//            return ParseError::InvalidSyntax;
//        }
//
//        auto try_parse_pattern = [&]<typename IExpr>() {
//            ParseResult<Expr::List> ops = X86Parser<typename IExpr::pattern>::parse(p);
//
//            if (ops.ok()) { x86_instr->ops_ = ops.value(); }
//
//            return ops.ok();
//        };
//
//        i1 success = (try_parse_pattern.template operator()<InstrExpr>() or ...);
//
//        p->ingest(TK::RParen, TK::SemiColon);
//
//        if (success) {
//            return x86_instr;
//        }
//        return ParseError::InvalidSyntax;
//    }
//};

} // namespace fiska::x86::fe

#endif // __X86_ASSEMBLER_LIB_FRONT_END_PARSER_HH__
