#include "lib/frontend/sema.hh"
#include "lib/common/base.hh"
#include "lib/frontend/ast.hh"

namespace fiska::assembler::frontend {
namespace {

struct Sema {
    struct OffsetExpressionChecker {
        struct LabelWithAddend { 
            StrRef label_name_;
            i64 addend_{};
            Span span_;

            auto has_label() const -> i1 { return not label_name_.empty(); }
        };

        Ctx* ctx_{};

        explicit OffsetExpressionChecker(Ctx* ctx) : ctx_(ctx) {}

        template <typename ErrorKind, typename... Args>
        [[noreturn]] auto Error(Span span, Args&&... args) -> void {
            Diag<ErrorKind>(ctx_, span, FWD(args)...);
        }

        auto add(const LabelWithAddend& l, const LabelWithAddend& r) -> LabelWithAddend {
            if (l.has_label() and r.has_label()) { Error<MultipleLabelsInsideExpression>({l.span_, r.span_}); }
            i1 overflow = l.addend_ > 0
                ? r.addend_ > std::numeric_limits<i64>::max() - l.addend_
                : r.addend_ < std::numeric_limits<i64>::min() - l.addend_;
            if (overflow) { Error<IntegerAdditionOverflow>({l.span_, r.span_}); }

            return LabelWithAddend {
                .label_name_ = l.has_label() ? l.label_name_ : r.label_name_,
                .addend_ = l.addend_ + r.addend_,
                .span_ = {l.span_, r.span_}
            };
        }

        auto substract(const LabelWithAddend& l, const LabelWithAddend& r) -> LabelWithAddend {
            if (r.has_label()) {
                if (l.has_label()) { Error<MultipleLabelsInsideExpression>({l.span_, r.span_}); }
                Error<IllegalSubstractionOnLabel>({l.span_, r.span_});
            }
            return LabelWithAddend {
                .label_name_ = l.has_label() ? l.label_name_ : r.label_name_,
                .addend_ = l.addend_ - r.addend_,
                .span_ = {l.span_, r.span_}
            };
        }

        auto check(const Expr* e) -> LabelWithAddend {
            if (auto integer = cast<IntExpr>(e)) { return check(integer); }
            if (auto label = cast<LabelExpr>(e)) { return check(label); }
            if (auto binary = cast<BinaryOpExpr>(e)) { return check(binary); }
            if (auto unary = cast<UnaryOpExpr>(e)) { return check(unary); }
            Error<IllegalOffsetExpression>(e->span_, e->kind_);
        };

        auto check(const BinaryOpExpr* binary) -> LabelWithAddend {
            if (binary->op_.kind_ == TK::Plus) {
                return add(check(binary->lhs_), check(binary->rhs_));
            }
            if (binary->op_.kind_ == TK::Minus) {
                return substract(check(binary->lhs_), check(binary->rhs_));
            }
            Error<IllegalBinaryOperator>(binary->op_.span_, binary->op_.kind_);
        }

        auto check(const UnaryOpExpr* unary) -> LabelWithAddend {
            // Construct a dummy offset result that will act as the 
            // left hand side of a binary expression. Note that this only works because
            // the only unary operators are '+' and '-'.
            LabelWithAddend dummy_lhs = {
                .label_name_ = "",
                .addend_ = 0,
                .span_ = unary->span_
            };

            if (unary->op_.kind_ == TK::Plus) {
                return add(dummy_lhs, check(unary->inner_));
            }
            if (unary->op_.kind_ == TK::Minus) {
                return substract(dummy_lhs, check(unary->inner_));
            }
            Error<IllegalUnaryOperator>(unary->op_.span_, unary->op_.kind_);
        }

        auto check(const IntExpr* integer) -> LabelWithAddend {
            return LabelWithAddend{"", integer->value_, integer->span_};
        }

        auto check(const LabelExpr* label) -> LabelWithAddend {
            return LabelWithAddend{label->name_, 0, label->span_};
        }
    };

    const Vec<Section>& ast_;
    Ctx* ctx_{};

    explicit Sema(Ctx* ctx, const Vec<Section>& ast) : ctx_(ctx), ast_(ast) {}

    template <typename ErrorKind, typename... Args>
    [[noreturn]] auto Error(Span span, Args&&... args) -> void {
        Diag<ErrorKind>(ctx_, span, FWD(args)...);
    }

    auto analyze_mem_scale(const Expr* e) -> void {
        OffsetExpressionChecker oec(ctx_);
        auto label_with_addend = oec.check(e);
        if (label_with_addend.has_label()) { 
            Error<LabelFoundInMemScale>(label_with_addend.span_);
        }
        if (not isa<1, 2, 4, 8>(label_with_addend.addend_)) { 
            Error<IllegalMemScaleValue>(label_with_addend.span_, label_with_addend.addend_);
        }
    }

    auto analyze_mem_disp(const Expr* e) -> void {
        OffsetExpressionChecker oec(ctx_);
        auto label_with_addend = oec.check(e);

        if (label_with_addend.addend_ > std::numeric_limits<i32>::max()) {
            Error<MemDispTooLarge>(label_with_addend.span_, label_with_addend.addend_);
        }
    }

    auto analyze_x86_operand(const X86Op* x86op) -> void {
        if (cast<RegisterOp>(x86op)) {
            // no-op. Registers are checked in the parsing phase.
        } else if (auto mem = cast<MemOp>(x86op)) {
            analyze_mem_scale(mem->scale_);
            analyze_mem_disp(mem->disp_);

        } else if (auto imm = cast<ImmOp>(x86op)) {

        } else if (auto moffs = cast<MoffsOp>(x86op)) {

        } else {
            unreachable("Sema is out of sync with the AST.");
        }
    }
    
    auto analyze_expression(const Expr* e) -> void {
    }
};



} // namespace
} // namespace fiska::assembler::frontend
