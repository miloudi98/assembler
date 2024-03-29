#include "lib/frontend/sema.hh"
#include "lib/common/base.hh"
#include "lib/frontend/ast.hh"

namespace fiska::assembler::frontend {
namespace {

struct OffsetExpressionChecker {
    struct LabelWithAddend { 
        StrRef label_name_;
        i64 addend_{};
        Span span_;

        auto has_label() const -> i1 { return not label_name_.empty(); }
    };

    Ctx* ctx_{};

    explicit OffsetExpressionChecker(Ctx* ctx) : ctx_(ctx) {}

    auto add(const LabelWithAddend& l, const LabelWithAddend& r) -> LabelWithAddend {
        if (l.has_label() and r.has_label()) { 
            Diag<MultipleLabelsInsideExpression>(ctx_, {l.span_, r.span_});
        }
        i1 overflow = l.addend_ > 0
            ? r.addend_ > std::numeric_limits<i64>::max() - l.addend_
            : r.addend_ < std::numeric_limits<i64>::min() - l.addend_;
        if (overflow) { Diag<IntegerAdditionOverflow>(ctx_, {l.span_, r.span_}); }

        return LabelWithAddend {
            .label_name_ = l.has_label() ? l.label_name_ : r.label_name_,
            .addend_ = l.addend_ + r.addend_,
            .span_ = {l.span_, r.span_}
        };
    }

    auto substract(const LabelWithAddend& l, const LabelWithAddend& r) -> LabelWithAddend {
        if (r.has_label()) {
            if (l.has_label()) {
                Diag<MultipleLabelsInsideExpression>(ctx_, {l.span_, r.span_});
            }
            Diag<IllegalSubstractionOnLabel>(ctx_, {l.span_, r.span_});
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
        Diag<IllegalOffsetExpression>(ctx_, e->span_, e->kind_);
    };

    auto check(const BinaryOpExpr* binary) -> LabelWithAddend {
        if (binary->op_.kind_ == TK::Plus) {
            return add(check(binary->lhs_), check(binary->rhs_));
        }
        if (binary->op_.kind_ == TK::Minus) {
            return substract(check(binary->lhs_), check(binary->rhs_));
        }
        Diag<IllegalBinaryOperator>(ctx_, binary->op_.span_, binary->op_.kind_);
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
        Diag<IllegalUnaryOperator>(ctx_, unary->op_.span_, unary->op_.kind_);
    }

    auto check(const IntExpr* integer) -> LabelWithAddend {
        return LabelWithAddend{"", integer->value_, integer->span_};
    }

    auto check(const LabelExpr* label) -> LabelWithAddend {
        return LabelWithAddend{label->name_, 0, label->span_};
    }

    static auto check(Ctx* ctx, const Expr* e) -> LabelWithAddend {
        OffsetExpressionChecker oec(ctx);
        return oec.check(e);
    }
};

struct Sema {
    Ctx* ctx_{};
    const Vec<Section>& ast_;

    explicit Sema(Ctx* ctx, const Vec<Section>& ast) : ctx_(ctx), ast_(ast) {}

    auto analyze_mem_scale(const Expr* e) -> void {
        auto label_with_addend = OffsetExpressionChecker::check(ctx_, e);
        if (label_with_addend.has_label()) { 
            Diag<LabelFoundInMemScale>(ctx_, label_with_addend.span_);
        }
        if (not isa<1, 2, 4, 8>(label_with_addend.addend_)) { 
            Diag<IllegalMemScaleValue>(ctx_, label_with_addend.span_, label_with_addend.addend_);
        }
    }

    auto analyze_mem_disp(const Expr* e) -> void {
        auto label_with_addend = OffsetExpressionChecker::check(ctx_, e);
        if (label_with_addend.addend_ > std::numeric_limits<i32>::max()) {
            Diag<MemDispTooLarge>(ctx_, label_with_addend.span_, label_with_addend.addend_);
        }
    }

    auto analyze_imm(const ImmOp* imm) -> void {
        std::ignore = OffsetExpressionChecker::check(ctx_, imm->value_);
    }

    auto analyze_moffs(const MoffsOp* moffs) -> void {
        std::ignore = OffsetExpressionChecker::check(ctx_, moffs->addr);
    }

    auto analyze_x86_operand(const X86Op* x86op) -> void {
        if (cast<RegisterOp>(x86op)) {
            // no-op. Registers are checked in the parsing phase.
        } else if (auto mem = cast<MemOp>(x86op)) {
            analyze_mem_scale(mem->scale_);
            analyze_mem_disp(mem->disp_);

        } else if (auto imm = cast<ImmOp>(x86op)) {
            analyze_imm(imm);

        } else if (auto moffs = cast<MoffsOp>(x86op)) {
            analyze_moffs(moffs);

        } else {
            unreachable("Sema is out of sync with the AST.");
        }
    }
    
    auto analyze_symbol(const Symbol* sym) -> void {
        if (auto proc = cast<Proc>(sym)) {
            for (auto x86inst : proc->body_) {
                rgs::for_each(x86inst->ops_,
                    [this](const X86Op* x86op) { this->analyze_x86_operand(x86op); });
            }

        } else if (cast<Proc>(sym)) {
            todo();

        } else {
            unreachable("Sema is out of symc with the AST.");
        }
    }

    auto analyze_ast() -> void {
        for (const Section& s : ast_) {
            rgs::for_each(s.symbols_, [this](const Symbol* sym) { analyze_symbol(sym); });
        }
    }

    static auto analyze(Ctx* ctx, const Vec<Section>& ast) -> void {
        Sema(ctx, ast).analyze_ast();
    }
};

} // namespace

auto analyze(Ctx* ctx, const Vec<Section>& ast) -> void {
    Sema::analyze(ctx, ast);
}

} // namespace fiska::assembler::frontend
