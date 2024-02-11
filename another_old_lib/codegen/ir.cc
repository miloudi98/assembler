#include "lib/codegen/ir.hh"
#include "lib/core.hh"
#include "lib/front_end/parser.hh"

auto fiska::x86::ir::IRBuilder::constant_fold_helper(fe::Expr* node) -> fe::Expr* {
    using namespace fiska::x86::fe;

    switch (node->kind_) {
    case ExprKind::Invalid: unreachable("Invalid ExprKind encountered.");

    case ExprKind::Proc: {
        auto proc = static_cast<ProcExpr*>(node);
        Vec<Expr*> result(proc->body_.size());
        std::transform(proc->body_.begin(), proc->body_.end(), result.begin(), 
            [this](Expr* node) { return this->constant_fold_helper(node); });
        proc->body_ = std::move(result);
        return proc;
    }
    case ExprKind::X86Instruction: {
        auto x86_inst = static_cast<X86InstructionExpr*>(node);
        Vec<X86OpExpr*> result(x86_inst->operands_.size());

        std::transform(x86_inst->operands_.begin(),
            x86_inst->operands_.end(), result.begin(), 
            [&](Expr* node) {
                auto folded = constant_fold_helper(node);
                assert(folded->kind_ == ExprKind::X86Op);
                return static_cast<X86OpExpr*>(folded);
            }
        );
        x86_inst->operands_ = std::move(result);
        return x86_inst;
    }
    case ExprKind::X86Op: {
        auto x86_op = static_cast<X86OpExpr*>(node);
        x86_op->op_ = constant_fold_helper(x86_op);
        return x86_op;
    }
    case ExprKind::RegLit:
    case ExprKind::MemRefLit:
    case ExprKind::IntLit:
    case ExprKind::Type:
    case ExprKind::Label: {
        return node;
    }
    case ExprKind::BinaryOp: {
        auto binary_op = static_cast<BinaryOpExpr*>(node);
        binary_op->lhs_ = constant_fold_helper(binary_op->lhs_);
        binary_op->rhs_ = constant_fold_helper(binary_op->rhs_);

        // TODO(miloudi): Assert that a binary operation is always between numbers and nothing else.
        if (binary_op->lhs_->kind_ == ExprKind::IntLit
            and binary_op->rhs_->kind_ == ExprKind::IntLit)
        {
            IntLitExpr* lhs = static_cast<IntLitExpr*>(binary_op->lhs_);
            IntLitExpr* rhs = static_cast<IntLitExpr*>(binary_op->rhs_);

            i64 result = [&] {
                switch (binary_op->op_) {
                case TK::Plus: {
                    // Check for overflow.
                    assert(lhs->value_ + rhs->value_ >= std::max(lhs->value_, rhs->value_), "Overflow detected.");
                    return lhs->value_ + rhs->value_;
                }
                case TK::Minus: {
                    return lhs->value_ - rhs->value_;
                }
                default: unreachable("Illegal binary operator: '{}'.", Lexer::str_of_tk(binary_op->op_));
                } // switch
            }();

            return new (ctx_) IntLitExpr(result);
        }

        // Not a foldable binary expression.
        return binary_op;
    }
    case ExprKind::UnaryOp: {
        auto unary = static_cast<UnaryOpExpr*>(node);
        auto folded_inner = constant_fold_helper(unary->inner_);

        if (folded_inner->kind_ == ExprKind::Label) {
            assert(unary->op_ == TK::Plus);
            return folded_inner;
        }

        IntLitExpr* inner = static_cast<IntLitExpr*>(constant_fold_helper(unary->inner_));
        assert(inner, "Invalid unary expression.");

        switch (unary->op_) {
        case TK::Plus: break;
        case TK::Minus: { 
            inner->value_ *= -1;
            return inner;
        }
        default: unreachable("Invalid unary operator: '{}'.", Lexer::str_of_tk(unary->op_));
        } // switch
    }
    } // switch
    unreachable();
}

auto fiska::x86::ir::IRBuilder::constant_fold() -> void {
    using namespace fiska::x86::fe;

    Expr::List folded_ast(ast_.size());
    std::transform(ast_.begin(),
            ast_.end(),
            folded_ast.begin(),
            [this](Expr* node) { return this->constant_fold_helper(node); });
    ast_ = std::move(folded_ast);
}

auto fiska::x86::ir::IRBuilder::lower_x86_op_expr(fe::X86OpExpr* x86_op_expr) -> X86Op {
    using namespace fe;

    BW bw = x86_op_expr->type_->bw_;
    i1 is_ptr = x86_op_expr->type_->is_ptr_;

    switch (x86_op_expr->op_->kind_) {
    case ExprKind::RegLit: {
        assert(not is_ptr);
        auto reg = static_cast<RegLitExpr*>(x86_op_expr->op_);
        return X86Op { Reg { x86_op_expr->type_->bw_, reg->id_ } };
    }
    case ExprKind::IntLit: {
        auto int_lit = static_cast<IntLitExpr*>(x86_op_expr->op_);
        if (is_ptr) {
            return X86Op { Moffs { bw, int_lit->value_ } };
        }
        return X86Op { Imm { bw, int_lit->value_ } };
    }
    case ExprKind::MemRefLit: {
        auto mem_ref = static_cast<MemRefLitExpr*>(x86_op_expr->op_);

        Mem mem{};

        if (mem_ref->disp_->kind_ == ExprKind::Label) {
            mem.disp_ = 0;
            mem.disp_bw_ = BW::B32;
        } else if (mem_ref->disp_->kind_ == ExprKind::IntLit) {
            mem.disp_ = static_cast<IntLitExpr*>(x86_op_expr->op_)->value_;
            // mem.disp_bw_ is a simple check to see whether it's not more than 32-bits.
        } else {
            unreachable("Invalid displacement");
        }
        break;
    }
    default: unreachable("x86_op_expr can't be lowered");
    } // switch
    unreachable();
}


auto fiska::x86::ir::IRBuilder::lower_ast() -> Vec<IR_Proc> {
    
    for (Expr* node : ast_) {
        switch (node->kind_) {
        case ExprKind::Proc: {
            auto proc = static_cast<ProcExpr*>(node);

            for (X86InstructionExpr* x86_instr_expr : proc->body_) {
                X86Op::List lowered_body(x86_instr_expr->operands_.size());
                std::transform(x86_instr_expr->body_.begin(), x86_instr_expr->body_.end(), lowered_body.begin(),
                        [this](X86OpExpr* expr) { return lower_x86_op_expr(expr); });

                X86Instruction i { x86_instr_expr->kind_, X86OperandsList(std::move(lowered_body)) };
            }
        }
        case ExprKind::Var: {
        }
        }
    }
}
