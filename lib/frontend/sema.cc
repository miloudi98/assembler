#include "lib/frontend/sema.hh"
#include "lib/common/base.hh"

namespace fiska::assembler::frontend {
namespace {

using ReducedValue = Pair<StrRef, i64>;

auto fits_in_bit_width(BW bw, i64 value) -> i1 {
    switch (bw) {
    case BW::Invalid:
    case BW::B24:
        unreachable();

    case BW::B8: return utils::fits_in_b8(value);
    case BW::B16: return utils::fits_in_b16(value);
    case BW::B32: return utils::fits_in_b32(value);
    case BW::B64: return true;
    } // switch
    unreachable();
}

auto label_count(Expr* node) -> i64 {
    assert(node);

    switch (node->kind_) {
    case Expr::Kind::Label: {
        return 1;
    }
    case Expr::Kind::UnaryOp: {
        return label_count(node->as<UnaryOpExpr>().inner_.get());
    }
    case Expr::Kind::BinaryOp: {
        const BinaryOpExpr& binary = node->as<BinaryOpExpr>();
        return label_count(binary.lhs_.get()) + label_count(binary.rhs_.get());
    }
    default: return 0;
    } // switch
}

// Assumes that |label_count(node) == 1|.
auto reduce_expr_to_value(Ctx* ctx, Expr* node) -> ReducedValue {
    assert(node);
    if (label_count(node) > 1) {
        Diagnostic {
            ctx,
            "Too many labels found.",
            node->span_
        };
    }

    switch (node->kind_) {
    case Expr::Kind::Label: {
        return { node->as<LabelExpr>().name_, 0 };
    }
    case Expr::Kind::Int: {
        return { "", node->as<IntExpr>().value_ };
    }
    case Expr::Kind::UnaryOp: {
        const UnaryOpExpr& unary = node->as<UnaryOpExpr>();

        ReducedValue rv = reduce_expr_to_value(ctx, unary.inner_.get());

        if (not rv.first.empty()) {
            Diagnostic {ctx, "Unary operations on labels is not allowed.", unary.inner_->span_};
        }

        if (unary.op_.kind_ == TK::Plus) {
            // no-op.
        } else if (unary.op_.kind_ == TK::Minus) {
            rv.second *= -1;
        } else {
            unreachable();
        }

        return rv;
    }
    case Expr::Kind::BinaryOp: {
        const BinaryOpExpr& binary = node->as<BinaryOpExpr>();

        ReducedValue rv{};
        ReducedValue lhs_rv = reduce_expr_to_value(ctx, binary.lhs_.get());
        ReducedValue rhs_rv = reduce_expr_to_value(ctx, binary.rhs_.get());

        if (binary.op_.kind_ == TK::Plus) {
            auto [ret, overflow] = utils::safe_add(lhs_rv.second, rhs_rv.second);
            if (overflow) {
                Diagnostic {ctx, "64-bit integer overflow.", node->span_};
            }
            rv.second = ret;

        } else if (binary.op_.kind_ == TK::Minus) {
            // A label can't be at the right hand side of a substraction.
            if (not rhs_rv.first.empty()) {
                Diagnostic {ctx, "A label can't appear at the right hand side of a substraction.", node->span_};
            }
            rv.second = lhs_rv.second - rhs_rv.second;
        } else {
            unreachable();
        }

        // |label_count(node) == 1|
        rv.first = lhs_rv.first.empty() ? rhs_rv.first : lhs_rv.first;

        return rv;
    }
    default: {
        Diagnostic {ctx, "Illegal expression encountered.", node->span_};
    }
    } // switch
}

auto analyze_expr(Ctx* ctx, Expr* node) -> void {
    switch (node->kind_) {
    case Expr::Kind::Invalid: { unreachable(); }
    case Expr::Kind::Register: { break; }
    case Expr::Kind::Mem: {
        const MemExpr& mem = node->as<MemExpr>();

        // Make sure we're not indexing with Rsp and Rip.
        if (isa<RI::Rsp, RI::Rip>(mem.iri_)) {
            Diagnostic { ctx, "Illegal indexing register.", node->span_ };
        }

        if (mem.scale_) {
            ReducedValue scale = reduce_expr_to_value(ctx, mem.scale_.get());
            if (not scale.first.empty()) {
                Diagnostic {ctx, "Labels are not allowed in the index scale.", mem.scale_->span_};
            }
            if (not isa<1, 2, 4, 8>(scale.second)) {
                Diagnostic {ctx, fmt::format("Illegal index scale value: '{}'.", scale.second), mem.scale_->span_};
            }
        }

        if (mem.disp_) {
            ReducedValue disp = reduce_expr_to_value(ctx, mem.disp_.get());
            // If there is a label then we will simply emit the relocation and the linker will take care of seeing
            // whether the value to patch in fits in 32-bits or not. However, when there is no relocations involved,
            // we have to check that ourselves.
            if (disp.first.empty() and disp.second >= std::numeric_limits<i32>::max()) {
                Diagnostic {
                    ctx,
                    fmt::format("Displacements larger than 32-bits "
                                "are not supported. Value = '{}'.", disp.second),
                    mem.disp_->span_
                };
            }
        }
        break;
    }
    case Expr::Kind::Imm: {
        const ImmExpr& imm = node->as<ImmExpr>();
        ReducedValue rv = reduce_expr_to_value(ctx, imm.value_.get());

        if (not fits_in_bit_width(imm.bw_, rv.second)) {
            Diagnostic {
                ctx, 
                fmt::format("Value '{}' does not fit in type '{}'.", rv.second, imm.bw_),
                imm.value_->span_ 
            };
        }
        break;
    }
    case Expr::Kind::Moffs: {
        reduce_expr_to_value(ctx, node->as<MoffsExpr>().addr_.get());
        break;
    }
    case Expr::Kind::Int:
    case Expr::Kind::Label:
    case Expr::Kind::Str: 
    case Expr::Kind::Array:
    case Expr::Kind::BinaryOp: 
    case Expr::Kind::UnaryOp: {
        break;
    }
    case Expr::Kind::X86Instr: {
        rgs::for_each(
            node->as<X86InstrExpr>().ops_,
            [&](const Box<Expr>& op) { analyze_expr(ctx, op.get()); }
        );
        break;
    }
    case Expr::Kind::Proc: {
        rgs::for_each(
            node->as<ProcExpr>().body_,
            [&](const Box<Expr>& op) { analyze_expr(ctx, op.get()); }
        );
        break;
    }
    case Expr::Kind::Var: {
        const VarExpr& var = node->as<VarExpr>();
        switch (var.value_->kind_) {
        case Expr::Kind::Str: {
            if (var.type_ != BW::B8) {
                Diagnostic {
                    ctx, 
                    fmt::format("Expected 'b8' as the variable type, found '{}'.", var.type_),
                    var.value_->span_
                };
            }
            break;
        }
        case Expr::Kind::Int: {
            i64 value = var.value_->as<IntExpr>().value_;
            if (not fits_in_bit_width(var.type_, value)) {
                Diagnostic {
                    ctx,
                    fmt::format("Value '{}' does not fit in type '{}'.", value, var.type_),
                    var.value_->span_
                };
            }
            break;
        }
        case Expr::Kind::Array: {
            for (const Box<Expr>& arr_item : var.value_->as<ArrayExpr>().values_) {
                if (not arr_item->is<IntExpr>()) {
                    Diagnostic {
                        ctx,
                        "Illegal value found.",
                        arr_item->span_
                    };
                }

                if (not fits_in_bit_width(var.type_, arr_item->as<IntExpr>().value_)) {
                    Diagnostic {
                        ctx,
                        fmt::format("Value '{}' does not fit in type '{}'.", 
                                    arr_item->as<IntExpr>().value_, var.type_),
                        var.value_->span_
                    };
                }
            }
            break;
        }
        default:
            Diagnostic {
                ctx,
                "Unsupported variable initialization.",
                var.value_->span_
            };
        } // switch
        break;
    }
    } // switch
}

} // namespace
} // namespace fiska::assembler::frontend

auto fiska::assembler::frontend::analyze(Ctx* ctx, const Vec<Box<Expr>>& ast) -> SemaDone {
    rgs::for_each(ast, [&](const Box<Expr>& node) { analyze_expr(ctx, node.get()); });
    return SemaDone();
}
