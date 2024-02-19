#include "lib/backend/ir.hh"
#include "lib/common/x86/types.hh"
#include "lib/common/base.hh"

#include <bit>

namespace fiska::assembler::backend {
namespace {

auto lower_x86_instr_expr(
    Ctx* ctx,
    const frontend::X86InstrExpr& x86_instr_expr,
    frontend::SemaDone sema_done
) -> IRX86Instr {
    using namespace frontend;

    IRX86Instr ir_x86_instr {
        .mnemonic_ = x86_instr_expr.mnemonic_,
        .ops_ = {}
    };

    for (const Box<Expr>& op_expr : x86_instr_expr.ops_) {
        IRX86Op* ir_x86_op = &ir_x86_instr.ops_.emplace_back();

        switch (op_expr->kind_) {
        case Expr::Kind::Register: {
            ir_x86_op->inner_ = IRReg {
                .bw_ = op_expr->as<RegisterExpr>().bw_,
                .ri_ = op_expr->as<RegisterExpr>().ri_
            };
            break;
        }
        case Expr::Kind::Mem: {
            const MemExpr& mem = op_expr->as<MemExpr>();

            IRMem ir_mem {
                .bw_ = mem.bw_,
                .bri_ = mem.bri_,
                .iri_ = mem.iri_
            };

            if (mem.scale_) {
                auto [_, scale] = reduce_value_node(ctx, mem.scale_.get(), sema_done);
                ir_mem.scale_ = IRMem::Scale(std::bit_width(u8(scale)) - 1);
            }
            
            if (mem.disp_) {
                auto [symbol_name, disp] = reduce_value_node(ctx, mem.disp_.get(), sema_done);
                ir_mem.disp_ = IRValue{symbol_name, disp};
            }

            ir_x86_op->inner_ = ir_mem;
            break;
        }
        case Expr::Kind::Imm: {
            auto [symbol_name, addend] = reduce_value_node(ctx, op_expr->as<ImmExpr>().value_.get(), sema_done);
            IRImm ir_imm {
                .bw_ = op_expr->as<ImmExpr>().bw_,
                .value_ = IRValue{symbol_name, addend}
            };
            ir_x86_op->inner_ = ir_imm;
            break;
        }
        case Expr::Kind::Moffs: {
            auto [symbol_name, addend] = reduce_value_node(ctx, op_expr->as<MoffsExpr>().addr_.get(), sema_done);
            IRImm ir_imm {
                .bw_ = op_expr->as<ImmExpr>().bw_,
                .value_ = IRValue{symbol_name, addend}
            };
            ir_x86_op->inner_ = ir_imm;
            break;
        }
        default: unreachable("Invalid X86 operands.");
        } // switch
    }

    return ir_x86_instr;
}

auto as_byte_vec(BW bw, i64 qword) -> ByteVec {
    ByteVec ret(8);

    switch (bw) {
    case BW::Invalid:
        unreachable();

    case BW::B8:
        ret.push_back(u8(qword >> 0) & 0xff);
        break;
    case BW::B16:
        ret.push_back(u8(qword >> 0) & 0xff);
        ret.push_back(u8(qword >> 8) & 0xff);
        break;
    case BW::B24:
        ret.push_back(u8(qword >> 0) & 0xff);
        ret.push_back(u8(qword >> 8) & 0xff);
        ret.push_back(u8(qword >> 16) & 0xff);
        break;
    case BW::B32:
        ret.push_back(u8(qword >> 0) & 0xff);
        ret.push_back(u8(qword >> 8) & 0xff);
        ret.push_back(u8(qword >> 16) & 0xff);
        ret.push_back(u8(qword >> 24) & 0xff);
        break;
    case BW::B64:
        ret.push_back(u8(qword >> 0) & 0xff);
        ret.push_back(u8(qword >> 8) & 0xff);
        ret.push_back(u8(qword >> 16) & 0xff);
        ret.push_back(u8(qword >> 24) & 0xff);
        ret.push_back(u8(qword >> 32) & 0xff);
        ret.push_back(u8(qword >> 40) & 0xff);
        ret.push_back(u8(qword >> 48) & 0xff);
        ret.push_back(u8(qword >> 56) & 0xff);
        break;
    } // switch
    return ret;
}
} // namespace

auto IRReg::ndx() const -> u8 {
    return X86Info::register_ndx(ri_);
}

auto IRReg::kind() const -> RK {
    return X86Info::register_kind(ri_);
}

auto IRReg::req_ext() const -> i1 {
    return X86Info::register_req_ext(ri_);
}

auto IRMem::disp_bw() const -> BW {
    if (bri_ == RI::Rip 
        or isa<MK::IndexDisp, MK::DispOnly>(kind())
        or disp_.req_reloc())
    { 
        return BW::B32; 
    }

    if (disp_.has_value() or isa<RI::Rbp, RI::R13>(bri_)) {
        return std::bit_width(u32(disp_.addend_)) <= 8 ? BW::B8 : BW::B32;
    }
    return BW::Invalid;
}

auto IRMem::kind() const -> MK {
    if (bri_ == RI::Invalid and iri_ == RI::Invalid) {
        return MK::DispOnly;
    }
    if (bri_ == RI::Invalid) {
        return MK::IndexDisp;
    }
    if (iri_ == RI::Invalid) {
        return MK::BaseDisp;
    }
    return MK::BaseIndexDisp;
}

auto IRMem::mod() const -> u8 {
    if (isa<MK::DispOnly, MK::IndexDisp>(kind())
            or disp_bw() == BW::Invalid
            or bri_ == RI::Rip)
    {
        return X86Info::kModMem;
    }

    if (disp_.req_reloc()) { return X86Info::kModMemDisp32; }

    return std::bit_width(u32(disp_.addend_)) <= 8 ? X86Info::kModMemDisp8 : X86Info::kModMemDisp32;
}

auto IRMem::ndx() const -> u8 {
    return need_sib() ? X86Info::kSibMarker : X86Info::register_ndx(bri_);
}

// Supress warnings from bit-fields narrowings. We are not aware of 
// any way to supress them other than removing the -Wconversion flag
// from the code in question.
GCC_DIAG_IGNORE_PUSH(-Wconversion)
auto IRMem::sib() const -> u8 {
    assert(need_sib());

    Sib sib{};
    switch (kind()) {
    case MK::Invalid: unreachable();

    case MK::BaseDisp: {
        sib.base = X86Info::register_ndx(bri_);
        sib.index = X86Info::kNoIndexRegInSib;
        break;
    }
    case MK::BaseIndexDisp: {
        sib.base = X86Info::register_ndx(bri_);
        sib.index = X86Info::register_ndx(iri_);
        sib.scale = 1 << +scale_;
        break;
    }
    case MK::IndexDisp: {
        sib.base = X86Info::kNoBaseRegInSib;
        sib.index = X86Info::register_ndx(iri_);
        sib.scale = 1 << +scale_;
        break;
    }
    case MK::DispOnly: {
        sib.base = X86Info::kNoBaseRegInSib;
        sib.index = X86Info::kNoIndexRegInSib;
        break;
    }
    } // switch
    return sib.raw();
}
GCC_DIAG_IGNORE_POP();

auto IRMem::need_sib() const -> i1 {
    return kind() != MK::BaseDisp or isa<RI::Rsp, RI::R12>(bri_);
}

auto IRX86Op::req_reloc() const -> i1 {
    if (r()) { return false; }
    if (m()) { return as_m().disp_.req_reloc(); }
    if (i()) { return as_i().value_.req_reloc(); }
    if (mo()) { return as_mo().addr_.req_reloc(); }

    unreachable();
}

auto IRX86Op::reloc_symbol_name() const -> StrRef {
    assert(req_reloc());

    if (m()) { return as_m().disp_.symbol_name_; }
    if (i()) { return as_i().value_.symbol_name_; }
    if (mo()) { return as_mo().addr_.symbol_name_; }

    unreachable();
}

auto lower(Ctx* ctx, const Vec<Box<frontend::Expr>>& ast, frontend::SemaDone sema_done) -> Vec<IRSymbol> {
    using namespace frontend;

    Vec<IRSymbol> symbols;

    for (const auto& node : ast) {
        IRSymbol* sym = &symbols.emplace_back();

        switch (node->kind_) {
        case Expr::Kind::Proc: {
            const ProcExpr& proc_expr = node->as<ProcExpr>();
            sym->name_ = proc_expr.name_;
            sym->section_ = node->section_;

            IRProc proc{};
            for (const Box<Expr>& instr : proc_expr.body_) {
                proc.body_.push_back(lower_x86_instr_expr(ctx, instr->as<X86InstrExpr>(), sema_done));
            }
            sym->inner_ = proc;
            break;
        }
        case Expr::Kind::Var: {
            const VarExpr& var_expr = node->as<VarExpr>();
            sym->name_ = var_expr.name_;
            sym->section_ = node->section_;

            IRVar var{};

            switch (var_expr.value_->kind_) {
            case Expr::Kind::Str: {
                StrRef lit = var_expr.value_->as<StrExpr>().lit_;
                var.data_ = ByteVec{lit.begin(), lit.end()};
                break;
            }
            case Expr::Kind::Int: {
                var.data_ = as_byte_vec(var_expr.type_, var_expr.value_->as<IntExpr>().value_); 
                break;
            }
            case Expr::Kind::Array: {
                const ArrayExpr& arr = var_expr.value_->as<ArrayExpr>();

                for (const Box<Expr>& arr_item : arr.values_) {
                    ByteVec item_value = as_byte_vec(var_expr.type_, arr_item->as<IntExpr>().value_);
                    var.data_.insert(var.data_.end(), item_value.begin(), item_value.end());
                }
                break;
            }
            default: unreachable();
            } // switch
            
            sym->inner_ = var;
            break;
        }
        default: unreachable("Not a top level expression.");
        } // switch
    }

    return symbols;
}

} // namespace fiska::assembler::backend
