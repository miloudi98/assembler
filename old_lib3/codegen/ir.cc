//#include "lib/codegen/ir.hh"
//#include "lib/support/core.hh"
//
//#include <bit>
//
//auto fiska::x86::codegen::IRReg::ndx(RI id) -> u8 {
//    switch (id) {
//    case RI::Invalid: unreachable();
//
//    case RI::Rax: return 0;
//    case RI::Rcx: return 1;
//    case RI::Rdx: return 2;
//    case RI::Rbx: return 3;
//    case RI::Rsp: return 4;
//    case RI::Rbp: return 5;
//    case RI::Rsi: return 6;
//    case RI::Rdi: return 7;
//    case RI::R8:  return 0;
//    case RI::R9:  return 1;
//    case RI::R10: return 2;
//    case RI::R11: return 3;
//    case RI::R12: return 4;
//    case RI::R13: return 5;
//    case RI::R14: return 6;
//    case RI::R15: return 7;
//    case RI::Rah: return 4;
//    case RI::Rch: return 5;
//    case RI::Rdh: return 6;
//    case RI::Rbh: return 7;
//
//    case RI::Rip: return 5;
//
//    case RI::Es: return 0;
//    case RI::Cs: return 1;
//    case RI::Ss: return 2;
//    case RI::Ds: return 3;
//    case RI::Fs: return 4;
//    case RI::Gs: return 5;
//
//    case RI::Cr0:  return 0;
//    case RI::Cr1:  return 1;
//    case RI::Cr2:  return 2;
//    case RI::Cr3:  return 3;
//    case RI::Cr4:  return 4;
//    case RI::Cr5:  return 5;
//    case RI::Cr6:  return 6;
//    case RI::Cr7:  return 7;
//    case RI::Cr8:  return 0;
//    case RI::Cr9:  return 1;
//    case RI::Cr10: return 2;
//    case RI::Cr11: return 3;
//    case RI::Cr12: return 4;
//    case RI::Cr13: return 5;
//    case RI::Cr14: return 6;
//    case RI::Cr15: return 7;
//
//    case RI::Dbg0:  return 0;
//    case RI::Dbg1:  return 1;
//    case RI::Dbg2:  return 2;
//    case RI::Dbg3:  return 3;
//    case RI::Dbg4:  return 4;
//    case RI::Dbg5:  return 5;
//    case RI::Dbg6:  return 6;
//    case RI::Dbg7:  return 7;
//    case RI::Dbg8:  return 0;
//    case RI::Dbg9:  return 1;
//    case RI::Dbg10: return 2;
//    case RI::Dbg11: return 3;
//    case RI::Dbg12: return 4;
//    case RI::Dbg13: return 5;
//    case RI::Dbg14: return 6;
//    case RI::Dbg15: return 7;
//    } // switch
//
//    unreachable();
//}
//
//auto fiska::x86::codegen::IRReg::kind(RI id) -> RK {
//    switch (id) {
//    case RI::Invalid: unreachable();
//
//    case RI::Rax:
//    case RI::Rcx:
//    case RI::Rdx:
//    case RI::Rbx:
//    case RI::Rsp:
//    case RI::Rbp:
//    case RI::Rsi:
//    case RI::Rdi:
//    case RI::R8:
//    case RI::R9:
//    case RI::R10:
//    case RI::R11:
//    case RI::R12:
//    case RI::R13:
//    case RI::R14:
//    case RI::R15:
//    case RI::Rah:
//    case RI::Rch:
//    case RI::Rdh:
//    case RI::Rbh:
//        return RK::Gp;
//
//    case RI::Rip:
//        return RK::Ip;
//
//    case RI::Es:
//    case RI::Cs:
//    case RI::Ss:
//    case RI::Ds:
//    case RI::Fs:
//    case RI::Gs:
//        return RK::Seg;
//
//    case RI::Cr0:
//    case RI::Cr1:
//    case RI::Cr2:
//    case RI::Cr3:
//    case RI::Cr4:
//    case RI::Cr5:
//    case RI::Cr6:
//    case RI::Cr7:
//    case RI::Cr8:
//    case RI::Cr9:
//    case RI::Cr10:
//    case RI::Cr11:
//    case RI::Cr12:
//    case RI::Cr13:
//    case RI::Cr14:
//    case RI::Cr15:
//        return RK::Ctrl;
//
//    case RI::Dbg0:
//    case RI::Dbg1:
//    case RI::Dbg2:
//    case RI::Dbg3:
//    case RI::Dbg4:
//    case RI::Dbg5:
//    case RI::Dbg6:
//    case RI::Dbg7:
//    case RI::Dbg8:
//    case RI::Dbg9:
//    case RI::Dbg10:
//    case RI::Dbg11:
//    case RI::Dbg12:
//    case RI::Dbg13:
//    case RI::Dbg14:
//    case RI::Dbg15:
//        return RK::Dbg;
//    } // switch
//    unreachable();
//}
//
//auto fiska::x86::codegen::IRReg::need_ext(RI id) -> i1 {
//    return (+id >= +RI::R8 and +id <= +RI::R15)
//        or (+id >= +RI::Cr8 and +id <= +RI::Cr15)
//        or (+id >= +RI::Dbg8 and +id <= +RI::Dbg15);
//}
//
//auto fiska::x86::codegen::IRReg::need_ext() const -> i1 {
//    return IRReg::need_ext(id_);
//}
//
//auto fiska::x86::codegen::IRReg::kind() const -> RK {
//    return IRReg::kind(id_);
//}
//
//auto fiska::x86::codegen::IRReg::ndx() const -> u8 {
//    return IRReg::ndx(id_);
//}
//
//auto fiska::x86::codegen::IRMem::disp_bw() const -> BW {
//    if (brid_ == RI::Rip or isa<MK::IndexDisp, MK::DispOnly>(kind())) { 
//        return BW::B32; 
//    }
//
//    if (disp_ or isa<RI::Rbp, RI::R13>(brid_)) {
//        return std::bit_width(u32(disp_)) <= 8 ? BW::B8 : BW::B32;
//    }
//    return BW::Invalid;
//}
//
//auto fiska::x86::codegen::IRMem::kind() const -> MK {
//    if (brid_ == RI::Invalid and irid_ == RI::Invalid) {
//        return MK::DispOnly;
//    }
//    if (brid_ == RI::Invalid) {
//        return MK::IndexDisp;
//    }
//    if (irid_ == RI::Invalid) {
//        return MK::BaseDisp;
//    }
//    return MK::BaseIndexDisp;
//}
//
//auto fiska::x86::codegen::IRMem::mod() const -> u8 {
//    if (isa<MK::DispOnly, MK::IndexDisp>(kind())
//            or disp_bw() == BW::Invalid
//            or brid_ == RI::Rip)
//    {
//        return IRX86Op::kModMem;
//    }
//
//    return std::bit_width(u32(disp_)) <= 8 ? IRX86Op::kModMemDisp8 : IRX86Op::kModMemDisp32;
//}
//
//auto fiska::x86::codegen::IRMem::ndx() const -> u8 {
//    return need_sib() ? IRX86Op::kSibMarker : IRReg::ndx(brid_);
//}
//
//auto fiska::x86::codegen::IRX86Op::ndx() const -> u8 {
//    assert(rm());
//    return r() ? as_r().ndx() : as_m().ndx();
//}
//
//// Supress warnings from bit-fields narrowings. We are not aware of 
//// any way to supress them other than removing the -Wconversion flag
//// from the code in question.
//GCC_DIAG_IGNORE_PUSH(-Wconversion)
//auto fiska::x86::codegen::IRMem::sib() const -> u8 {
//    assert(need_sib());
//
//    Sib sib{};
//    switch (kind()) {
//    case MK::Invalid: unreachable();
//
//    case MK::BaseDisp: {
//        sib.base = IRReg::ndx(brid_);
//        sib.index = IRX86Op::kNoIndexRegInSib;
//        break;
//    }
//    case MK::BaseIndexDisp: {
//        sib.base = IRReg::ndx(brid_);
//        sib.index = IRReg::ndx(irid_);
//        sib.scale = 1 << +scale_;
//        break;
//    }
//    case MK::IndexDisp: {
//        sib.base = IRX86Op::kNoBaseRegInSib;
//        sib.index = IRReg::ndx(irid_);
//        sib.scale = 1 << +scale_;
//        break;
//    }
//    case MK::DispOnly: {
//        sib.base = IRX86Op::kNoBaseRegInSib;
//        sib.index = IRX86Op::kNoIndexRegInSib;
//        break;
//    }
//    } // switch
//    return sib.raw();
//}
//GCC_DIAG_IGNORE_POP();
//
//auto fiska::x86::codegen::IRMem::need_sib() const -> i1 {
//    return kind() != MK::BaseDisp or isa<RI::Rsp, RI::R12>(brid_);
//}
//
//auto fiska::x86::codegen::IRBuilder::build(fe::Expr::ListRef ast_) {
//    for (fe::Expr* expr : ast_) {
//        switch (expr->kind_) {
//        default: unreachable("Sorry! Unsupported top level expression.");
//
//        case fe::Expr::Kind::Proc: {
//            auto proc = static_cast<fe::ProcExpr*>(expr);
//            IRProc ir_proc{};
//
//            IRSymbol* ir_sym = &ir_syms_.emplace_back();
//            ir_sym->kind_ = IRSymbol::Kind::Proc;
//            ir_sym->name_ = proc->name_; 
//            ir_sym->section_ = /*proc->section_;*/ ".text";
//            ir_sym->inner_ = IRProc{};
//
//            ir_proc.name_ = proc->name_;
//
//            ir_proc.body_.resize(proc->body_.size());
//            std::transform(
//                proc->body_.begin(),
//                proc->body_.end(),
//                ir_proc.body_.begin(),
//                [this](fe::X86InstrExpr* e) { return lower_x86_instr_expr(e); }
//            );
//
//            link_sym_name_to_section(ir_proc.name_, /*TODO: proc->sct_name_*/ ".text");
//            ir_objects_.push_back(std::move(ir_proc));
//            break;
//        }
//        } // switch
//    }
//}
//
//auto fiska::x86::codegen::IRBuilder::lower_expr(fe::Expr* expr) -> IRX86Op {
//    switch (expr->kind_) {
//    default: unreachable("Unsupported expr.");
//
//    case fe::Expr::Kind::RegLit: {
//        auto reg = static_cast<fe::RegLitExpr*>(expr);
//        IRReg ir_reg {
//            .bw_ = reg->bw_,
//            .id_ = reg->id_
//        };
//        return IRX86Op(ir_reg);
//    }
//    case fe::Expr::Kind::MemRefLit: {
//        auto mem = static_cast<fe::MemRefLitExpr*>(expr);
//        IRMem ir_mem {
//            .bw_ = mem->bw_,
//            .brid_ = mem->brid_,
//            .irid_ = mem->irid_,
//            //.scale_ = IRMem::Scale(std::bit_width(u8(mem->scale_)) - 1)
//        };
//        return IRX86Op(ir_mem);
//    }
//    case fe::Expr::Kind::ImmLit: {
//        auto imm = static_cast<fe::ImmLitExpr*>(expr);
//        assert((isa<fe::Expr::Kind::Label, fe::Expr::Kind::IntLit>(imm->value_->kind_)));
//
//        RelocInfo reloc_info{};
//        IRImm ir_imm {
//            .bw_ = imm->bw_,
//            .value_ = 0
//        };
//
//        if (imm->value_->kind_ == fe::Expr::Kind::Label) {
//            reloc_info.sym_name_ = static_cast<fe::LabelExpr*>(imm->value_)->name_;
//            reloc_info.must_reloc_ = true;
//        } else {
//            ir_imm.value_ = static_cast<fe::IntLitExpr*>(expr)->value_;
//        }
//
//        return IRX86Op(ir_imm, reloc_info);
//    }
//    case fe::Expr::Kind::MoffsLit: {
//        auto moffs = static_cast<fe::MoffsLitExpr*>(expr);
//        assert((isa<fe::Expr::Kind::Label, fe::Expr::Kind::IntLit>(moffs->addr_->kind_)));
//
//        RelocInfo reloc_info{};
//        IRMoffs ir_moffs {
//            .bw_ = moffs->bw_,
//            .addr_ = 0
//        };
//
//        if (moffs->addr_->kind_ == fe::Expr::Kind::Label) {
//            reloc_info.sym_name_ = static_cast<fe::LabelExpr*>(moffs->addr_)->name_;
//            reloc_info.must_reloc_ = true;
//        } else {
//            ir_moffs.addr_ = static_cast<fe::IntLitExpr*>(moffs->addr_)->value_;
//        }
//
//        return IRX86Op(ir_moffs, reloc_info);
//    }
//    case fe::Expr::Kind::BinaryOp: {
//        auto binary = static_cast<fe::BinaryOpExpr*>(expr);
//
//        assert(binary->lhs_->kind_ == fe::Expr::Kind::MemRefLit);
//        assert((isa<fe::Expr::Kind::IntLit, fe::Expr::Kind::Label>(binary->rhs_->kind_)));
//
//        auto mem = static_cast<fe::MemRefLitExpr*>(binary->lhs_);
//
//        RelocInfo reloc_info{};
//        IRMem ir_mem = lower_expr(mem).as_m();
//
//        if (binary->rhs_->kind_ == fe::Expr::Kind::Label) {
//            reloc_info.sym_name_ = static_cast<fe::LabelExpr*>(binary->rhs_)->name_;
//            reloc_info.must_reloc_ = true;
//        } else {
//            i64 disp_unchecked = static_cast<fe::IntLitExpr*>(binary->rhs_)->value_;
//            assert(utils::fits_in_b32(disp_unchecked));
//
//            ir_mem.disp_ = i32(disp_unchecked);
//        }
//        return IRX86Op(ir_mem, reloc_info);
//    }
//    } // switch
//    unreachable();
//}
//
//auto fiska::x86::codegen::IRBuilder::lower_x86_instr_expr(fe::X86InstrExpr* x86_instr_expr) -> IRX86Instr {
//    IRX86Instr ir_x86_instr {
//        .mmic_ = x86_instr_expr->mmic_,
//        .ops_ = {}
//    };
//
//    ir_x86_instr.ops_.resize(x86_instr_expr->ops_.size());
//    for (fe::Expr* op : x86_instr_expr->ops_) {
//        ir_x86_instr.ops_.push_back(lower_expr(op));
//    }
//
//    return ir_x86_instr;
//}
//
//auto fiska::x86::codegen::IRBuilder::link_sym_name_to_section(StrRef sym_name, StrRef sct_name) -> void {
//    // Make sure the symbol name doesn't exist, otherwise we would have a conflict.
//    assert(not sym_sct_info_.contains(sym_name), "Duplicate symbols!");
//    sym_sct_info_[Str{sym_name}] = Str{sct_name};
//}
