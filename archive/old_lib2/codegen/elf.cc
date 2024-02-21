#include "lib/codegen/elf.hh"

#include <cstring>

#include "lib/core.hh"
#include "lib/instructions/emitter.hh"
#include "lib/x86_utils.hh"
#include "lib/front_end/parser.hh"

auto fiska::x86::codegen::ELFBuilder::find_or_create_ndx(StrRef sct_name) -> u16 {
    auto it = rgs::find(sct_names_, sct_name);
    if (it == sct_names_.end()) {
        sct_names_.push_back(sct_name);
        scts_.emplace_back();
        hdrs_.emplace_back();
    }

    i64 ret = std::distance(sct_names_.begin(), it);

    assert(ret >= 0 and ret <= std::numeric_limits<u16>::max());
    return u16(ret);
}

auto fiska::x86::codegen::ELFBuilder::strtab_find(StrRef sct_name, StrRef str) -> u32 {
    u32 idx = 1;
    while (idx < sct(sct_name).size()) {
        const char* curr_str = reinterpret_cast<const char*>(&sct(sct_name)[idx]);

        if (std::strlen(curr_str) != str.size()
         or std::memcmp(curr_str, str.data(), str.size()))
        {
            idx += u32(str.size() + 1);
            continue;
        }

        return idx;
    }
    return 0;
}

auto fiska::x86::codegen::ELFBuilder::strtab_find_or_add(StrRef sct_name, StrRef str) -> u32 {
    u32 existing_offset = strtab_find(sct_name, str);
    if (existing_offset) { return existing_offset; }

    if (sct(sct_name).size() == 0) { sct(sct_name).push_back(0); }

    u32 off = u32(sct(sct_name).size());
    sct(sct_name).resize(sct(sct_name).size() + str.size());
    std::memcpy(sct(sct_name).data() + off, str.data(), str.size());
    return off;

}

//auto fiska::x86::codegen::ELFBuilder::build_syms(fe::Expr::ListRef ast) -> void {
//    using namespace fe;
//
//    for (Expr* expr : ast) {
//        switch (expr->kind_) {
//        case EK::ProcExpr: {
//            auto proc = static_cast<ProcExpr*>(expr);
//
//            // No duplicate symbols.
//            assert(strtab_find(".strtab", proc->name_) == 0);
//
//            auto* sym = &syms_.emplace_back();
//            sym->st_name = strtab_find_or_add(".strtab", proc->name_);
//            sym->st_info = Elf64_Sym::set_binding_and_type(STB_GLOBAL, STT_FUNC); 
//            // TODO(miloudi): Make all expression carry the section in which they reside.
//            sym->st_shndx = find_or_create_ndx(".text");
//            sym->st_value = sct(".text").size();
//
//            for (X86InstructionExpr::Ref x86_inst_expr : proc->body_) {
//                X86Instruction i = lower_x86_inst_expr(x86_inst_expr);
//
//                InstructionBuf code = instructions::emit(i);
//                assert(not code.empty());
//
//                if (i.patchable_) {
//                    auto* rel = &rels_.emplace_back();
//                    rel->r_offset = code.patch_offset_ + sym->st_value;
//                    // We are currently only dealing with PC relative relocations. The RIP always points to the next instruction.
//                    // That's why we need this sort of relocation.
//                    rel->r_addend = 4;
//                    rel->r_info = 0/**/;
//                }
//            }
//
//        }
//        } // switch
//    }
//
//}
//
//auto fiska::x86::codegen::ELFBuilder::lower_x86_inst_expr(fe::X86InstructionExpr::Ref x86_inst_expr) -> X86Instruction {
//    using namespace fe;
//
//    X86Instruction i{};
//    i.kind_ = x86_inst_expr.kind_;
//
//    Vec<X86Op> lowered_op_list{};
//    for (X86OpExpr* op : x86_inst_expr.operands_) {
//        switch (op->ty_) {
//        case X86OpK::RegExpr: {
//            auto reg = static_cast<RegExpr*>(op);
//            lowered_op_list.push_back(X86Op { Reg(reg->bw_, reg->id_) });
//            break;
//        }
//        case X86OpK::ImmExpr: {
//            auto imm = static_cast<ImmExpr*>(op);
//            lowered_op_list.push_back(X86Op { Imm{imm->bw_, imm->value_} });
//            break;
//        }
//        case X86OpK::MoffsExpr: {
//            auto moffs = static_cast<MoffsExpr*>(op);
//            lowered_op_list.push_back(X86Op { Moffs{moffs->bw_, moffs->addr_} });
//            break;
//        }
//        case X86OpK::MemRefExpr: {
//            auto mem = static_cast<MemRefExpr*>(op);
//            i.patchable_ = std::holds_alternative<StrRef>(mem->disp_);
//
//            Opt<Reg> breg{};
//            if (mem->breg_id_ != RI::Invalid) { breg.emplace(BW::B64, mem->breg_id_); }
//            Opt<Reg> ireg{};
//            if (mem->ireg_id_ != RI::Invalid) { ireg.emplace(BW::B64, mem->ireg_id_); }
//
//            Opt<MemIndexScale> scale{};
//            if (mem->scale_ != MemIndexScale::Invalid) { scale.emplace(mem->scale_); }
//
//            i64 disp = i.patchable_ ? 0 : std::get<i64>(mem->disp_);
//
//            lowered_op_list.push_back(X86Op { Mem::make(mem->bw_, breg, ireg, scale, disp) });
//            break;
//        }
//        } // switch
//    }
//
//    i.op_list_ = X86InstructionOperands(lowered_op_list);
//
//    return i;
//}
//
////auto fiska::x86::codegen::codegen(fe::Expr::ListRef ast, const fs::path& out_path) -> void {
////    using enum ES;
////    using fe::Expr;
////    using fe::ProcExpr;
////    using fe::VarExpr;
////    using fe::EK;
////
////    Elf64_Ehdr elf_hdr{};
////    Arr<Elf64_Shdr, +NSections> sheaders{};
////    StringTable shstrtab_sec{};
////    StringTable strtab_sec{};
////    Vec<Elf64_Sym> symtab_sec = {Elf64_Sym{}};
////    ByteVec text_sec{};
////
////
////    // Initialize the ELF header.
////    elf_hdr.e_ident[EI_MAG0] = 0x7f;
////    elf_hdr.e_ident[EI_MAG1] = 'E';
////    elf_hdr.e_ident[EI_MAG2] = 'L';
////    elf_hdr.e_ident[EI_MAG3] = 'F';
////    elf_hdr.e_ident[EI_CLASS] = ELFCLASS64; 
////    elf_hdr.e_ident[EI_DATA] = ELFDATA2LSB;
////    elf_hdr.e_ident[EI_VERSION] = EV_CURRENT;
////    elf_hdr.e_ident[EI_OSABI] = ELFOSABI_NONE;
////    elf_hdr.e_ident[EI_ABIVERSION] = 0;
////
////    elf_hdr.e_type = ET_REL;
////    elf_hdr.e_machine = EM_X86_64;
////    elf_hdr.e_version = EV_CURRENT;
////    elf_hdr.e_entry = 0;
////    elf_hdr.e_phoff = 0;
////    elf_hdr.e_shoff = sizeof(Elf64_Ehdr);
////    elf_hdr.e_flags = 0;
////    elf_hdr.e_ehsize = sizeof(Elf64_Ehdr);
////    elf_hdr.e_phentsize = 0;
////    elf_hdr.e_phnum = 0;
////    elf_hdr.e_shentsize = sizeof(Elf64_Shdr);
////    elf_hdr.e_shnum = +NSections;
////    elf_hdr.e_shstrndx = +ShStrTab;
////
////    // Initialze section headers.
////    //
////    // Section header string table.
////    sheaders[+ShStrTab].sh_name = shstrtab_sec.save(".shstrtab");
////    sheaders[+ShStrTab].sh_type = SHT_STRTAB;
////    sheaders[+ShStrTab].sh_flags = 0;
////    sheaders[+ShStrTab].sh_addr = 0;
////    sheaders[+ShStrTab].sh_offset = 0;
////    sheaders[+ShStrTab].sh_size = 0;
////    sheaders[+ShStrTab].sh_link = 0;
////    sheaders[+ShStrTab].sh_info = 0;
////    sheaders[+ShStrTab].sh_addralign = 1;
////    sheaders[+ShStrTab].sh_entsize = 0;
////
////    // Text section.
////    sheaders[+Text].sh_name = shstrtab_sec.save(".text");
////    sheaders[+Text].sh_type = SHT_PROGBITS;
////    sheaders[+Text].sh_flags = SHF_EXECINSTR | SHF_ALLOC;
////    sheaders[+Text].sh_addr = 0;
////    sheaders[+Text].sh_offset = 0;
////    sheaders[+Text].sh_size = 0;
////    sheaders[+Text].sh_link = 0;
////    sheaders[+Text].sh_info = 0;
////    sheaders[+Text].sh_addralign = 16;
////    sheaders[+Text].sh_entsize = 0;
////
////    // Symbol table.
////    sheaders[+SymTab].sh_name = shstrtab_sec.save(".symtab");
////    sheaders[+SymTab].sh_type = SHT_SYMTAB;
////    sheaders[+SymTab].sh_flags = 0;
////    sheaders[+SymTab].sh_addr = 0;
////    sheaders[+SymTab].sh_offset = 0;
////    sheaders[+SymTab].sh_size = 0;
////    // The section header index of the associated string table.
////    sheaders[+SymTab].sh_link = +StrTab;
////    // One greater than the symbol table index of the last local symbol (binding STB_LOCAL).
////    // This is set to one because we are not going to have local symbols initially. We'll remove
////    // this constraint in the future.
////    sheaders[+SymTab].sh_info = 1;
////    sheaders[+SymTab].sh_addralign = 8;
////    sheaders[+SymTab].sh_entsize = sizeof(Elf64_Sym);
////
////    // String table.
////    sheaders[+StrTab].sh_name = shstrtab_sec.save(".strtab");
////    sheaders[+StrTab].sh_type = SHT_STRTAB;
////    sheaders[+StrTab].sh_flags = 0;
////    sheaders[+StrTab].sh_addr = 0;
////    sheaders[+StrTab].sh_offset = 0;
////    sheaders[+StrTab].sh_size = 0;
////    sheaders[+StrTab].sh_link = 0;
////    sheaders[+StrTab].sh_info = 0;
////    sheaders[+StrTab].sh_addralign = 1;
////    sheaders[+StrTab].sh_entsize = 0;
////
////    // Initialize the sections.
////    for (Expr* expr : ast) {
////        switch (expr->kind_) {
////        case EK::ProcExpr: {
////            auto proc_expr = static_cast<ProcExpr*>(expr);
////
////            Elf64_Sym* sym = &symtab_sec.emplace_back();
////            sym->st_name = strtab_sec.save(proc_expr->name_);
////            sym->st_info = Elf64_Sym::set_binding_and_type(STB_GLOBAL, STT_FUNC);
////            sym->st_other = 0;
////            sym->st_shndx = +Text;
////            sym->st_value = text_sec.size();
////            append_proc(text_sec, proc_expr);
////            break;
////        }
////        case EK::VarExpr: {
////            auto var_expr = static_cast<VarExpr*>(expr);
////
////            Elf64_Sym* sym = &symtab_sec.emplace_back();
////            sym->st_name = strtab_sec.save(var_expr->label_);
////            //todo("Do the same thign as procexpr, the only difference is that symbols will be in the .data section.");
////            break;
////        }
////        } // switch
////    }
////
////    u64 bin_sz = sizeof(Elf64_Ehdr)
////        + sheaders.size() * sizeof(Elf64_Shdr)
////        + shstrtab_sec.size()
////        + strtab_sec.size()
////        + text_sec.size()
////        + symtab_sec.size() * sizeof(Elf64_Sym);
////
////    u64 offset = 0;
////    ByteVec out(bin_sz);
////
////    // Skip the Elf64_Ehdr
////    offset += sizeof(Elf64_Ehdr);
////
////    // Append the section header string table.
////    //
////    // Update the missing fields in the section header.
////    sheaders[+ShStrTab].sh_offset = offset;
////    sheaders[+ShStrTab].sh_size = shstrtab_sec.size();
////    // Write the section.
////    std::memcpy(out.data() + offset, shstrtab_sec.data(), shstrtab_sec.size());
////    // Advance the offset.
////    offset += shstrtab_sec.size();
////
////
////    // Append the string table.
////    //
////    // Update the missing fields in the section header.
////    sheaders[+StrTab].sh_offset = offset;
////    sheaders[+StrTab].sh_size = strtab_sec.size();
////    // Write the section.
////    std::memcpy(out.data() + offset, strtab_sec.data(), strtab_sec.size());
////    // Advance the offset.
////    offset += strtab_sec.size();
////
////    // Append the text section.
////    //
////    // Update the missing fields in the section header.
////    sheaders[+Text].sh_offset = offset;
////    sheaders[+Text].sh_size = text_sec.size();
////    // Write the section.
////    std::memcpy(out.data() + offset, text_sec.data(), text_sec.size());
////    // Advance the offset.
////    offset += text_sec.size();
////
////    // Append the symbol table.
////    //
////    // Update the missing fields in the section header.
////    sheaders[+SymTab].sh_offset = offset;
////    sheaders[+SymTab].sh_size = symtab_sec.size() * sizeof(Elf64_Sym);
////    // Write the section.
////    std::memcpy(out.data() + offset, symtab_sec.data(), symtab_sec.size() * sizeof(Elf64_Sym));
////    // Advance the offset.
////    offset += symtab_sec.size() * sizeof(Elf64_Sym);
////
////    // Update the section header table offset in the elf header.
////    elf_hdr.e_shoff = offset;
////
////    // Write the section header table.
////    std::memcpy(out.data() + offset, sheaders.data(), sheaders.size() * sizeof(Elf64_Shdr));
////    // Advance the offset.
////    offset += sheaders.size() * sizeof(Elf64_Shdr);
////
////    // Write the elf header.
////    std::memcpy(out.data(), &elf_hdr, sizeof(Elf64_Ehdr));
////
////    assert(utils::write_file(out.data(), out.size(), out_path), "Failed to write elf file to '{}'.", out_path.string());
////
////}
//
////auto fiska::x86::codegen::IRBuilder::gen_ir() -> void {
////    using fiska::fe::Expr;
////    using fiska::fe::ProcExpr;
////    using fiska::fe::EK;
////        
////    for (Expr* expr : ast_) {
////        switch (expr->kind_) {
////        case EK::ProcExpr: {
////            auto proc = static_cast<ProcExpr*>(expr);
////
////            IRSymbol* ir_sym = &ir_symtab_.emplace_back();
////            ir_sym->name_ = proc->name_;
////            ir_sym->sec_ = utils::strmap_get(sym_tab_, proc->name_);
////            ir_sym->sec_offset_ = sections_[+ir_sym->sec_].size();
////            ir_sym->symtab_idx_ = u32(ir_symtab_.size() - 1);
////
////            for (const X86InstructionExpr& x86_inst_expr : proc->body_) {
////                X86Instruction lowered_instr = lower_x86_instruction(x86_inst_expr);
////                auto code = instructions::emit(lowered_instr);
////                ir_sym->data_.insert(ir_sym->data_.end(), code.begin(), code.end());
////
////                for (u8 expr_idx = 0; expr_idx < u8(x86_inst_expr.operands_.size()); ++expr_idx) {
////                    if (x86_inst_expr.operands_[expr_idx].kind_ != X86OpIK::LabelExpr) { continue; }
////
////                    IRReloc* rel = &rels.emplace_back();
////                    rel->sec_ = ir_sym->sec_;
////                    rel->sym_offset_ = ir_sym->data_.size() - code.size() + code.disp_or_imm_offset_[expr_idx];
////                    rel->sym_ = ir_sym;
////                }
////            }
////        }
////        } // switch
////    }
////}
////
////auto fiska::x86::codegen::IRBuilder::lower_x86_instruction(X86Instruction::Ref instr) -> X86Instruction::List {
////    // Get the instruction kind.
////    // get the operands.
////    switch (expr->kind_) {
////    case X86OpIK::RegExpr: {
////        auto reg = static_cast<RegExpr*>(expr);
////        return X86Op {
////            Reg(reg->bw_, reg->id_)
////        };
////    }
////    case X86OpIK::MemRefExpr: {
////        auto mem_ref = static_cast<MemRefExpr*>(expr);
////
////        i.patchable_ = std::holds_alternative<StrRef>(mem_ref.disp_);
////        disp = i.patchable_ ? 0 : mem_ref.disp_.get<i64>().value();
////    }
////    case X86OpIK::LabelExpr: {
////        auto label = static_cast<LabelExpr*>(expr);
////
////    }
////    }
////}
