//#include "lib/codegen/elf.hh"
//#include "lib/support/core.hh"
//#include "lib/codegen/instrs/emitter.hh"
//
//auto fiska::x86::codegen::ELFBuilder::find_or_create_ndx(StrRef sct_name) -> u16 {
//    auto it = rgs::find(sct_names_, sct_name);
//    if (it == sct_names_.end()) {
//        sct_names_.push_back(sct_name);
//        scts_.emplace_back();
//        hdrs_.emplace_back();
//    }
//
//    i64 ret = std::distance(sct_names_.begin(), it);
//
//    assert(ret >= 0 and ret <= std::numeric_limits<u16>::max());
//    return u16(ret);
//}
//
//auto fiska::x86::codegen::ELFBuilder::strtab_find(StrRef sct_name, StrRef str) -> u32 {
//    u32 idx = 1;
//    while (idx < sct(sct_name).size()) {
//        const char* curr_str = reinterpret_cast<const char*>(&sct(sct_name)[idx]);
//
//        if (std::strlen(curr_str) != str.size()
//         or std::memcmp(curr_str, str.data(), str.size()))
//        {
//            idx += u32(str.size() + 1);
//            continue;
//        }
//
//        return idx;
//    }
//    return 0;
//}
//
//auto fiska::x86::codegen::ELFBuilder::strtab_find_or_add(StrRef sct_name, StrRef str) -> u32 {
//    u32 existing_offset = strtab_find(sct_name, str);
//    if (existing_offset) { return existing_offset; }
//
//    if (sct(sct_name).size() == 0) { sct(sct_name).push_back(0); }
//
//    u32 off = u32(sct(sct_name).size());
//    sct(sct_name).resize(sct(sct_name).size() + str.size());
//    std::memcpy(sct(sct_name).data() + off, str.data(), str.size());
//    return off;
//}
//
//auto fiska::x86::codegen::ELFBuilder::build(const IRBuilder& ir_builder) -> void {
//    // First pass. Add all the code of the symbols to the appropriate sections.
//    for (const IRProc& ir_object : ir_builder.ir_objects_) {
//        // Add the object as a symbol.
//        StrRef ir_object_sct = utils::strmap_get(ir_builder.sym_sct_info_, ir_object.name_);
//
//        auto* sym = &syms_.emplace_back();
//        sym->st_name = strtab_find_or_add(ir_object_sct, ir_object.name_);
//        sym->st_info = Elf64_Sym::set_binding_and_type(STB_GLOBAL, STT_FUNC);
//        sym->st_shndx = find_or_create_ndx(ir_object_sct);
//        sym->st_value = sct(ir_object_sct).size();
//
//        for (IRX86Instr::Ref ir_x86_instr : ir_object.body_) {
//            ir_x86_instr.sct_offset_ = sct(ir_object_sct).size();
//            ByteVec code = instrs::emitter::emit(ir_x86_instr);
//            // Add the instruction bytes to the symbol's section.
//            sct(ir_object_sct).insert(sct(ir_object_sct).end(), code.begin(), code.end());
//        }
//    }
//
//    // Second pass. Handle relocations.
//    //for (const IRProc& ir_object : ir_builder.ir_objects_) {
//
//    //    StrRef ir_object_sct = utils::strmap_get(ir_builder.sym_sct_info_, ir_object.name_);
//
//    //    for (IRX86Instr::Ref ir_x86_instr : ir_object.body_) {
//    //        for (IRX86Op::Ref ir_x86_op : ir_x86_instr.ops_) {
//    //            if (not ir_x86_op.reloc_info_.must_reloc_) { continue; }
//
//    //            StrRef sym_name_to_reloc = ir_x86_op.reloc_info_.sym_name_;
//
//    //            StrRef sct_of_sym_to_reloc = utils::strmap_get(ir_builder.sym_sct_info_, sym_name_to_reloc);
//
//    //            Elf64_Sym sym_to_reloc = find_sym_to_reloc();
//
//    //            auto* rel = &rels.emplace_back();
//    //            rel->r_offset = sym_to_reloc.value + ir_x86_instr.sct_offset_ + ir_x86_op.instr_offset_;
//    //            rel->rinfo = /**/ R_X86_64_32;
//    //        }
//    //    }
//    //}
//}
