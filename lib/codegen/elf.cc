#include "lib/codegen/elf.hh"

#include <cstring>

#include "lib/core.hh"
#include "lib/instructions/emitter.hh"
#include "lib/x86_utils.hh"
#include "lib/front_end/parser.hh"

namespace {

auto append_proc(ByteVec& txt, fiska::fe::ProcExpr* proc) -> void {
    using fiska::x86::X86Instruction;
    using fiska::x86::InstructionBuf;

    for (fiska::x86::X86Instruction::Ref i : proc->instructions_) {
        InstructionBuf bytecode = fiska::x86::instructions::emit(i);
        for (u8 idx = 0; idx < bytecode.sz_; ++idx) { txt.push_back(bytecode.buf_[idx]); }
    }
}

} // namespace

auto fiska::x86::codegen::StringTable::save(StrRef str) -> u32 {
    u32 existing_idx = find(str);
    if (existing_idx) { return existing_idx; }

    u32 offset = u32(storage_.size());

    storage_.reserve(storage_.size() + str.size());
    storage_.insert(storage_.end(), str.begin(), str.end());
    storage_.push_back('\0');

    return offset;
}

auto fiska::x86::codegen::StringTable::size() -> u64 {
    return storage_.size();
}

auto fiska::x86::codegen::StringTable::data() -> const char* {
    return storage_.data();
}

auto fiska::x86::codegen::StringTable::find(StrRef str) -> u32 {
    // Skip the empty name at the beginning of each string table.
    u32 idx = 1;
    while (idx < storage_.size()) {
        const char* name = &storage_[idx];

        if ( (std::strlen(name) != str.size())
          or (std::memcmp(name, str.data(), str.size())))
        {
            idx += u32(str.size() + 1);
            continue;
        }

        return idx;
    }
    return 0;
}

auto fiska::x86::codegen::StringTable::get(u32 offset) -> StrRef {
    assert(offset < storage_.size(), "Index out of bounds.");
    return {&storage_[offset], std::strlen(&storage_[offset])};
}

auto fiska::x86::codegen::codegen(fe::Expr::ListRef ast, const fs::path& out_path) -> void {
    using enum ES;
    using fe::Expr;
    using fe::ProcExpr;
    using fe::VarExpr;
    using fe::EK;

    Elf64_Ehdr elf_hdr{};
    Arr<Elf64_Shdr, +NSections> sheaders{};
    StringTable shstrtab_sec{};
    StringTable strtab_sec{};
    Vec<Elf64_Sym> symtab_sec = {Elf64_Sym{}};
    ByteVec text_sec{};


    // Initialize the ELF header.
    elf_hdr.e_ident[EI_MAG0] = 0x7f;
    elf_hdr.e_ident[EI_MAG1] = 'E';
    elf_hdr.e_ident[EI_MAG2] = 'L';
    elf_hdr.e_ident[EI_MAG3] = 'F';
    elf_hdr.e_ident[EI_CLASS] = ELFCLASS64; 
    elf_hdr.e_ident[EI_DATA] = ELFDATA2LSB;
    elf_hdr.e_ident[EI_VERSION] = EV_CURRENT;
    elf_hdr.e_ident[EI_OSABI] = ELFOSABI_NONE;
    elf_hdr.e_ident[EI_ABIVERSION] = 0;

    elf_hdr.e_type = ET_REL;
    elf_hdr.e_machine = EM_X86_64;
    elf_hdr.e_version = EV_CURRENT;
    elf_hdr.e_entry = 0;
    elf_hdr.e_phoff = 0;
    elf_hdr.e_shoff = sizeof(Elf64_Ehdr);
    elf_hdr.e_flags = 0;
    elf_hdr.e_ehsize = sizeof(Elf64_Ehdr);
    elf_hdr.e_phentsize = 0;
    elf_hdr.e_phnum = 0;
    elf_hdr.e_shentsize = sizeof(Elf64_Shdr);
    elf_hdr.e_shnum = +NSections;
    elf_hdr.e_shstrndx = +ShStrTab;

    // Initialze section headers.
    //
    // Section header string table.
    sheaders[+ShStrTab].sh_name = shstrtab_sec.save(".shstrtab");
    sheaders[+ShStrTab].sh_type = SHT_STRTAB;
    sheaders[+ShStrTab].sh_flags = 0;
    sheaders[+ShStrTab].sh_addr = 0;
    sheaders[+ShStrTab].sh_offset = 0;
    sheaders[+ShStrTab].sh_size = 0;
    sheaders[+ShStrTab].sh_link = 0;
    sheaders[+ShStrTab].sh_info = 0;
    sheaders[+ShStrTab].sh_addralign = 1;
    sheaders[+ShStrTab].sh_entsize = 0;

    // Text section.
    sheaders[+Text].sh_name = shstrtab_sec.save(".text");
    sheaders[+Text].sh_type = SHT_PROGBITS;
    sheaders[+Text].sh_flags = SHF_EXECINSTR | SHF_ALLOC;
    sheaders[+Text].sh_addr = 0;
    sheaders[+Text].sh_offset = 0;
    sheaders[+Text].sh_size = 0;
    sheaders[+Text].sh_link = 0;
    sheaders[+Text].sh_info = 0;
    sheaders[+Text].sh_addralign = 16;
    sheaders[+Text].sh_entsize = 0;

    // Symbol table.
    sheaders[+SymTab].sh_name = shstrtab_sec.save(".symtab");
    sheaders[+SymTab].sh_type = SHT_SYMTAB;
    sheaders[+SymTab].sh_flags = 0;
    sheaders[+SymTab].sh_addr = 0;
    sheaders[+SymTab].sh_offset = 0;
    sheaders[+SymTab].sh_size = 0;
    // The section header index of the associated string table.
    sheaders[+SymTab].sh_link = +StrTab;
    // One greater than the symbol table index of the last local symbol (binding STB_LOCAL).
    // This is set to one because we are not going to have local symbols initially. We'll remove
    // this constraint in the future.
    sheaders[+SymTab].sh_info = 1;
    sheaders[+SymTab].sh_addralign = 8;
    sheaders[+SymTab].sh_entsize = sizeof(Elf64_Sym);

    // String table.
    sheaders[+StrTab].sh_name = shstrtab_sec.save(".strtab");
    sheaders[+StrTab].sh_type = SHT_STRTAB;
    sheaders[+StrTab].sh_flags = 0;
    sheaders[+StrTab].sh_addr = 0;
    sheaders[+StrTab].sh_offset = 0;
    sheaders[+StrTab].sh_size = 0;
    sheaders[+StrTab].sh_link = 0;
    sheaders[+StrTab].sh_info = 0;
    sheaders[+StrTab].sh_addralign = 1;
    sheaders[+StrTab].sh_entsize = 0;

    // Initialize the sections.
    for (Expr* expr : ast) {
        switch (expr->kind_) {
        case EK::ProcExpr: {
            auto proc_expr = static_cast<ProcExpr*>(expr);

            Elf64_Sym* sym = &symtab_sec.emplace_back();
            sym->st_name = strtab_sec.save(proc_expr->name_);
            sym->st_info = Elf64_Sym::set_binding_and_type(STB_GLOBAL, STT_FUNC);
            sym->st_other = 0;
            sym->st_shndx = +Text;
            sym->st_value = text_sec.size();
            append_proc(text_sec, proc_expr);
            break;
        }
        case EK::VarExpr: {
            //auto var_expr = static_cast<VarExpr*>(expr);
            //todo("Do the same thign as procexpr, the only difference is that symbols will be in the .data section.");
            break;
        }
        } // switch
    }

    u64 bin_sz = sizeof(Elf64_Ehdr)
        + sheaders.size() * sizeof(Elf64_Shdr)
        + shstrtab_sec.size()
        + strtab_sec.size()
        + text_sec.size()
        + symtab_sec.size() * sizeof(Elf64_Sym);

    u64 offset = 0;
    ByteVec out(bin_sz);

    // Skip the Elf64_Ehdr
    offset += sizeof(Elf64_Ehdr);

    // Append the section header string table.
    //
    // Update the missing fields in the section header.
    sheaders[+ShStrTab].sh_offset = offset;
    sheaders[+ShStrTab].sh_size = shstrtab_sec.size();
    // Write the section.
    std::memcpy(out.data() + offset, shstrtab_sec.data(), shstrtab_sec.size());
    // Advance the offset.
    offset += shstrtab_sec.size();


    // Append the string table.
    //
    // Update the missing fields in the section header.
    sheaders[+StrTab].sh_offset = offset;
    sheaders[+StrTab].sh_size = strtab_sec.size();
    // Write the section.
    std::memcpy(out.data() + offset, strtab_sec.data(), strtab_sec.size());
    // Advance the offset.
    offset += strtab_sec.size();

    // Append the text section.
    //
    // Update the missing fields in the section header.
    sheaders[+Text].sh_offset = offset;
    sheaders[+Text].sh_size = text_sec.size();
    // Write the section.
    std::memcpy(out.data() + offset, text_sec.data(), text_sec.size());
    // Advance the offset.
    offset += text_sec.size();

    // Append the symbol table.
    //
    // Update the missing fields in the section header.
    sheaders[+SymTab].sh_offset = offset;
    sheaders[+SymTab].sh_size = symtab_sec.size() * sizeof(Elf64_Sym);
    // Write the section.
    std::memcpy(out.data() + offset, symtab_sec.data(), symtab_sec.size() * sizeof(Elf64_Sym));
    // Advance the offset.
    offset += symtab_sec.size() * sizeof(Elf64_Sym);

    // Update the section header table offset in the elf header.
    elf_hdr.e_shoff = offset;

    // Write the section header table.
    std::memcpy(out.data() + offset, sheaders.data(), sheaders.size() * sizeof(Elf64_Shdr));
    // Advance the offset.
    offset += sheaders.size() * sizeof(Elf64_Shdr);

    // Write the elf header.
    std::memcpy(out.data(), &elf_hdr, sizeof(Elf64_Ehdr));

    assert(utils::write_file(out.data(), out.size(), out_path), "Failed to write elf file to '{}'.", out_path.string());

}
