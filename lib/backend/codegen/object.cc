#include "lib/backend/codegen/object.hh"
#include "lib/backend/codegen/emitters.hh"
#include "lib/backend/x86instructions/assembler.hh"
#include "lib/backend/ir.hh"
#include "lib/common/elf.hh"
#include "lib/common/base.hh"

constexpr StrRef kShstrtab = "shstrtab";
constexpr StrRef kSymtab = "symtab";
constexpr StrRef kText = "text";
constexpr StrRef kStrtab = "strtab";

namespace fiska::assembler::backend {
namespace {

struct ObjectFile;
auto string_table_add(ObjectFile*, StrRef, StrRef) -> u32;

struct ELFSection {
    using Iterator = Map<Str, ELFSection>::iterator;

    Str name_;
    Elf64_Shdr header_{};
    ByteVec data_;
    Vec<Elf64_Rela> relocations_;

    auto offset() const -> u64 { return data_.size(); }
};

struct ObjectFile {
    Elf64_Ehdr elf_header_{};
    std::list<ELFSection> sections_ = {ELFSection{}};
    Vec<Elf64_Sym> symbols_ = {Elf64_Sym{}};
};

auto get_section_info(ObjectFile* obj, StrRef section_name) -> Pair<u32, ELFSection&> {
    u32 idx = 0;
    for (ELFSection& s : obj->sections_) {
        if (s.name_ == section_name) { return { idx, s }; }
        idx++;
    }
    ELFSection& new_section = obj->sections_.emplace_back();
    new_section.name_ = section_name;

    new_section.header_.sh_name = string_table_add(obj, kShstrtab, new_section.name_);
    return { idx, new_section };
}

auto get_section(ObjectFile* obj, StrRef section_name) -> ELFSection& {
    return get_section_info(obj, section_name).second;
}

auto get_section_ndx(ObjectFile* obj, StrRef section_name) -> u32 {
    return get_section_info(obj, section_name).first;
}


auto string_table_add(ObjectFile* obj, StrRef section_name, StrRef name) -> u32 {
    ELFSection& s = get_section(obj, section_name);

    if (s.data_.empty()) { s.data_.push_back(0); }

    u32 offset = u32(s.data_.size());
    s.data_.resize(s.data_.size() + name.size());
    std::memcpy(s.data_.data() + offset, name.data(), name.size());
    s.data_.push_back(0);
    return offset;
}

auto write_section(ObjectFile* obj, ELFSection* section, ByteVec* out) -> u64 {
    assert(obj and section and out);

    u64 written = 0;

    // Patch the header.
    section->header_.sh_offset = out->size(); 
    section->header_.sh_size = section->data_.size();
    // Write the data.
    out->insert(out->end(), section->data_.begin(), section->data_.end());
    written += section->header_.sh_size;

    if (not section->relocations_.empty()) {
        // Create the relocation section if it doesn't already exists.
        //
        // Temporary objects are destroyed as the last step in evaluating the full-expression (1.9) that (lexically)
        // contains the point where they were created. This is true even if that evaluation ends in throwing an exception (C++03 §12.2/3).
        ELFSection& reloc_section = get_section(obj, fmt::format("rel.{}", section->name_));
        // Patch the header. 
        reloc_section.header_.sh_type = SHT_RELA;
        reloc_section.header_.sh_offset = out->size();
        reloc_section.header_.sh_size = section->relocations_.size() * sizeof(Elf64_Rela);
        reloc_section.header_.sh_entsize = sizeof(Elf64_Rela);
        reloc_section.data_.resize(reloc_section.header_.sh_size);

        std::memcpy(
            reloc_section.data_.data(),
            section->relocations_.data(),
            reloc_section.header_.sh_size
        );
    }

    return written;
}

} // namespace 

auto gen(const Vec<IRSymbol>& ir_symbols, const fs::path& out_path) -> void {
    ObjectFile obj{};

    for (const IRSymbol& ir_sym : ir_symbols) {
        u32 sym_idx = obj.symbols_.size() - 1;
        Elf64_Sym* sym = &obj.symbols_.emplace_back();
        const auto& [section_idx, section] = get_section_info(&obj, ir_sym.section_);

        sym->st_name = string_table_add(&obj, kStrtab, ir_sym.name_);
        sym->st_info = X86Info::symbol_binding_and_type(STB_GLOBAL, STT_FUNC);
        sym->st_shndx = section_idx;
        sym->st_value = section.offset();

        switch (ir_sym.kind()) {
        case IRSymbol::Kind::Invalid: unreachable();

        case IRSymbol::Kind::Proc: {
            for (const IRX86Instr& inst : ir_sym.as<IRProc>().body_) {
                X86ByteCode code = assemble(inst);

                for (const auto& reloc : code.rels_) {
                    Elf64_Rela* relocation = &section.relocations_.emplace_back();
                    relocation->r_offset = section.offset() + reloc.i_offset_;
                    relocation->r_info = X86Info::symbol_relocation_info(sym_idx, reloc.type_);
                    relocation->r_addend = reloc.addend_;
                }

                assert(not code.empty());
                section.data_.insert(section.data_.end(), code.begin(), code.end());
            }
            break;
        }
        case IRSymbol::Kind::Var: {
            section.data_.insert(
                section.data_.end(),
                ir_sym.as<IRVar>().data_.begin(),
                ir_sym.as<IRVar>().data_.end()
            );
            break;
        }
        } // switch
    }


    get_section(&obj, kText).header_.sh_type = SHT_PROGBITS;
    get_section(&obj, kText).header_.sh_flags = SHF_EXECINSTR | SHF_ALLOC;


    get_section(&obj, kSymtab).header_.sh_type = SHT_SYMTAB;
    get_section(&obj, kSymtab).header_.sh_link = get_section_ndx(&obj, kStrtab);
    get_section(&obj, kSymtab).header_.sh_info = 1;
    get_section(&obj, kSymtab).header_.sh_entsize = sizeof(Elf64_Sym);

    get_section(&obj, kShstrtab).header_.sh_type = SHT_STRTAB;

    get_section(&obj, kStrtab).header_.sh_type = SHT_STRTAB;

    // Initialize the ELF header.
    obj.elf_header_.e_ident[EI_MAG0] = 0x7f;
    obj.elf_header_.e_ident[EI_MAG1] = 'E';
    obj.elf_header_.e_ident[EI_MAG2] = 'L';
    obj.elf_header_.e_ident[EI_MAG3] = 'F';
    obj.elf_header_.e_ident[EI_CLASS] = ELFCLASS64; 
    obj.elf_header_.e_ident[EI_DATA] = ELFDATA2LSB;
    obj.elf_header_.e_ident[EI_VERSION] = EV_CURRENT;
    obj.elf_header_.e_ident[EI_OSABI] = ELFOSABI_NONE;
    obj.elf_header_.e_ident[EI_ABIVERSION] = 0;
    obj.elf_header_.e_type = ET_REL;
    obj.elf_header_.e_machine = EM_X86_64;
    obj.elf_header_.e_version = EV_CURRENT;
    obj.elf_header_.e_entry = 0;
    obj.elf_header_.e_phoff = 0;
    obj.elf_header_.e_shoff = sizeof(Elf64_Ehdr);
    obj.elf_header_.e_flags = 0;
    obj.elf_header_.e_ehsize = sizeof(Elf64_Ehdr);
    obj.elf_header_.e_phentsize = 0;
    obj.elf_header_.e_phnum = 0;
    obj.elf_header_.e_shentsize = sizeof(Elf64_Shdr);
    obj.elf_header_.e_shstrndx = get_section_ndx(&obj, kShstrtab);

    u64 offset{};
    ByteVec out(sizeof(Elf64_Ehdr));

    // Skip the elf header.
    offset += sizeof(Elf64_Ehdr);

    // Write all sections.
    for (ELFSection& s : obj.sections_) {
        offset += write_section(&obj, &s, &out);
    }

    // Update the section header table offset in the elf header.
    obj.elf_header_.e_shnum = obj.sections_.size();
    obj.elf_header_.e_shoff = offset;
    // Write the headers.
    for (ELFSection& s : obj.sections_) {
        out.resize(out.size() + sizeof(Elf64_Shdr));
        std::memcpy(out.data() + offset, &s.header_, sizeof(Elf64_Shdr));
        offset += sizeof(Elf64_Shdr);
    }
    // Write the elf header.
    std::memcpy(out.data(), &obj.elf_header_, sizeof(Elf64_Ehdr));

    // Write the file to disk.
    assert(utils::write_file(out.data(), out.size(), out_path), "Failed to write the binary file.");
}

} // namespace fiska::assembler::backend


