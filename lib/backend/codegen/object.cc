#include "lib/backend/codegen/object.hh"
#include "lib/backend/codegen/emitters.hh"
#include "lib/backend/x86instructions/assembler.hh"
#include "lib/backend/ir.hh"
#include "lib/common/elf.hh"
#include "lib/common/base.hh"

constexpr StrRef kShstrtab = ".shstrtab";
constexpr StrRef kSymtab = ".symtab";
constexpr StrRef kText = ".text";
constexpr StrRef kSymtab = ".symtab";
constexpr StrRef kStrtab = ".strtab";

namespace fiska::assembler::backend {
namespace {

auto string_table_add(ObjectFile*, StrRef, StrRef) -> u16;

struct ELFSection {
    StrRef name_;
    Elf64_Shdr header_{};
    ByteVec data_;
    Vec<Elf64_Rela> relocations_;

    auto offset() const -> u64 {
        return data_.size();
    }

    auto size() const -> u64 {
        u64 reloc_size = relocations_.empty() 
            * (sizeof(Elf64_Shdr) + relocations_.size() * sizeof(Elf64_Rela)); 

        return data_.size() + sizeof(Elf64_Shdr) + reloc_size;
    }
};

struct ObjectFile {
    Elf64_Ehdr elf_header_{};
    Vec<ELFSection> sections_;
    utils::StringMap<Elf64_Sym> symbols_;
};

auto find_section(ObjectFile* obj, StrRef section_name) -> ELFSection* {
    for (ELFSection& section : obj->sections_) {
        if (section.name_ != section_name) { continue; }
        return &section;
    }

    string_table_add(obj, kShstrtab, section_name);
    return &obj->sections_.emplace_back();
}

auto section_ndx(ObjectFile* obj, StrRef section_name) -> u16 {
    return u16(find_section(obj, section_name) - obj->sections_.data());
}

auto string_table_add(ObjectFile* obj, StrRef section_name, StrRef name) -> u16 {
    ByteVec& strtab = find_section(obj, section_name)->data_;
    if (strtab.empty()) { strtab.push_back(0); }

    u16 offset = strtab.size();
    strtab.resize(strtab.size() + name.size());
    std::memcpy(strtab.data() + offset, name.data(), name.size());
    return offset;
}

} // namespace 

auto gen(const Vec<IRSymbol>& ir_symbols) -> ByteVec {
    ObjectFile obj{};

    for (const IRSymbol& ir_sym : ir_symbols) {
        Elf64_Sym* sym = &obj.symbols_[Str(ir_sym.name_)];
        ELFSection* section = find_section(&obj, ir_sym.section_);

        // TODO: Make sure there are no duplicate symbol names.
        sym->st_name = string_table_add(&obj, kSymtab, ir_sym.name_);
        sym->st_info = X86Info::symbol_binding_and_type(STB_GLOBAL, STT_FUNC);
        sym->st_shndx = section_ndx(&obj, section->name_);
        sym->st_value = section->offset();

        switch (ir_sym.kind_) {
        case IRSymbol::Kind::Invalid: unreachable();

        case IRSymbol::Kind::Proc: {
            for (const IRX86Instr& inst : ir_sym.as<IRProc>().body_) {
                X86ByteCode code = assemble(inst);

                for (const auto& reloc : code.rels_) {
                    Elf64_Rela* relocation = &section->relocations_.emplace_back();
                    relocation->r_offset = section->offset() + reloc.i_offset_;
                    relocation->r_info = reloc.type_;
                    relocation->r_addend = reloc.addend_;
                }

                assert(not code.empty());
                section->data_.insert(section->data_.end(), code.begin(), code.end());
            }
            break;
        }
        case IRSymbol::Kind::Var: {
            section->data_.insert(
                section->data_.end(),
                ir_sym.as<IRVar>().data_.begin(),
                ir_sym.as<IRVar>().data_.end()
            );
            break;
        }
        } // switch
    }

    // Create all the relocation sections.
    for (ELFSection& section : obj.sections_) {
        if (section.relocations_.empty()) { continue; }
        
        ELFSection* reloc_section = find_section(&obj, fmt::format(".rel{}", section.name_));
        reloc_section->header_.sh_type = SHT_RELA;

        reloc_section->data_.resize(section.relocations_.size() * sizeof(Elf64_Rela));
        std::memcpy(
            reloc_section->data_.data(),
            section.relocations_.data(),
            reloc_section->data_.size()
        );
    }

    find_section(&obj, kShstrtab)->header_.sh_type = SHT_STRTAB;

    find_section(&obj, kStrtab)->header_.sh_type = SHT_STRTAB;

    find_section(&obj, kText)->header_.sh_type = SHT_PROGBITS;
    find_section(&obj, kText)->header_.sh_flags = SHF_EXECINSTR | SHF_ALLOC;

    find_section(&obj, kSymtab)->header_.sh_type = SHT_SYMTAB;
    find_section(&obj, kSymtab)->header_.sh_link = section_ndx(&obj, kStrtab);
    find_section(&obj, kSymtab)->header_.sh_info = 1;
    find_section(&obj, kSymtab)->header_.sh_entsize = sizeof(Elf64_Sym);

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
    obj.elf_header_.e_shnum = obj.sections_.size();
    obj.elf_header_.e_shstrndx = section_ndx(&obj, kShstrtab);

    u64 offset{};
    ByteVec out;

    // Skip the elf header.
    offset += sizeof(Elf64_Ehdr);


} // namespace fiska::assembler::backend


