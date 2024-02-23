#include "lib/backend/codegen/object.hh"
#include "lib/backend/codegen/emitters.hh"
#include "lib/backend/x86instructions/assembler.hh"
#include "lib/backend/ir.hh"
#include "lib/common/elf.hh"
#include "lib/common/base.hh"

constexpr u8 kShstrtabIdx = 1;
constexpr u8 kStrtabIdx = 2;
constexpr u8 kSymtabIdx = 3;
constexpr StrRef kShstrtab = ".shstrtab";
constexpr StrRef kStrtab = ".strtab";
constexpr StrRef kSymtab = ".symtab";
constexpr std::array non_relocatable_sections = {kSymtab, kShstrtab, kStrtab};

namespace fiska::assembler::backend {
namespace {

struct ObjectFile;
struct ELFSection;

auto create_section(ObjectFile*, StrRef) -> ELFSection*;
auto create_object_file() -> ObjectFile;
auto find_section(ObjectFile*, StrRef) -> ELFSection*;
auto add(ELFSection* s, const X86ByteCode& code) -> u32;
auto add(ELFSection* s, const ByteVec& code) -> u32;
auto add(ELFSection* s, StrRef str) -> u32;
auto add(ELFSection* s, const Elf64_Rela& rel) -> u32;
auto add(ELFSection* s, const Elf64_Sym& sym) -> u32;

struct ELFSection {
    Elf64_Shdr header_{};
    ByteVec data_;
    Box<ELFSection> reloc_section_;

    friend auto create_section(ObjectFile* obj, StrRef section_name) -> ELFSection*;
    friend auto create_object_file() -> ObjectFile;
private:
    ELFSection() = default;
};

struct ObjectFile {
    Elf64_Ehdr elf_header_{};
    Vec<ELFSection> sections_;
    Vec<Elf64_Sym> symbols_;

    friend auto create_object_file() -> ObjectFile;
private:
    ObjectFile() = default;
};

auto register_symbol_name(ObjectFile* obj, StrRef sym_name) -> u32 {
    ELFSection* s = &obj->sections_[kStrtabIdx];
    return add(s, sym_name);
}

auto register_section_name(ObjectFile* obj, StrRef section_name) -> u32 {
    ELFSection* s = &obj->sections_[kShstrtabIdx];
    return add(s, section_name);
}

auto fetch_symbol_name(ObjectFile* obj, u32 idx) -> StrRef {
    assert(idx);
    const char* name = reinterpret_cast<const char*>(obj->sections_[kStrtabIdx].data_.data() + idx);
    return StrRef{name, std::strlen(name)};
}

auto fetch_section_name(ObjectFile* obj, u32 idx) -> StrRef {
    const char* name = reinterpret_cast<const char*>(obj->sections_[kShstrtabIdx].data_.data() + idx);
    return StrRef{name, std::strlen(name)};
}

auto symbol_ndx(ObjectFile* obj, StrRef sym_name) -> u32 {
    ELFSection* strtab = find_section(obj, kStrtab);

    for (u32 idx{}; const Elf64_Sym& symbol : obj->symbols_) {
        const char* symbol_name = reinterpret_cast<const char*>(strtab->data_.data() + symbol.st_name); 

        if (
            std::memcmp(
                symbol_name,
                sym_name.data(),
                std::min(std::strlen(symbol_name), sym_name.size())
                ) == 0
            ) { return idx; }

        idx++;
    }
    return 0;
}

auto section_ndx(ObjectFile* obj, StrRef section_name) -> u32 {
    for (u32 idx{}; const ELFSection& s : obj->sections_) {
        if (fetch_section_name(obj, s.header_.sh_name) == section_name) { return idx; }
        idx++;
    }
    return 0;
}

auto create_section(ObjectFile* obj, StrRef section_name) -> ELFSection* {
    ELFSection* s = &obj->sections_.emplace_back(ELFSection());
    s->header_.sh_name = register_section_name(obj, section_name);

    if (rgs::contains(non_relocatable_sections, section_name)) {
        return s;
    }

    // Initialize the relocation section.
    s->reloc_section_ = Box<ELFSection>(new ELFSection());

    s->reloc_section_->header_.sh_name = register_section_name(obj, fmt::format(".rela{}", section_name));
    s->reloc_section_->header_.sh_type = SHT_RELA;
    s->reloc_section_->header_.sh_flags = SHF_INFO_LINK;
    s->reloc_section_->header_.sh_link = section_ndx(obj, kSymtab);
    s->reloc_section_->header_.sh_info = section_ndx(obj, section_name);
    s->reloc_section_->header_.sh_entsize = sizeof(Elf64_Rela);

    return s;
}

auto create_symbol(ObjectFile* obj, StrRef sym_name, u8 binding, u8 type, ELFSection* s) -> Elf64_Sym* {
    Elf64_Sym* sym = &obj->symbols_.emplace_back();
    sym->st_name = register_symbol_name(obj, sym_name);
    sym->st_info = X86Info::symbol_binding_and_type(binding, type);
    sym->st_value = s->data_.size();
    sym->st_shndx = section_ndx(obj, fetch_section_name(obj, s->header_.sh_name));

    return sym;
}

auto find_section(ObjectFile* obj, StrRef section_name) -> ELFSection* {
    for (ELFSection& s : obj->sections_) {
        if (fetch_section_name(obj, s.header_.sh_name) != section_name) { continue; }
        return &s;
    }
    unreachable("Section name: '{}' not found.", section_name);
}

auto find_or_create_section(ObjectFile* obj, StrRef section_name) -> ELFSection* {
    for (ELFSection& s : obj->sections_) {
        if (fetch_section_name(obj, s.header_.sh_name) != section_name) { continue; }
        return &s;
    }
    return create_section(obj, section_name);
}

auto add(ELFSection* s, const X86ByteCode& code) -> u32 {
    u64 offset = s->data_.size();
    s->data_.insert(s->data_.end(), code.begin(), code.end());
    return u32(offset);
}

auto add(ELFSection* s, const ByteVec& code) -> u32 {
    u64 offset = s->data_.size();
    s->data_.insert(s->data_.end(), code.begin(), code.end());
    return u32(offset);
}

auto add(ELFSection* s, StrRef str) -> u32 {
    if (s->data_.empty()) { s->data_.push_back(0); }

    u64 offset = s->data_.size();
    s->data_.insert(s->data_.end(), str.begin(), str.end());
    s->data_.push_back(0);

    return u32(offset);
}

auto add(ELFSection* s, const Elf64_Rela& rel) -> u32 {
    u64 offset = s->data_.size();
    s->data_.resize(s->data_.size() + sizeof(Elf64_Rela));
    std::memcpy(s->data_.data() + offset, &rel, sizeof(Elf64_Rela));
    return u32(offset);
}

auto add(ELFSection* s, const Elf64_Sym& sym) -> u32 {
    u64 offset = s->data_.size();
    s->data_.resize(s->data_.size() + sizeof(Elf64_Sym));
    std::memcpy(s->data_.data() + offset, &sym, sizeof(Elf64_Sym));
    return u32(offset);
}

auto create_object_file() -> ObjectFile {
    ObjectFile obj{};

    // Add the dummy symbol.
    obj.symbols_.emplace_back();
    // Add the NULL section.
    obj.sections_.emplace_back(ELFSection());

    ELFSection* shstrtab = create_section(&obj, kShstrtab);
    shstrtab->header_.sh_type = SHT_STRTAB;

    ELFSection* strtab = create_section(&obj, kStrtab);
    strtab->header_.sh_type = SHT_STRTAB;

    ELFSection* symtab = create_section(&obj, kSymtab);
    symtab->header_.sh_type = SHT_SYMTAB;
    symtab->header_.sh_link = section_ndx(&obj, kStrtab);
    symtab->header_.sh_info = 1;
    symtab->header_.sh_entsize = sizeof(Elf64_Sym);

    return obj;
}

auto write_section(ELFSection* s, ByteVec* out) -> u64 {
    u64 written{};

    // Patch the section header.
    s->header_.sh_offset = out->size();
    s->header_.sh_size = s->data_.size();

    // Write the data.
    out->insert(out->end(), s->data_.begin(), s->data_.end());
    written += s->header_.sh_size;

    if (s->reloc_section_ and not s->reloc_section_->data_.empty()) {
        // Patch the section header.
        s->reloc_section_->header_.sh_offset = out->size();
        s->reloc_section_->header_.sh_size = s->reloc_section_->data_.size();

        // Write the data.
        out->insert(out->end(), s->reloc_section_->data_.begin(), s->reloc_section_->data_.end());
        written += s->reloc_section_->header_.sh_size;
    }

    return written;
}

auto write_header(ELFSection* s, ByteVec* out) -> u64 {
    u64 written{};
    u64 offset = out->size();

    out->resize(out->size() + sizeof(Elf64_Shdr));
    std::memcpy(out->data() + offset, &s->header_, sizeof(Elf64_Shdr));
    written += sizeof(Elf64_Shdr);

    if (s->reloc_section_ and not s->reloc_section_->data_.empty()) {
        // write the relocation section header.
        out->resize(out->size() + sizeof(Elf64_Shdr));
        std::memcpy(out->data() + written + offset, &s->reloc_section_->header_, sizeof(Elf64_Shdr));
        written += sizeof(Elf64_Shdr);
    }

    return written;
}

auto total_number_of_sections(ObjectFile* obj) -> u32 {
    u32 ret{};
    for (const ELFSection& s : obj->sections_) {
        ret += 1 + u32(s.reloc_section_ and not s.reloc_section_->data_.empty());
    }
    return ret;
}

} // namespace 

auto gen(const Vec<IRSymbol>& ir_symbols, const fs::path& out_path) -> void {
    ObjectFile obj = create_object_file();

    for (const IRSymbol& ir_sym : ir_symbols) {
        ELFSection* section = find_or_create_section(&obj, ir_sym.section_);
        Elf64_Sym* sym = create_symbol(&obj, ir_sym.name_, STB_GLOBAL, STT_FUNC, section); 

        switch (ir_sym.kind()) {
        case IRSymbol::Kind::Invalid: unreachable();

        case IRSymbol::Kind::Proc: {
            for (const IRX86Instr& inst : ir_sym.as<IRProc>().body_) {
                X86ByteCode code = assemble(inst);

                for (const auto& reloc : code.rels_) {
                    fmt::print("codegen reloc.symbol_name = {}\n", reloc.symbol_name_);
                    if (not reloc.valid()) { continue; }

                    Elf64_Rela rela {
                        .r_offset = section->data_.size() + reloc.i_offset_,
                        .r_info = X86Info::symbol_relocation_info(symbol_ndx(&obj, ir_sym.name_), reloc.type_),
                        .r_addend = reloc.addend_
                    };
                    add(section->reloc_section_.get(), rela);
                }

                assert(not code.empty());
                add(section, code);
            }
            break;
        }
        case IRSymbol::Kind::Var: {
            add(section, ir_sym.as<IRVar>().data_);
            break;
        }
        } // switch

        // Add the symbol to the symbol table.
        add(find_section(&obj, kSymtab), *sym);
    }

    // Intialize the elf header.
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
    obj.elf_header_.e_shstrndx = kShstrtabIdx;
    obj.elf_header_.e_shnum = total_number_of_sections(&obj);

    // Update the section types and flags.
    ELFSection* text = find_section(&obj, "text");
    text->header_.sh_type = SHT_PROGBITS;
    text->header_.sh_flags = SHF_EXECINSTR | SHF_ALLOC;

    u64 offset{};
    ByteVec out(sizeof(Elf64_Ehdr));

    // Skip the elf header.
    offset += sizeof(Elf64_Ehdr);

    // Write all the sections.
    for (ELFSection& s : obj.sections_) {
        offset += write_section(&s, &out);
    }

    // Update the main elf header.
    obj.elf_header_.e_shoff = offset;

    // Write the headers.
    for (ELFSection& s : obj.sections_) {
        offset += write_header(&s, &out);
    }

    // Write the elf header.
    std::memcpy(out.data(), &obj.elf_header_, sizeof(Elf64_Ehdr));

    // Write the file to disk.
    assert(utils::write_file(out.data(), out.size(), out_path), "Failed to write the binary file.");
}

//
//
//    u64 offset{};
//    ByteVec out(sizeof(Elf64_Ehdr));
//
//    // Skip the elf header.
//    offset += sizeof(Elf64_Ehdr);
//
//    // Write all sections.
//    for (ELFSection& s : obj.sections_) {
//        offset += write_section(&obj, &s, &out);
//    }
//
//    // Update the section header table offset in the elf header.
//    obj.elf_header_.e_shnum = obj.sections_.size();
//    obj.elf_header_.e_shoff = offset;
//    // Write the headers.
//    for (ELFSection& s : obj.sections_) {
//        out.resize(out.size() + sizeof(Elf64_Shdr));
//        std::memcpy(out.data() + offset, &s.header_, sizeof(Elf64_Shdr));
//        offset += sizeof(Elf64_Shdr);
//    }
//    // Write the elf header.
//    std::memcpy(out.data(), &obj.elf_header_, sizeof(Elf64_Ehdr));
//
//    // Write the file to disk.
//    assert(utils::write_file(out.data(), out.size(), out_path), "Failed to write the binary file.");
//}

} // namespace fiska::assembler::backend





//auto write_section(ObjectFile* obj, ELFSection* section, ByteVec* out) -> u64 {
//    assert(obj and section and out);
//
//    u64 written = 0;
//
//    // Patch the header.
//    section->header_.sh_offset = out->size(); 
//    section->header_.sh_size = section->data_.size();
//    // Write the data.
//    out->insert(out->end(), section->data_.begin(), section->data_.end());
//    written += section->header_.sh_size;
//
//    if (not section->relocations_.empty()) {
//        // Create the relocation section if it doesn't already exists.
//        //
//        // Temporary objects are destroyed as the last step in evaluating the full-expression (1.9) that (lexically)
//        // contains the point where they were created. This is true even if that evaluation ends in throwing an exception (C++03 §12.2/3).
//        ELFSection& reloc_section = get_section(obj, fmt::format("rel.{}", section->name_));
//        // Patch the header. 
//        reloc_section.header_.sh_type = SHT_RELA;
//        reloc_section.header_.sh_offset = out->size();
//        reloc_section.header_.sh_size = section->relocations_.size() * sizeof(Elf64_Rela);
//        reloc_section.header_.sh_entsize = sizeof(Elf64_Rela);
//        reloc_section.data_.resize(reloc_section.header_.sh_size);
//
//        std::memcpy(
//            reloc_section.data_.data(),
//            section->relocations_.data(),
//            reloc_section.header_.sh_size
//        );
//    }
//
//    return written;
//}
