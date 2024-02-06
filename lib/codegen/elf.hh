#ifndef __X86_ASSEMBLER_LIB_CODEGEN_ELF_HH__
#define __X86_ASSEMBLER_LIB_CODEGEN_ELF_HH__

#include "lib/core.hh"
#include "lib/front_end/parser.hh"

namespace fiska::x86::codegen {

// Elf64_Ehdr.e_ident indices description.
enum {
    EI_MAG0 = 0, 
    EI_MAG1 = 1,
    EI_MAG2 = 2,
    EI_MAG3 = 3,
    EI_CLASS = 4,
    EI_DATA = 5,
    EI_VERSION = 6,
    EI_OSABI = 7,
    EI_ABIVERSION = 8,
    EI_PAD = 9,
    EI_NIDENT = 16
};

// Object file classes.
enum {
    // 64-bit architecture.
    ELFCLASS64 = 2,
};

// Object file byte orderings.
enum {
    // Little endian.
    ELFDATA2LSB = 1,
};

// Versionning
enum {
    // Current version.
    EV_CURRENT = 1,
};

// OS ABI
enum {
    // Unix System V ABI.
    ELFOSABI_NONE = 0,
};

// ELF file type.
enum {
    ET_NONE = 0,
    ET_REL = 1,
};

// Machine architectures
enum {
    // AMD x86-64 architecture
    EM_X86_64 = 62,
};

// Section's content and semantics.
enum {
    SHT_NULL = 0,
    SHT_PROGBITS = 1,
    SHT_SYMTAB = 2,
    SHT_STRTAB = 3,
};

// Symbol type and binding attributes.
enum {
    STT_NOTYPE = 0,
    STT_OBJECT = 1,
    STT_FUNC = 2,
};

// Symbol bindings.
enum {
    STB_LOCAL = 0,
    STB_GLOBAL = 1,
};

// Section flags.
enum {
    SHF_WRITE = (1 << 0),
    SHF_ALLOC = (1 << 1),
    SHF_EXECINSTR = (1 << 2),
};

// x86-64 relocations.
enum {
    // Calculated as S + A where:
    // S: Represents the value of the symbol whose index resides in the relocation entry.
    // A: Represents the addend used to compute the value of the relocatable field.
    R_X86_64_32S = 11,
};

struct Elf64_Ehdr {
    u8 e_ident[EI_NIDENT];
    u16 e_type;
    u16 e_machine;
    u32 e_version;
    u64 e_entry;
    u64 e_phoff;
    // Section header table offset.
    u64 e_shoff;
    u32 e_flags;
    u16 e_ehsize;
    u16 e_phentsize;
    u16 e_phnum;
    // Size in bytes of each entry in the section header table.
    u16 e_shentsize;
    // Number of entries in the section header table.
    u16 e_shnum;
    // Section header string table index.
    u16 e_shstrndx;
};

struct Elf64_Shdr {
    u32 sh_name;
    u32 sh_type;
    u64 sh_flags;
    u64 sh_addr;
    // Section offset.
    u64 sh_offset;
    // Section size.
    u64 sh_size;
    u32 sh_link;
    u32 sh_info;
    u64 sh_addralign;
    u64 sh_entsize;
};

// Symbol table entries for ELF64.
struct Elf64_Sym {
  u32 st_name;     // Symbol name (index into string table)
  u8 st_info;      // Symbol's type and binding attributes
  u8 st_other;     // Must be zero; reserved
  u16 st_shndx;    // Which section (header tbl index) it's defined in
  u64 st_value;    // Value or address associated with the symbol
  u64 st_size;     // Size of the symbol

  static auto set_binding_and_type(u8 b, u8 t) -> u8 { return (b << 4) | (t & 0x0f); }
};

// Relocation entry without an explicit addend.
struct Elf64_Rel {
    u64 r_offset;
    u64 r_info;
};

// Relocation entry with an explicit addend.
struct Elf64_Rela {
    u64 r_offset;
    u64 r_info;
    u32 r_addend;
};

struct StringTable {
    Vec<char> storage_ = {0};

    auto save(StrRef str) -> u32;
    auto find(StrRef str) -> u32;
    auto get(u32 offset) -> StrRef;
    auto data() -> const char*;
    auto size() -> u64;
};

enum struct ElfSection : u8 {
    Null = 0,
    Text = 1,
    Data = 2,

    NSections = 3,
};
static_assert(+ElfSection::NSections == 3);

struct IRSymbol {
    Str name_{};
    ByteVec data_{};
    ElfSection sec_{};
    u32 sec_offset_{};
    u32 symtab_idx_{};
};

struct IRReloc {
    ElfSection sec_{};
    u32 sym_offset_{};
    BW rel_sz_{};
    Rc<IRSymbol> sym_{};
};

struct IRBuilder {
    utils::StringMap<ElfSection> symtab_{};
    Vec<ByteVec> sections_(+ElfSection::NSections);
    Vec<IRSymbol> ir_symtab_{};
    
    auto gen_ir() -> void;
    auto lower_x86_operand_list(const Vec<X86OpExpr*>& expr) -> X86Instruction;
};

// Elf Section.
enum struct ES : u16 {
    Null = 0,
    ShStrTab = 1,
    StrTab = 2,
    SymTab = 3,
    Text = 4,

    NSections = 5
};

auto codegen(fe::Expr::ListRef ast, const fs::path& out_path) -> void;

}  // namespace fiska::x86::codegen

#endif // __X86_ASSEMBLER_LIB_CODE_GEN_ELF_HH__
