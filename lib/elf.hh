#ifndef __X86_ASSEMBLER_LIB_ELF_HH__
#define __X86_ASSEMBLER_LIB_ELF_HH__

#include "lib/core.hh"
#include "lib/x86_core.hh"

#include "lib/front_end.hh"

namespace fiska {

struct Elf64_Ehdr {
  static constexpr u8 ke_ident_size = 16; 

  u8 e_ident[ke_ident_size];
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
};

auto get_instr_encodings(const File* elf) -> utils::StringMap<ByteVec>;

auto translate_x86_op_to_gas_syntax(const X86Op& op) -> Str;

auto translate_x86_instr_to_gas_syntax(const X86Instruction* instr) -> Str;

auto get_encoding_of(Ctx* ctx, const Vec<X86Instruction*>& instructions) -> utils::StringMap<ByteVec>;

} // namespace fiska

#endif
