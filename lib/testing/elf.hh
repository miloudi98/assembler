#ifndef __X86_ASSEMBLER_LIB_TESTING_ELF_HH__
#define __X86_ASSEMBLER_LIB_TESTING_ELF_HH__

#include "lib/core.hh"
#include "lib/x86_utils.hh"

namespace fiska::x86::elf {

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

auto read_symbols_from_elf(StrRef elf) -> Vec<ByteVec>;
auto assemble_instructions_with_gas(X86Instruction::ListRef instructions) -> Vec<ByteVec>;
auto write_instructions_with_gas_syntax(const fs::path& dst_path, X86Instruction::ListRef instructions) -> void;
auto str_of_instruction_with_gas_syntax(X86Instruction::Ref instruction) -> Str;
auto str_of_operand_with_gas_syntax(X86Op::Ref op) -> Str;

} // namespace fiska::x86::elf

#endif // __X86_ASSEMBLER_LIB_TESTING_ELF_HH__
