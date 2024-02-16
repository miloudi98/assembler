#ifndef __X86_ASSEMBLER_LIB_TESTING_ELF_HH__
#define __X86_ASSEMBLER_LIB_TESTING_ELF_HH__

#include "lib/core.hh"
#include "lib/x86_utils.hh"
#include "lib/x86_assembler.hh"

#include <string>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>

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

auto read_symbols_from_elf(StrRef elf) -> Vec<InstructionBuf>;
auto assemble_instructions_with_gas(X86Instruction::ListRef instructions) -> Vec<InstructionBuf>;
auto write_instructions_with_gas_syntax(const fs::path& dst_path, X86Instruction::ListRef instructions) -> void;
auto str_of_instruction_with_gas_syntax(X86Instruction::Ref instruction) -> Str;
auto str_of_operand_with_gas_syntax(X86Op::Ref op) -> Str;
auto str_of_instruction_kind_with_gas_syntax(X86IK kind) -> StrRef;

template <typename... X86Instr>
auto build_testing_asm_and_elf_file(const fs::path& out_path) -> void {
    Vec<X86Instruction> cpp_x86_instructions;
    Str asm_file{};
    u64 instr_idx{};

    asm_file += fmt::format(".intel_syntax noprefix\n");
    asm_file += fmt::format(".text\n");

    auto write_instructions = [&]<typename Instr>() {
        // All possible instances of a given instruction (e.g 'MOV').
        Vec<X86Op::List> instruction_instances = Instr::instances();

        for (X86Op::ListRef instr_op_list : instruction_instances) {
            cpp_x86_instructions.push_back(
                X86Instruction{Instr::ik, X86InstructionOperands(instr_op_list)}
            );

            asm_file += fmt::format("i{}: {}\n",
                instr_idx++,
                str_of_instruction_with_gas_syntax(cpp_x86_instructions.back())
            );
        }
    };

    (write_instructions.template operator()<X86Instr>(), ...);

    auto asm_file_path = fs::path(fmt::format("{}.asm", out_path.string()));
    auto elf_file_path = fs::path(fmt::format("{}.elf", out_path.string()));
    auto cpp_x86_instructions_file_path = fs::path(fmt::format("{}.cpp.x86.instructions", out_path.string()));

    //=============================================================================
    // Write the file.
    //=============================================================================
    auto success = utils::write_file(asm_file.data(), asm_file.size(), asm_file_path)
               and utils::write_file(
                       cpp_x86_instructions.data(),
                       cpp_x86_instructions.size() * sizeof (decltype(cpp_x86_instructions)::value_type),
                       cpp_x86_instructions_file_path
                    );
    assert(success, "Failed to write the file '{}'.", asm_file_path.string());

    //=============================================================================
    // Assemble the file.
    //=============================================================================
    auto pid = fork();
    assert(pid > -1, "Failed to fork a new process when attempting to assemble the file.");

    // Child process
    if (pid == 0) {
        execl(
            "/usr/bin/as",
            "/usr/bin/as",
            asm_file_path.string().c_str(),
            "-o",
            elf_file_path.string().c_str(),
            NULL
        );

        unreachable("Failed to assemble file '{}'.", out_path.string()); 

    // Parent process
    } else {
        int status;
        waitpid(pid, &status, 0);
        // Make sure the assembler exited normally and didn't encounter any errors.
        assert(WIFEXITED(status) and WEXITSTATUS(status) == 0, "GAS failed assembling the file.");
    }
}

} // namespace fiska::x86::elf

#endif // __X86_ASSEMBLER_LIB_TESTING_ELF_HH__
