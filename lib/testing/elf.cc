#include "lib/testing/elf.hh"

#include <string>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>

#include "lib/core.hh"
#include "lib/x86_assembler.hh"

namespace {

auto str_of_bw_with_gas_syntax(fiska::x86::BW w) -> StrRef {
    using enum fiska::x86::BW;

    switch (w) {
    case B8: return "BYTE PTR";
    case B16: return "WORD PTR";
    case B32: return "DWORD PTR";
    case B64: return "QWORD PTR";
    default: unreachable();
    } // switch
    unreachable();
}

auto str_of_reg_with_gas_syntax(fiska::x86::Reg r) -> Str {
    using enum fiska::x86::RI;
    using enum fiska::x86::BW;

    switch (r.id_) {
    case Rax: {
        switch (r.bw_) {
        case B8: return "al";
        case B16: return "ax";
        case B32: return "eax";
        case B64: return "rax";
        default: unreachable();
        } // switch
    }
    case Rcx: {
        switch (r.bw_) {
        case B8: return "cl";
        case B16: return "cx";
        case B32: return "ecx";
        case B64: return "rcx";
        default: unreachable();
        } // switch 
    }
    case Rdx: {
        switch (r.bw_) {
        case B8: return "dl";
        case B16: return "dx";
        case B32: return "edx";
        case B64: return "rdx";
        default: unreachable();
        } // switch 
    }
    case Rbx: {
        switch (r.bw_) {
        case B8: return "bl";
        case B16: return "bx";
        case B32: return "ebx";
        case B64: return "rbx";
        default: unreachable();
        } // switch 
    }
    case Rsp: {
        switch (r.bw_) {
        case B8: return "spl";
        case B16: return "sp";
        case B32: return "esp";
        case B64: return "rsp";
        default: unreachable();
        } // switch 
    }
    case Rah: {
        switch (r.bw_) {
        case B8: return "ah";
        default: unreachable();
        } // switch
    }
    case Rbp: {
        switch (r.bw_) {
        case B8: return "bpl";
        case B16: return "bp";
        case B32: return "ebp";
        case B64: return "rbp";
        default: unreachable();
        } // switch 
    }
    case Rch: {
        switch (r.bw_) {
        case B8: return "ch";
        default: unreachable();
        } // switch
    }
    case Rip: {
        switch (r.bw_) {
        case B16: return "ip";
        case B32: return "eip";
        case B64: return "rip";
        default: unreachable();
        } // switch 
    }
    case Rsi: {
        switch (r.bw_) {
        case B8: return "sil";
        case B16: return "si";
        case B32: return "esi";
        case B64: return "rsi";
        default: unreachable();
        } // switch 
    }
    case Rdh: {
        switch (r.bw_) {
        case B8: return "dh";
        default: unreachable();
        }
    }
    case Rdi: {
        switch (r.bw_) {
        case B8: return "dil";
        case B16: return "di";
        case B32: return "edi";
        case B64: return "rdi";
        default: unreachable();
        } // switch 
    }
    case Rbh: {
        switch (r.bw_) {
        case B8: return "bh";
        default: unreachable();
        } // switch
    }
    case R8:
    case R9: 
    case R10: 
    case R11:
    case R12: 
    case R13:
    case R14:
    case R15: {
        Str ret = fmt::format("r{}", 8 + (+r.id_) - (+R8));
        switch (r.bw_) {
        case B8:
            ret += "b";
            break;
        case B16:
            ret += "w";
            break;
        case B32:
            ret += "d";
            break;
        case B64:
            break;
        default: unreachable();
        } // switch 

        return ret;
    }
    case Es: return "es";
    case Cs: return "cs";
    case Ss: return "ss";
    case Ds: return "ds";
    case Fs: return "fs";
    case Gs: return "gs";

    case Cr0:
    case Cr1:
    case Cr2:
    case Cr3:
    case Cr4:
    case Cr5:
    case Cr6:
    case Cr7:
    case Cr8:
    case Cr9:
    case Cr10:
    case Cr11:
    case Cr12:
    case Cr13:
    case Cr14:
    case Cr15: {
        return fmt::format("cr{}", (+r.id_) - (+Cr0));
    }
    case Dbg0:
    case Dbg1:
    case Dbg2:
    case Dbg3:
    case Dbg4:
    case Dbg5:
    case Dbg6:
    case Dbg7:
    case Dbg8:
    case Dbg9:
    case Dbg10:
    case Dbg11:
    case Dbg12:
    case Dbg13:
    case Dbg14:
    case Dbg15: {
        return fmt::format("dr{}", (+r.id_) - (+Dbg0));
    }
    } // switch
    unreachable();
}

} // namespace

// This function breaks the strict aliasing rule, but it's perfectly 
// safe to do it since we're only doing read operations.
auto fiska::x86::elf::read_symbols_from_elf(StrRef elf) -> Vec<InstructionBuf> {
    auto header = reinterpret_cast<const Elf64_Ehdr*>(elf.data());
    auto sht = reinterpret_cast<const Elf64_Shdr*>(elf.data() + header->e_shoff);
    const char* shstrtab = elf.data() + sht[header->e_shstrndx].sh_offset;

    utils::StringMap<Elf64_Shdr> sh_info;
    for (u32 sh_idx = 0; sh_idx < header->e_shnum; ++sh_idx) {
        const char* name = shstrtab + sht[sh_idx].sh_name;
        sh_info[Str{name}] = sht[sh_idx];
    }

    const char* strtab = elf.data() + utils::strmap_get(sh_info, ".strtab").sh_offset;

    const Elf64_Shdr& text_hdr = utils::strmap_get(sh_info, ".text");
    StrRef text{elf.data() + text_hdr.sh_offset, text_hdr.sh_size};
    
    const Elf64_Shdr& symtab_hdr = utils::strmap_get(sh_info, ".symtab");
    Vec<Elf64_Sym> syms(symtab_hdr.sh_size / symtab_hdr.sh_entsize);
    std::memcpy(syms.data(), elf.data() + symtab_hdr.sh_offset, symtab_hdr.sh_size);

    // Erase the 'UND' symbol.
    std::erase_if(syms, [&](const Elf64_Sym& sym) { return *(strtab + sym.st_name) == '\0'; });

    std::sort(syms.begin(), syms.end(),
        [](const Elf64_Sym& me, const Elf64_Sym& other) {
            return me.st_value < other.st_value;
        }
    );

    Vec<InstructionBuf> as_instruction_encoding(syms.size());
    for (u32 sym_idx = 0; sym_idx < syms.size(); ++sym_idx) {
        const Elf64_Sym& sym = syms[sym_idx];
        const char* name = strtab + sym.st_name;

        // All the procedures names match the regexp: 'i\d+'.
        assert(*name == 'i');
        u64 instr_idx = std::stoull((name + std::strlen("i")));

        InstructionBuf* ibuf = &as_instruction_encoding[instr_idx];

        ibuf->sz_ = sym_idx == syms.size() - 1
            ? u8(text.size() - sym.st_value)
            : u8(syms[sym_idx + 1].st_value - sym.st_value);

        std::memcpy(ibuf->buf_, text.data() + sym.st_value, ibuf->sz_);
    }

    return as_instruction_encoding;
}

auto fiska::x86::elf::str_of_instruction_kind_with_gas_syntax(X86IK kind) -> StrRef {
    switch (kind) {
    case X86IK::Mov: return "mov";
    case X86IK::Add: return "add";
    case X86IK::Adc: return "adc";
    case X86IK::Syscall: return "syscall";
    } // switch
    unreachable();
}

auto fiska::x86::elf::str_of_instruction_with_gas_syntax(X86Instruction::Ref instruction) -> Str {
    return fmt::format("{} {}, {}",
        str_of_instruction_kind_with_gas_syntax(instruction.kind_),
        str_of_operand_with_gas_syntax(instruction.op_list_[0]),
        str_of_operand_with_gas_syntax(instruction.op_list_[1])
    );
}

auto fiska::x86::elf::write_instructions_with_gas_syntax(const fs::path& out_path, X86Instruction::ListRef instructions) -> void {
    //=============================================================================
    // Generate the file.
    //=============================================================================
    Str gas_file{};
    gas_file.reserve(instructions.size() * 8);

    gas_file += fmt::format(".intel_syntax noprefix\n\n");
    gas_file += fmt::format(".text\n");

    for (u64 instr_idx{}; X86Instruction::Ref instr : instructions) {
        gas_file += fmt::format(
            "i{}: {}\n",
            instr_idx,
            str_of_instruction_with_gas_syntax(instr)
        );
        instr_idx++;
    }

    //=============================================================================
    // Write the file.
    //=============================================================================
    auto success = utils::write_file(gas_file.data(), gas_file.size(), out_path);
    assert(success, "Failed to write the file '{}'.", out_path.string());
}


auto fiska::x86::elf::assemble_instructions_with_gas(X86Instruction::ListRef instructions) -> Vec<InstructionBuf> {
    //=============================================================================
    // Write the instructions to a temporary location and assemble it.
    //=============================================================================
    fs::path tmp_path = utils::random_tmp_path(".fiska.as");
    fs::path out_path = fs::path(fmt::format("{}.o", tmp_path.string()));

    write_instructions_with_gas_syntax(tmp_path, instructions);
    defer {
        fs::remove_all(tmp_path);
        fs::remove_all(out_path);
    };

    auto pid = fork();
    assert(pid > -1, "Failed to fork a new process when attempting to assemble the file.");

    // Child process
    if (pid == 0) {
        execl(
            "/usr/bin/as",
            "/usr/bin/as",
            tmp_path.string().c_str(),
            "-o",
            out_path.string().c_str(),
            NULL
        );

        unreachable("Failed to assemble file '{}'.", tmp_path.string()); 

    // Parent process
    } else {
        int status;
        waitpid(pid, &status, 0);
        // Make sure the assembler exited normally and didn't encounter any errors.
        assert(WIFEXITED(status) and WEXITSTATUS(status) == 0, "GAS failed assembling the file.");
    }

    //=============================================================================
    // Read the elf file assembled by the assembler and extract the encoding of
    // the symbols.
    //=============================================================================
    Vec<char> elf_file = utils::load_file(out_path);
    return read_symbols_from_elf({elf_file.data(), elf_file.size()});
}

auto fiska::x86::elf::str_of_operand_with_gas_syntax(X86Op::Ref op) -> Str {
    using enum BW;

    // Translate |Reg|.
    if (op.is<Reg>()) { return str_of_reg_with_gas_syntax(op.as<Reg>()); }

    // Translate |Mem|.
    if (op.is<Mem>()) {
        Str ret;
        ret.reserve(64);

        const Mem& mem = op.as<Mem>();

        ret += fmt::format("{} [", str_of_bw_with_gas_syntax(mem.bw_));
        if (mem.base_reg_) {
            ret += str_of_reg_with_gas_syntax(mem.base_reg_.value());
        }

        if (mem.index_reg_) {
            ret += fmt::format("{} {}*{}",
                mem.base_reg_ ? " +" : "",
                1 << +mem.scale_.value(),
                str_of_reg_with_gas_syntax(mem.index_reg_.value())
            );
        }

        if (mem.disp_bw_ != B0) {
            ret += mem.disp_ >= 0 ? "+" : "";
            ret += fmt::format("{:#04x}", mem.disp_);
        }

        ret += "]";
        return ret;
    }

    // Translate |Imm|.
    if (op.is<Imm>()) { return fmt::format("{}", op.as<Imm>().value_); }

    // Translate |Moffs|.
    if (op.is<Moffs>()) {
        const Moffs& moffs = op.as<Moffs>();
        return fmt::format("{} [{:#04}]",
            str_of_bw_with_gas_syntax(moffs.bw_),
            moffs.addr_
        );
    }

    unreachable();
}

