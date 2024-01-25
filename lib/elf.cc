#include "lib/elf.hh"

#include <cstring>

#include "lib/core.hh"
#include "lib/x86_core.hh"

// This function breaks the strict aliasing rule. However, it's perfectly 
// safe to do it since we're only doing read operations.
auto fiska::get_instr_encodings(const File* elf) -> utils::StringMap<ByteVec> {
    
    auto header = reinterpret_cast<const Elf64_Ehdr*>(elf->data());
    auto sht = reinterpret_cast<const Elf64_Shdr*>(elf->data() + header->e_shoff);
    const char* shstrtab = elf->data() + sht[header->e_shstrndx].sh_offset;

    utils::StringMap<Elf64_Shdr> sh_info;
    for (u32 sh_idx = 0; sh_idx < header->e_shnum; ++sh_idx) {
        const char* name = shstrtab + sht[sh_idx].sh_name;
        sh_info[Str{name}] = sht[sh_idx];
    }

    const char* strtab = elf->data() + sh_info[".strtab"].sh_offset;
    StrRef text{elf->data() + sh_info[".text"].sh_offset, sh_info[".text"].sh_size};
    
    const Elf64_Shdr& symtab_hdr = sh_info[".symtab"];
    Vec<Elf64_Sym> syms(symtab_hdr.sh_size / symtab_hdr.sh_entsize);
    std::memcpy(syms.data(), elf->data() + symtab_hdr.sh_offset, symtab_hdr.sh_size);

    std::sort(syms.begin(), syms.end(),
        [](const Elf64_Sym& me, const Elf64_Sym& other) {
            return me.st_value < other.st_value;
        }
    );


    utils::StringMap<ByteVec> instr_encoding;
    for (u32 sym_idx = 0; sym_idx < syms.size(); ++sym_idx) {
        const auto& sym = syms[sym_idx];

        auto end = sym_idx == syms.size() - 1
            ? text.end()
            : text.begin() + syms[sym_idx + 1].st_value;

        instr_encoding[Str{strtab + sym.st_name}] = ByteVec{text.begin() + sym.st_value, end};
    }

    return instr_encoding;
}

auto fiska::translate_x86_op_to_gas_syntax(const X86Op& op) -> Str {
    using enum RI;
    using enum BW; 

    BW bw = op.bit_width();

    if (op.is<Reg>()) {
        const Reg& reg = op.as<Reg>();

        // TODO(miloudi): Add support for AH, CH, DH, BH.
        switch(reg.id_) {
        case Rax: {
            switch (bw) {
            case B8: return "al";
            case B16: return "ax";
            case B32: return "eax";
            case B64: return "rax";
            default: unreachable();
            } // switch
        }
        case Rcx: {
            switch (bw) {
            case B8: return "cl";
            case B16: return "cx";
            case B32: return "ecx";
            case B64: return "rcx";
            default: unreachable();
            } // switch 
        }
        case Rdx: {
            switch (bw) {
            case B8: return "dl";
            case B16: return "dx";
            case B32: return "edx";
            case B64: return "rdx";
            default: unreachable();
            } // switch 
        }
        case Rbx: {
            switch (bw) {
            case B8: return "bl";
            case B16: return "bx";
            case B32: return "ebx";
            case B64: return "rbx";
            default: unreachable();
            } // switch 
        }
        case Rsp: {
            switch (bw) {
            case B16: return "sp";
            case B32: return "esp";
            case B64: return "rsp";
            default: unreachable();
            } // switch 
        }
        case Rbp: {
            switch (bw) {
            case B16: return "bp";
            case B32: return "ebp";
            case B64: return "rbp";
            default: unreachable();
            } // switch 
        }
        case Rip: {
            switch (bw) {
            case B16: return "ip";
            case B32: return "eip";
            case B64: return "rip";
            default: unreachable();
            } // switch 
        }
        case Rsi: {
            switch (bw) {
            case B8: return "sil";
            case B16: return "si";
            case B32: return "esi";
            case B64: return "rsi";
            default: unreachable();
            } // switch 
        }
        case Rdi: {
            switch (bw) {
            case B8: return "dil";
            case B16: return "di";
            case B32: return "edi";
            case B64: return "rdi";
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
            Str ret{"r"};
            ret += std::to_string(8 + (+reg.id_) - (+R8));
            
            switch (bw) {
            case B8:
                ret += "l";
                break;
            case B16:
                ret += "w";
                break;
            case B32:
                ret += "b";
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
            return Str{"cr"} + std::to_string(+(reg.id_) - (+Cr0));
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
            return Str{"dr"} + std::to_string(+(reg.id_) - (+Dbg0));
        }
        } // switch
    }

    if (op.is<Mem>()) {
        Str ret{};
        const Mem& mem = op.as<Mem>();

        switch (bw) {
        case B8:
            ret += "BYTE PTR ";
            break;
        case B16:
            ret += "WORD PTR ";
            break;
        case B32:
            ret += "DWORD PTR ";
            break;
        case B64:
            ret += "QWORD PTR ";
            break;
        default: unreachable();
        } // switch

        switch (mem.kind()) {
        case MK::BaseDisp: {
            assert(mem.base_reg_.has_value());

            ret += "[";
            ret += translate_x86_op_to_gas_syntax(X86Op{mem.base_reg_.value()});
            if (mem.disp_) {
                ret += fmt::format("+{:#02x}", *mem.disp_);
            }
            ret += "]";

            return ret;
        }
        case MK::BaseIndexDisp: {
            assert(mem.base_reg_.has_value() and mem.index_reg_.has_value());

            ret += "[";
            ret += translate_x86_op_to_gas_syntax(X86Op{mem.base_reg_.value()});
            ret += fmt::format("+{}*", (1 << +mem.scale_.value()));
            ret += translate_x86_op_to_gas_syntax(X86Op{mem.index_reg_.value()});

            if (mem.disp_) {
                ret += fmt::format("+{:#02x}", *mem.disp_);
            }

            ret += "]";
            return ret;
        }
        case MK::IndexDisp: {
            assert(mem.index_reg_.has_value());

            ret += "[";
            ret += fmt::format("{}*", (1 << +mem.scale_.value()));
            ret += translate_x86_op_to_gas_syntax(X86Op{mem.index_reg_.value()});
            if (mem.disp_) {
                ret += fmt::format("+{:#02x}", *mem.disp_);
            }
            ret += "]";
            return ret;
        }
        case MK::DispOnly: {
            assert(mem.disp_ and not mem.base_reg_ and not mem.index_reg_);
            ret += fmt::format("[{:#02x}]", *mem.disp_);
            return ret;
        }
        } // switch
    }

    if (op.is<Moffs>()) {
        Str ret{};
        switch (bw) {
        case B8:
            ret += "BYTE PTR ";
            break;
        case B16:
            ret += "WORD PTR ";
            break;
        case B32:
            ret += "DWORD PTR ";
            break;
        case B64:
            ret += "QWORD PTR ";
            break;
        default: unreachable();
        } // switch

        return ret + fmt::format("[{:#02x}]", op.as<Moffs>().as<u64>());
    }

    if (op.is<Imm>()) {
        return fmt::format("{}", op.as<Imm>().as<u64>()); 
    }

    unreachable("Unhandled operand.");
}

auto fiska::translate_x86_instr_to_gas_syntax(const X86Instruction* instr) -> Str {
    switch (instr->kind_) {
    case X86IK::Mov: {
        Str ret{};
    
        auto mov = static_cast<const Mov*>(instr);

        return fmt::format("mov {}, {}",
            translate_x86_op_to_gas_syntax(mov->dst_),
            translate_x86_op_to_gas_syntax(mov->src_)
        );
    }
    } // switch
    unreachable();
}

auto fiska::get_encoding_of(const Vec<X86Instruction*>& instructions) -> Vec<ByteVec> {
    Str gas_file{};

    gas_file += fmt::format(".intel_syntax noprefix\n\n");
    gas_file += fmt::format(".text\n\n");

    u64 instr_ctr = 0;

    for (u64 instr_idx = 0; instr_idx < instructions.size(); ++instr_idx) {
        gas_file += fmt::format("i{}: {}\n",
            instr_idx,
            translate_x86_instr_to_gas_syntax(instructions[instr_idx])
        );
    }

    // write the file to a temporary location.
    // assemble the file.
    // pass the ELf file to get_instr_encodings.
    todo();
}
