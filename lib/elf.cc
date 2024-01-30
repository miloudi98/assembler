#include "lib/elf.hh"

#include <cstring>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>

#include "lib/core.hh"
#include "lib/x86_core.hh"

// This function breaks the strict aliasing rule, but it's perfectly 
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

    // Erase the 'UND' symbol.
    std::erase_if(syms, [&](const Elf64_Sym& sym) { return *(strtab + sym.st_name) == '\0'; });

    std::sort(syms.begin(), syms.end(),
        [](const Elf64_Sym& me, const Elf64_Sym& other) {
            return me.st_value < other.st_value;
        }
    );

    utils::StringMap<ByteVec> instr_encoding;
    for (u32 sym_idx = 0; sym_idx < syms.size(); ++sym_idx) {
        const Elf64_Sym& sym = syms[sym_idx];

        instr_encoding[Str{strtab + sym.st_name}] = ByteVec{
            text.begin() + sym.st_value,
            sym_idx == syms.size() - 1 ? text.end() : text.begin() + syms[sym_idx + 1].st_value
        };
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
            case B8: return "spl";
            case B16: return "sp";
            case B32: return "esp";
            case B64: return "rsp";
            default: unreachable();
            } // switch 
        }
        case Rah: {
            switch (bw) {
            case B8: return "ah";
            default: unreachable();
            } // switch
        }
        case Rbp: {
            switch (bw) {
            case B8: return "bpl";
            case B16: return "bp";
            case B32: return "ebp";
            case B64: return "rbp";
            default: unreachable();
            } // switch 
        }
        case Rch: {
            switch (bw) {
            case B8: return "ch";
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
        case Rdh: {
            switch (bw) {
            case B8: return "dh";
            default: unreachable();
            }
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
        case Rbh: {
            switch (bw) {
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
            Str ret{"r"};

            ret += fmt::format("{}", 8 + (+reg.id_) - (+R8));
            
            switch (bw) {
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

auto fiska::get_encoding_of(Ctx* ctx, const Vec<X86Instruction*>& instructions) -> utils::StringMap<ByteVec> {
    //=============================================================================
    // Generate the file.
    //=============================================================================
    Str gas_file{};

    gas_file += fmt::format(".intel_syntax noprefix\n\n");
    gas_file += fmt::format(".text\n\n");

    for (u64 instr_idx = 0; instr_idx < instructions.size(); ++instr_idx) {
        gas_file += fmt::format(
            "i{}: {}\n",
            instr_idx,
            translate_x86_instr_to_gas_syntax(instructions[instr_idx])
        );
    }

    //=============================================================================
    // Write the file to a temporary location and assemble it.
    //=============================================================================
    fs::path tmp_path = utils::random_tmp_path(".fiska.as");
    fs::path out_path = fs::path(fmt::format("{}.o", tmp_path.string()));

    auto success = utils::write_file(gas_file.data(), gas_file.size(), tmp_path);
    assert(success, "Failed to write the file '{}'.", tmp_path.string());

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
    // the instructions.
    //=============================================================================
    const File* elf = ctx->get_file(ctx->load_file(out_path)); 
    defer {
        ctx->files_.pop_back();
        fs::remove_all(tmp_path);
        fs::remove_all(out_path);
    };

    return get_instr_encodings(elf);
}

namespace fiska {
using r8 = r<BW::B8>;
using r16 = r<BW::B16>;
using r32 = r<BW::B32>;
using r64 = r<BW::B64>;
using sreg = r<BW::B16, RK::Seg>;

using m8 = m<BW::B8>;
using m16 = m<BW::B16>;
using m32 = m<BW::B32>;
using m64 = m<BW::B64>;

template <BW w>
using rm = Any<r<w>, m<w>>;

using rm8 = rm<BW::B8>;
using rm16 = rm<BW::B16>;
using rm32 = rm<BW::B32>;
using rm64 = rm<BW::B64>;

using moffs8 = mo<BW::B8>;
using moffs16 = mo<BW::B16>;
using moffs32 = mo<BW::B32>;
using moffs64 = mo<BW::B64>;

using imm8 = i<BW::B8>;
using imm16 = i<BW::B16>;
using imm32 = i<BW::B32>;
using imm64 = i<BW::B64>;
}

void fiska::run_global_tests() {
    // 0x88 MOV r/m8, r8 -- MR
    Vec<X86Op> rm8_inst = rm8::instances();
    Vec<X86Op> r8_inst = r8::instances();

    //================================================
    // Test setup.
    //================================================
    auto test_ctx = std::make_unique<Ctx>();

    auto test_mod = std::make_unique<Module>();
    test_mod->name_ = "test_mod";
    test_mod->ctx_ = test_ctx.get();

    auto main_test_proc = new (test_mod.get(), true) ProcExpr();
    main_test_proc->func_name_ = "main_test_proc";

    Assembler as{};

    for (const auto& lhs : rm8_inst) {
        for (const auto& rhs : r8_inst) {
            if (not as.is_valid_instr<X86IK::Mov>({lhs, rhs})) { continue; }
            std::ignore = new (main_test_proc) Mov(lhs, rhs, as.encode<X86IK::Mov>({lhs, rhs}));
        }
    }

    utils::StringMap<ByteVec> encodings = get_encoding_of(test_ctx.get(), main_test_proc->instructions_);

    for (auto [idx, instr] : main_test_proc->instructions_ | vws::enumerate) {
        Str key = fmt::format("i{}", idx);
        ByteVec as_encoding = utils::strmap_get(encodings, key);


        fmt::print("Instr = {}; as encoding = ", translate_x86_instr_to_gas_syntax(instr));


        fmt::print("[");
        for (auto [idx, byte] : as_encoding | vws::enumerate) {
            fmt::print("{:#04x}", byte);
            fmt::print("{}", u32(idx) == as_encoding.size() - 1 ? "" : ", ");
        }
        fmt::print("]");

        fmt::print("; fiska encoding = ");
        fmt::print("[");
        for (auto [idx, byte] : instr->encoding_ | vws::enumerate) {
            fmt::print("{:#04x}", byte);
            fmt::print("{}", u32(idx) == instr->encoding_.size() - 1 ? "" : ", ");
        }
        fmt::print("]\n");
        assert(as_encoding == instr->encoding_, "Instruction #{} assembles wrong.", idx);
    }
}

//void fiska::run_global_tests() {
//    using enum RI;
//    using enum BW;
//    using enum Mem::Scale;
//
//    Vec<i64> mem_disps = {0, 0x11223344, 0x11, 0x1122, 0x11223, -0x11223344, -0x11, -0x112};
//    Vec<i64> moffs_values = {0x1122334455, 0x11223344, 0x1122, 0x11};
//    Vec<i64> imms_values = {0x11223344, 0x1122334455667788, 0x1122, 0x112, 0x11, 0, -1, -2, -10};
//
//    Vec<Reg> regs;
//    Vec<Mem> mems;
//    Vec<Moffs> moffs;
//    Vec<Imm> imms;
//    Vec<X86Op> all_x86_operands;
//
//    //================================================
//    // Gather all possible registers.
//    //================================================
//    for (BW w : {B8, B16, B32, B64}) {
//        // GP registers.
//        for (u32 ri = +Rax; ri <= +R15; ++ri) {
//            Reg reg{w, RI(ri)};
//            if (reg.is_valid()) { regs.push_back(reg); }
//        }
//
//        Reg reg{w, RI::Rip};
//        if (reg.is_valid()) { regs.push_back(reg); }
//
//        // Segment registers.
//        for (u32 ri = +Es; ri <= +Gs; ++ri) {
//            Reg reg{w, RI(ri)};
//            if (reg.is_valid()) { regs.push_back(reg); }
//        }
//
//        // Control and debug registers.
//        for (u32 ri = +Cr0; ri <= +Dbg15; ++ri) {
//            Reg reg{w, RI(ri)};
//            if (reg.is_valid()) { regs.push_back(reg); }
//        }
//    }
//
//    fmt::print("regs.size() = {}\n", regs.size());
//
//    //================================================
//    // Gather all possible memory references.
//    //================================================
//    // MK::BaseDisp
//    for (BW w : {B8, B16, B32, B64}) {
//        for (Reg r : regs) {
//            Mem mem{w, r, std::nullopt, std::nullopt, std::nullopt};
//            if (mem.is_valid()) { mems.push_back(mem); }
//        }
//    }
//    // MK::BaseIndexDisp
//    for (BW w : {B8, B16, B32, B64}) {
//        for (Reg base : regs) {
//            for (Mem::Scale s : {One, Two, Four, Eight}) {
//                for (Reg index : regs) {
//                    Mem mem{w, base, index, s, std::nullopt};
//                    if (mem.is_valid()) { mems.push_back(mem); }
//                }
//            }
//        }
//    }
//    // MK::IndexDisp
//    for (BW w : {B8, B16, B32, B64}) {
//        for (Mem::Scale s : {One, Two, Four, Eight}) {
//            for (Reg index : regs) {
//                for (i64 disp : mem_disps) {
//                    Mem mem{w, std::nullopt, index, s, disp ? Opt<i64>{disp} : std::nullopt};
//                    if (mem.is_valid()) { mems.push_back(mem); }
//                }
//            }
//        }
//    }
//    // MK::DispOnly
//    for (BW w : {B8, B16, B32, B64}) {
//        for (i64 disp : mem_disps) {
//            if (not disp) { continue; }
//
//            Mem mem{w, std::nullopt, std::nullopt, std::nullopt, disp ? Opt<i64>{disp} : std::nullopt};
//            if (mem.is_valid()) { mems.push_back(mem); }
//        }
//    }
//
//    //================================================
//    // Gather Random offset values.
//    //================================================
//    for (BW w : {B8, B16, B32, B64}) {
//        for (i64 mo : moffs_values) {
//            moffs.push_back(Moffs{w, mo});
//        }
//    }
//
//    //================================================
//    // Gather Random immediate values.
//    //================================================
//    for (BW w : {B8, B16, B32, B64}) {
//        for (i64 imm : imms_values) {
//            imms.push_back(Imm{w, imm});
//        }
//    }
//
//    //================================================
//    // Populate the container of all x86 operands.
//    //================================================
//    for (Reg r : regs) { all_x86_operands.push_back({r}); } 
//    for (Mem m : mems) { all_x86_operands.push_back({m}); }
//    for (Moffs mo : moffs) { all_x86_operands.push_back({mo}); }
//    for (Imm imm : imms) { all_x86_operands.push_back({imm}); }
//
//    
//    
//    
//    
//
//    
//    
//    
//
//    
//    
//
//    
//
//    u64 ctr = 0;
//    for (Reg dst : regs) {
//        if (ctr >= 10) { break; }
//        for (Reg src : regs) {
//            if (not as.is_valid_instr<X86IK::Mov>({{dst}, {src}})) {
//                continue;
//            }
//
//            ctr++;
//            if (ctr >= 10) { break; }
//
//            std::ignore = new (main_test_proc) Mov({dst}, {src}, as.encode<X86IK::Mov>({{dst}, {src}}));
//        }
//    }
//
//    ModulePrinter::print_module(test_mod.get());
//}
