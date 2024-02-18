#include "lib/common/x86/types.hh"
#include "lib/common/base.hh"

namespace fiska::assembler {

const utils::StringMap<X86Mnemonic> X86Info::kMnemonics = {
    {"mov", X86Mnemonic::Mov},
    {"add", X86Mnemonic::Add},
    {"adc", X86Mnemonic::Adc},
    {"syscall", X86Mnemonic::Syscall},
};

const utils::StringMap<RI> X86Info::kRegIds = {
    {"rax", RI::Rax}, {"rcx", RI::Rcx}, {"rdx", RI::Rdx}, {"rbx", RI::Rbx}, {"rsp", RI::Rsp}, {"rbp", RI::Rbp},
    {"rsi", RI::Rsi}, {"rdi", RI::Rdi}, {"rip", RI::Rip}, {"r8", RI::R8}, {"r9", RI::R9}, {"r10", RI::R10},
    {"r11", RI::R11}, {"r12", RI::R12}, {"r13", RI::R13}, {"r14", RI::R14}, {"r15", RI::R15}, {"rah", RI::Rah},
    {"Rch", RI::Rch}, {"Rdh", RI::Rdh}, {"Rbh", RI::Rbh},

    {"es", RI::Es}, {"cs", RI::Cs}, {"ss", RI::Ss}, {"ds", RI::Ds}, {"fs", RI::Fs}, {"gs", RI::Gs},

    {"cr0", RI::Cr0}, {"cr1", RI::Cr1}, {"cr2", RI::Cr2}, {"cr3", RI::Cr3}, {"cr4", RI::Cr4},
    {"cr5", RI::Cr5}, {"cr6", RI::Cr6}, {"cr7", RI::Cr7}, {"cr8", RI::Cr8}, {"cr9", RI::Cr9},
    {"cr10", RI::Cr10}, {"cr11", RI::Cr11}, {"cr12", RI::Cr12}, {"cr13", RI::Cr13}, {"cr14", RI::Cr14}, 
    {"cr15", RI::Cr15},

    {"dbg0", RI::Dbg0}, {"dbg1", RI::Dbg1}, {"dbg2", RI::Dbg2}, {"dbg3", RI::Dbg3}, {"dbg4", RI::Dbg4},
    {"dbg5", RI::Dbg5}, {"dbg6", RI::Dbg6}, {"dbg7", RI::Dbg7}, {"dbg8", RI::Dbg8}, {"dbg9", RI::Dbg9},
    {"dbg10", RI::Dbg10}, {"dbg11", RI::Dbg11}, {"dbg12", RI::Dbg12}, {"dbg13", RI::Dbg13}, {"dbg14", RI::Dbg14},
    {"dbg15", RI::Dbg15},
};

const utils::StringMap<BW> X86Info::kBitWidths = {
    {"b8", BW::B8},
    {"b16", BW::B16},
    {"b24", BW::B24},
    {"b32", BW::B32},
    {"b64", BW::B64},
};

auto X86Info::register_req_ext(RI ri) -> i1 {
    return (+ri >= +RI::R8 and +ri <= +RI::R15)
        or (+ri >= +RI::Cr8 and +ri <= +RI::Cr15)
        or (+ri >= +RI::Dbg8 and +ri <= +RI::Dbg15);
}

auto X86Info::register_kind(RI ri) -> RK {
    switch (ri) {
    case RI::Invalid: unreachable();

    case RI::Rax:
    case RI::Rcx:
    case RI::Rdx:
    case RI::Rbx:
    case RI::Rsp:
    case RI::Rbp:
    case RI::Rsi:
    case RI::Rdi:
    case RI::R8:
    case RI::R9:
    case RI::R10:
    case RI::R11:
    case RI::R12:
    case RI::R13:
    case RI::R14:
    case RI::R15:
    case RI::Rah:
    case RI::Rch:
    case RI::Rdh:
    case RI::Rbh:
        return RK::Gp;

    case RI::Rip:
        return RK::Ip;

    case RI::Es:
    case RI::Cs:
    case RI::Ss:
    case RI::Ds:
    case RI::Fs:
    case RI::Gs:
        return RK::Seg;

    case RI::Cr0:
    case RI::Cr1:
    case RI::Cr2:
    case RI::Cr3:
    case RI::Cr4:
    case RI::Cr5:
    case RI::Cr6:
    case RI::Cr7:
    case RI::Cr8:
    case RI::Cr9:
    case RI::Cr10:
    case RI::Cr11:
    case RI::Cr12:
    case RI::Cr13:
    case RI::Cr14:
    case RI::Cr15:
        return RK::Ctrl;

    case RI::Dbg0:
    case RI::Dbg1:
    case RI::Dbg2:
    case RI::Dbg3:
    case RI::Dbg4:
    case RI::Dbg5:
    case RI::Dbg6:
    case RI::Dbg7:
    case RI::Dbg8:
    case RI::Dbg9:
    case RI::Dbg10:
    case RI::Dbg11:
    case RI::Dbg12:
    case RI::Dbg13:
    case RI::Dbg14:
    case RI::Dbg15:
        return RK::Dbg;
    } // switch
    unreachable();
}

auto X86Info::register_ndx(RI ri) -> u8 {
    switch (ri) {
    case RI::Invalid: unreachable();

    case RI::Rax: return 0;
    case RI::Rcx: return 1;
    case RI::Rdx: return 2;
    case RI::Rbx: return 3;
    case RI::Rsp: return 4;
    case RI::Rbp: return 5;
    case RI::Rsi: return 6;
    case RI::Rdi: return 7;
    case RI::R8:  return 0;
    case RI::R9:  return 1;
    case RI::R10: return 2;
    case RI::R11: return 3;
    case RI::R12: return 4;
    case RI::R13: return 5;
    case RI::R14: return 6;
    case RI::R15: return 7;
    case RI::Rah: return 4;
    case RI::Rch: return 5;
    case RI::Rdh: return 6;
    case RI::Rbh: return 7;

    case RI::Rip: return 5;

    case RI::Es: return 0;
    case RI::Cs: return 1;
    case RI::Ss: return 2;
    case RI::Ds: return 3;
    case RI::Fs: return 4;
    case RI::Gs: return 5;

    case RI::Cr0:  return 0;
    case RI::Cr1:  return 1;
    case RI::Cr2:  return 2;
    case RI::Cr3:  return 3;
    case RI::Cr4:  return 4;
    case RI::Cr5:  return 5;
    case RI::Cr6:  return 6;
    case RI::Cr7:  return 7;
    case RI::Cr8:  return 0;
    case RI::Cr9:  return 1;
    case RI::Cr10: return 2;
    case RI::Cr11: return 3;
    case RI::Cr12: return 4;
    case RI::Cr13: return 5;
    case RI::Cr14: return 6;
    case RI::Cr15: return 7;

    case RI::Dbg0:  return 0;
    case RI::Dbg1:  return 1;
    case RI::Dbg2:  return 2;
    case RI::Dbg3:  return 3;
    case RI::Dbg4:  return 4;
    case RI::Dbg5:  return 5;
    case RI::Dbg6:  return 6;
    case RI::Dbg7:  return 7;
    case RI::Dbg8:  return 0;
    case RI::Dbg9:  return 1;
    case RI::Dbg10: return 2;
    case RI::Dbg11: return 3;
    case RI::Dbg12: return 4;
    case RI::Dbg13: return 5;
    case RI::Dbg14: return 6;
    case RI::Dbg15: return 7;
    } // switch

    unreachable();
}

} // namespace fiska::assembler
