#include "lib/x86/common.hh"
#include "lib/support/core.hh"

const utils::StringMap<fiska::x86::X86Mnemonic> fiska::x86::X86Info::kMnemonics = {
    {"mov", X86Mnemonic::Mov},
    {"add", X86Mnemonic::Add},
    {"adc", X86Mnemonic::Adc},
    {"syscall", X86Mnemonic::Syscall},
};

const utils::StringMap<fiska::x86::RI> fiska::x86::X86Info::kRegIds = {
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

const utils::StringMap<fiska::x86::BW> fiska::x86::X86Info::kBitWidths = {
    {"b8", BW::B8},
    {"b16", BW::B16},
    {"b24", BW::B24},
    {"b32", BW::B32},
    {"b64", BW::B64},
};
