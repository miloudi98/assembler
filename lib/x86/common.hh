#ifndef __X86_ASSEMBLER_LIB_X86_COMMON_HH__
#define __X86_ASSEMBLER_LIB_X86_COMMON_HH__

#include "lib/support/core.hh"

namespace fiska::x86 {

// X86 Instruction mnemonic.
enum struct X86Mnemonic {
    Invalid,

    Mov,
    Add,
    Adc,
    Syscall,
};

// Bit width.
enum struct BW : i8 {
    Invalid = -1,

    B8 = 8,
    B16 = 16,
    B24 = 24,
    B32 = 32,
    B64 = 64
};

// Register Id.
enum struct RI : u8 {
    Invalid,

    Rax,  Rcx,  Rdx,  
    Rbx,  Rsp,  Rbp,  
    Rsi,  Rdi,  R8,    
    R9,   R10,  R11,  
    R12,  R13,  R14,  
    R15,
    // 8-bit Rsp
    Rah,
    // 8-bit Rbp
    Rch,
    // 8-bit Rsi
    Rdh,
    // 8-bit Rdi
    Rbh,

    Rip,

    Es, Cs, Ss,
    Ds, Fs, Gs,

    Cr0, Cr1, Cr2,
    Cr3, Cr4, Cr5,
    Cr6, Cr7, Cr8,
    Cr9, Cr10, Cr11,
    Cr12, Cr13, Cr14,
    Cr15,

    Dbg0, Dbg1, Dbg2,
    Dbg3, Dbg4, Dbg5,
    Dbg6, Dbg7, Dbg8,
    Dbg9, Dbg10, Dbg11,
    Dbg12, Dbg13, Dbg14,
    Dbg15
};

// Register kind.
enum struct RK : i8 {
    Invalid, 

    Ip,
    Gp,
    Seg,
    Ctrl,
    Dbg
};

// Memory reference kind.
enum struct MK {
    Invalid,

    BaseDisp,
    BaseIndexDisp,
    IndexDisp,
    DispOnly
};

struct Rex {
    // Mod_Rm::r/m or Sib::Base extension or opcode extension.
    u8 b: 1{};
    // Sib::Index extension.
    u8 x: 1{};
    // Mod_Rm::reg extension.
    u8 r: 1{};
    // Operand size override.
    u8 w: 1{};
    // Reserved.
    u8 mod: 4 {0b0100}; 

    auto raw() const -> u8 {
        return (mod << 4) | (w << 3) | (r << 2) | (x << 1) | b;
    }
    auto has_set_bits() const -> i1 {
        return b or x or r or w;
    }
};

struct ModRM {
    u8 rm: 3{};
    u8 reg: 3{};
    u8 mod: 2{};

    auto raw() const -> u8 {
        return (mod << 6) | (reg << 3) | rm;
    }
};

struct Sib {
    u8 base: 3{};
    u8 index: 3{};
    u8 scale: 2{};

    auto raw() const -> u8 {
        return (scale << 6) | (index << 3) | base;
    }
};

} // namespace fiska::x86


#endif // __X86_ASSEMBLER_LIB_X86_COMMON_HH__
