#ifndef __X86_ASSEMBLER_LIB_X86_COMMON_HH__
#define __X86_ASSEMBLER_LIB_X86_COMMON_HH__

#include "lib/core.hh"

namespace fiska::x86 {

// Bit width.
enum struct BW : i8 {
    Invalid = -1,

    B0 = 0,
    B8 = 8,
    B16 = 16,
    B24 = 24,
    B32 = 32,
    B64 = 64
};

// X86 Instruction mnemonic.
enum struct X86Mnemonic {
    Invalid,

    Mov, Add, Adc, Syscall,
};

}  // namespace fiska::x86

#endif // __X86_ASSEMBLER_LIB_X86_COMMON_HH__
