#ifndef __X86_ASSEMBLER_LIB_INSTRUCTIONS_SYSCALL_HH__
#define __X86_ASSEMBLER_LIB_INSTRUCTIONS_SYSCALL_HH__

#include "lib/core.hh"
#include "lib/x86_assembler.hh"

namespace fiska::x86::instructions {

using syscall = InstrExprList<
    X86IK::Syscall,

    // 0x0f 0x05 SYSCALL -- ZO
    InstrExpr<
        OpCode<0x0f, 0x05>,
        Pat<>,
        Emitter<OpEn::ZO>
    >
>;

} // namespace fiska::x86::instructions

#endif // __X86_ASSEMBLER_LIB_INSTRUCTIONS_SYSCALL_HH__
