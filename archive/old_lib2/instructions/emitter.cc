#include "lib/instructions/emitter.hh"

#include "lib/core.hh"
#include "lib/instructions/mov.hh"
#include "lib/instructions/add.hh"
#include "lib/instructions/syscall.hh"

auto fiska::x86::instructions::emit(X86Instruction::Ref i) -> InstructionBuf {
    using enum X86IK;
    using namespace patterns;

    switch (i.kind_) {
    case Invalid: unreachable("Invalid instruction encountered.");
    case Mov: return mov::emit(i.op_list_);
    case Add: return add::emit(i.op_list_);
    case Adc: return adc::emit(i.op_list_);
    case Syscall: return syscall::emit(i.op_list_);
    } // switch
    unreachable();
}
