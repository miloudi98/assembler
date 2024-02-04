#include "lib/instructions/emitter.hh"

#include "lib/core.hh"

auto fiska::x86::instructions::emit(X86Instruction::Ref i) -> InstructionBuf {
    using enum X86IK;
    using namespace patterns;

    switch (i.kind_) {
    case Mov: return mov::emit(i.op_list_);
    case Add: return add::emit(i.op_list_);
    case Adc: return adc::emit(i.op_list_);
    } // switch
    unreachable();
}
