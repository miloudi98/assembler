#ifndef __X86_ASSEMBLER_LIB_INSTRUCTIONS_EMITTER_HH__
#define __X86_ASSEMBLER_LIB_INSTRUCTIONS_EMITTER_HH__

#include "lib/core.hh"
#include "lib/instructions/mov.hh"
#include "lib/instructions/add.hh"

namespace fiska::x86::instructions {

using namespace patterns;

auto emit(X86Instruction::Ref inst) -> InstructionBuf {
    using enum X86IK;

    switch (inst.kind_) {
    case Mov: return mov::emit(inst.op_list_);
    case Add: return add::emit(inst.op_list_);
    case Adc: return adc::emit(inst.op_list_);
    } // switch
    unreachable();
}

} // namespace fiska::x86::instructions

#endif // __X86_ASSEMBLER_LIB_INSTRUCTIONS_EMITTER_HH__
