#ifndef __X86_ASSEMBLER_LIB_INSTRUCTIONS_EMITTER_HH__
#define __X86_ASSEMBLER_LIB_INSTRUCTIONS_EMITTER_HH__

#include "lib/core.hh"
#include "lib/instructions/mov.hh"
#include "lib/instructions/add.hh"

namespace fiska::x86::instructions {

auto emit(X86Instruction::Ref inst) -> InstructionBuf;

} // namespace fiska::x86::instructions

#endif // __X86_ASSEMBLER_LIB_INSTRUCTIONS_EMITTER_HH__
