#ifndef __X86_ASSEMBLER_LIB_CODEGEN_INSTRS_EMITTER_HH__
#define __X86_ASSEMBLER_LIB_CODEGEN_INSTRS_EMITTER_HH__

#include "lib/support/core.hh"
#include "lib/codegen/patterns.hh"
#include "lib/codegen/instrs/shard0.hh"

namespace fiska::x86::codegen::instrs::emitter {

auto emit(IRX86Instr::Ref i) -> ByteVec;

}  // namespace fiska::x86::instrs

#endif // __X86_ASSEMBLER_LIB_CODEGEN_INSTRS_EMITTER_HH__
