#ifndef __X86_ASSEMBLER_LIB_CODEGEN_INSTRS_EMITTER_HH__
#define __X86_ASSEMBLER_LIB_CODEGEN_INSTRS_EMITTER_HH__

#include "lib/support/core.hh"
#include "lib/codegen/ir.hh"
#include "lib/codegen/instrs/shard0.hh"

namespace fiska::x86::codegen::instrs {

auto emit(IRX86Instr::Ref ops) -> ByteVec;

}  // namespace fiska::x86::instrs

#endif // __X86_ASSEMBLER_LIB_CODEGEN_INSTRS_EMITTER_HH__
