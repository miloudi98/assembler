#ifndef __X86_ASSEMBLER_LIB_BACKEND_X86INSTRUCTIONS_ASSEMBLER_HH__
#define __X86_ASSEMBLER_LIB_BACKEND_X86INSTRUCTIONS_ASSEMBLER_HH__

#include "lib/common/base.hh"

namespace fiska::assembler::backend {

struct IRX86Instr;
struct X86ByteCode;

auto assemble(const IRX86Instr& i) -> X86ByteCode;

} // namespace fiska::assembler::backend

#endif // __X86_ASSEMBLER_LIB_BACKEND_X86INSTRUCTIONS_ASSEMBLER_HH__
