#ifndef __X86_ASSEMBLER_LIB_BACKEND_CODEGEN_OBJECT_HH__
#define __X86_ASSEMBLER_LIB_BACKEND_CODEGEN_OBJECT_HH__

#include "lib/common/base.hh"

namespace fiska::assembler::backend {

struct IRSymbol;
auto gen(const Vec<IRSymbol>& ir_symbols) -> ByteVec;

} // namespace fiska::assembler::backend

#endif // __X86_ASSEMBLER_LIB_BACKEND_CODEGEN_OBJECT_HH__
