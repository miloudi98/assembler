#ifndef __X86_ASSEMBLER_LIB_BACKEND_CODEGEN_OBJECT_HH__
#define __X86_ASSEMBLER_LIB_BACKEND_CODEGEN_OBJECT_HH__

#include "lib/common/base.hh"

namespace fiska::assembler::backend {

struct IRSymbol;
auto gen(const Vec<IRSymbol>& ir_symbols, const fs::path& out_path) -> void;

} // namespace fiska::assembler::backend

#endif // __X86_ASSEMBLER_LIB_BACKEND_CODEGEN_OBJECT_HH__
