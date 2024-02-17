#ifndef __X86_ASSEMBLER_LIB_FRONTEND_PARSER_HH__
#define __X86_ASSEMBLER_LIB_FRONTEND_PARSER_HH__

#include "lib/common/base.hh"
#include "lib/frontend/ast.hh"

namespace fiska::assembler::frontend {

auto parse(Ctx*, u16 fid) -> Vec<Box<Expr>>;

} // namespace fiska::assembler::frontend

#endif // __X86_ASSEMBLER_LIB_FRONTEND_PARSER_HH__
