#ifndef __X86_ASSEMBLER_LIB_FRONTEND_LEXER_HH__
#define __X86_ASSEMBLER_LIB_FRONTEND_LEXER_HH__

#include "lib/common/base.hh"
#include "lib/frontend/token.hh"

namespace fiska::assembler::frontend {

auto lex(Ctx* ctx, u16 fid) -> TokStream;

} // namespace fiska::assembler::frontend

#endif // __X86_ASSEMBLER_LIB_FRONTEND_LEXER_HH__
