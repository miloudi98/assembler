#ifndef __X86_ASSEMBLER_LIB_FRONTEND_PARSER_HH__
#define __X86_ASSEMBLER_LIB_FRONTEND_PARSER_HH__

#include "lib/common/base.hh"

namespace fiska::assembler::frontend {

struct Ctx;
struct Section;

auto parse(Ctx*, u16 fid) -> Vec<Section>;

} // namespace fiska::assembler::frontend

#endif // __X86_ASSEMBLER_LIB_FRONTEND_PARSER_HH__
