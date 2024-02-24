#ifndef __X86_ASSEMBLER_LIB_FRONTEND_SEMA_HH__
#define __X86_ASSEMBLER_LIB_FRONTEND_SEMA_HH__

#include "lib/common/base.hh"

namespace fiska::assembler::frontend {

struct Ctx;
struct Section;
auto analyze(Ctx* ctx, const Vec<Section>& ast) -> void;

} // namespace fiska::assembler::frontend

#endif // __X86_ASSEMBLER_LIB_FRONTEND_SEMA_HH__
