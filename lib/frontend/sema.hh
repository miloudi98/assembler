#ifndef __X86_ASSEMBLER_LIB_FRONTEND_SEMA_HH__
#define __X86_ASSEMBLER_LIB_FRONTEND_SEMA_HH__

#include "lib/common/base.hh"
#include "lib/frontend/ast.hh"

namespace fiska::assembler::frontend {

struct SemaDone {
    friend auto analyze(Ctx* ctx, const Vec<Box<Expr>>& ast) -> SemaDone;
private:
    SemaDone() = default;
};

auto analyze(Ctx* ctx, const Vec<Box<Expr>>& ast) -> SemaDone;

} // namespace fiska::assembler::frontend

#endif // __X86_ASSEMBLER_LIB_FRONTEND_SEMA_HH__
