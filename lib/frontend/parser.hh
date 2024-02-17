#ifndef __X86_ASSEMBLER_LIB_FRONTEND_PARSER_HH__
#define __X86_ASSEMBLER_LIB_FRONTEND_PARSER_HH__

#include "lib/common/base.hh"
#include "lib/frontend/ast.hh"

namespace fiska::assembler::frontend {

struct Parser {
    Ctx* ctx_{};
    u16 fid_{};
    i32 tok_idx_{};
    TokStream tok_stream_;

    explicit Parser(Ctx* ctx, u16 fid);
    static auto parse(Ctx* ctx, u16 fid) -> Vec<Expr>;
};


} // namespace fiska::assembler::frontend

#endif // __X86_ASSEMBLER_LIB_FRONTEND_PARSER_HH__
