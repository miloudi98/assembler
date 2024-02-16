#ifndef __X86_ASSEMBLER_LIB_FRONTEND_LEXER_HH__
#define __X86_ASSEMBLER_LIB_FRONTEND_LEXER_HH__

#include "lib/common/base.hh"
#include "lib/frontend/token.hh"

namespace fiska::assembler::frontend {

struct Lexer {
    Ctx* ctx_{};
    u16 fid_{};
    const char* cur_{};
    const char* end_{};
    char c_{};
    TokStream tok_stream_;

    explicit Lexer(Ctx* ctx, u16 fid);
    // Lex the file identified by |fid_|.
    auto lex() -> TokStream;

    static const utils::StringMap<TK> kKeywords;
};

} // namespace fiska::assembler::frontend

#endif // __X86_ASSEMBLER_LIB_FRONTEND_LEXER_HH__
