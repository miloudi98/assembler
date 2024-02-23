#ifndef __X86_ASSEMBLER_LIB_COMMON_CTX_HH__
#define __X86_ASSEMBLER_LIB_COMMON_CTX_HH__

#include "lib/common/base.hh"
#include "lib/common/support.hh"

namespace fiska::assembler::frontend {

struct Expr;

struct Ctx {
    NOT_COPYABLE_NOT_MOVABLE(Ctx);

    // Files loaded from disk.
    Vec<Box<File>> files_;
    // Interned string pool.
    StringInterner string_pool_;
    // All AST nodes allocated.
    Vec<Expr*> exprs_;

    Ctx() = default;
    // Destroy all the AST nodes allocated.
    ~Ctx();
    // Load file to memory.
    auto load_file(const fs::path& path) -> File*;
    // Return the file with fid |fid|.
    auto read_file(u16 fid) -> File*;
};

// Error Handling types and utility functions.
struct UnexpectedToken {
    static constexpr StrRef fmt_string = "Unexpected token: '{}', found: '{}'.";
};

struct InvalidNumber {
    static constexpr StrRef fmt_string = "Invalid number encountered: '{}'.";
};

struct InvalidStartOfExpr {
    static constexpr StrRef fmt_string = "Token '{}' does not start a valid expression.";
};

template <typename ErrorKind>
struct Diag {
    template <typename... Args>
    [[noreturn]] Diag(Ctx* ctx, Span span, Args&&... args) {
        unreachable(ErrorKind::fmt_string, FWD(args)...);
    }
};


} // namespace fiska::assembler

#endif // __X86_ASSEMBLER_LIB_COMMON_CTX_HH__
