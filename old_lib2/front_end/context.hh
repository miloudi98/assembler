#ifndef __X86_ASSEMBLER_LIB_FRONT_END_CONTEXT_HH__
#define __X86_ASSEMBLER_LIB_FRONT_END_CONTEXT_HH__

#include "lib/core.hh"
#include "lib/front_end/lexer.hh"
#include "lib/front_end/support.hh"
#include "lib/front_end/parser.hh"

namespace fiska::x86::fe {

// Global context for the entire front end.
struct Ctx {
    // List of files loaded.
    Vec<Box<File>> files_{};
    // Tokens of each file loaded.
    Vec<TokStream> tok_streams_{};
    // Ast nodes allocated.
    Vec<Expr*> ast_{};
    // Pool of interned strings.
    StringInterner str_pool_{};

    // Dealloacte all ast nodes.
    ~Ctx();
    // Load file to memory.
    auto load_file(const fs::path& path) -> File*;
    // Return the file with fid |fid|.
    auto get_file(u16 fid) -> File*;
};

} // namespace fiska::x86::fe

#endif // __X86_ASSEMBLER_LIB_FRONT_END_CONTEXT_HH__
