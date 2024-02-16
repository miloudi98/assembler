#ifndef __X86_ASSEMBLER_LIB_FRONT_END_CTX2_HH__
#define __X86_ASSEMBLER_LIB_FRONT_END_CTX2_HH__

#include "lib/support/core.hh"

namespace fiska::frontend {

namespace fiska::x86::fe {
struct TokStream;
struct Expr;
}

namespace fiska::x86 {
struct StringInterner;
struct File;
}

struct Ctx2 {
    // List of files loaded.
    Vec<Box<fiska::x86::File>> files_;
    // Tokens of each file loaded.
    Vec<fiska::x86::fe::TokStream> tok_streams_;
    // Ast nodes allocated.
    Vec<fiska::x86::fe::Expr*> ast_;
    // Pool of interned strings.
    Box<fiska::x86::StringInterner> str_pool_;
};

}

#endif
