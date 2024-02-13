#include "lib/support/core.hh"
#include "lib/front_end/lexer.hh"
#include "lib/front_end/ctx.hh"
#include "lib/support/fe_utils.hh"

using namespace fiska::x86;
using namespace fiska::x86::fe;

constexpr StrRef test_file = "/home/jawad/fiska/dev/x86_assembler/tests/mov.fiska";

auto main(i32 argc, char* argv[]) -> i32 {
    auto ctx = std::make_unique<fe::Ctx>();
    File* file = ctx->load_file(fs::path(test_file));

    // Print the file's content.
    fmt::print("================ FILE START ==================\n");
    fmt::print("{}", StrRef{file->data(), file->size()});
    fmt::print("================ FILE END ==================\n");

    Lexer lxr{ctx.get(), file->fid_};
    while (lxr.tok().kind_ != TK::Eof) { lxr.next_tok(); }


    for (const Tok& tok : ctx->tok_streams_[file->fid_]) {

        ErrorSpan err = ErrorSpan::from(ctx.get(), tok.loc_);
        ErrorSpan::print(err);
        fmt::print("\n");
    }

    return 0;
} 
