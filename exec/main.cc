#include "lib/common/base.hh"
#include "lib/common/support.hh"
#include "lib/frontend/lexer.hh"

using namespace fiska::assembler;

constexpr StrRef test_file = "/home/jawad/fiska/dev/x86_assembler/tests/mov.fiska";

auto main(i32 argc, char* argv[]) -> i32 {
    auto ctx = std::make_unique<Ctx>();

    File* file = ctx->load_file(fs::path(test_file));

    frontend::Lexer lxr(ctx.get(), file->fid_);
    frontend::TokStream ts = lxr.lex();

    for (const auto& tok : ts.storage_) {
        fmt::print("tok.kind = {}\n", tok.kind_);
    }
    return 0;
}
