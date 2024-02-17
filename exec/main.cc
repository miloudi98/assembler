#include "lib/common/base.hh"
#include "lib/common/support.hh"
#include "lib/frontend/lexer.hh"
#include "lib/frontend/parser.hh"
#include "lib/frontend/sema.hh"

using namespace fiska::assembler;

constexpr StrRef test_file = "/home/jawad/fiska/dev/x86_assembler/tests/mov.fiska";

auto main(i32 argc, char* argv[]) -> i32 {
    auto ctx = std::make_unique<Ctx>();

    File* file = ctx->load_file(fs::path(test_file));

    auto ast = frontend::parse(ctx.get(), file->fid_);

    frontend::SemaDone token = frontend::analyze(ctx.get(), ast);

    return 0;
}
