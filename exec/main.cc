#include "lib/common/base.hh"
#include "lib/frontend/parser.hh"
#include "lib/frontend/ctx.hh"
#include "lib/frontend/ast.hh"

using namespace fiska::assembler;
using namespace fiska::assembler::frontend;

constexpr StrRef filename = "/home/jawad/fiska/dev/x86_assembler/tests/mov.fiska";

auto main(i32 argc, char* argv[]) -> i32 {
    auto ctx = std::make_unique<Ctx>();
    auto file = ctx->load_file(fs::path(filename));
    auto sections = parse(ctx.get(), file->fid_);

    fmt::print("Success\n");
    return 0;
}
