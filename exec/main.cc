#include "lib/core.hh"
#include "lib/x86_core.hh"
#include "lib/front_end.hh"

using namespace fiska;

constexpr std::string_view file_name = "/home/jawad/fiska/dev/x86_assembler/tests/mov.fiska";

auto main(i32 argc, char* argv[]) -> i32 {
    auto ctx = std::make_unique<Context>();
    auto mod = std::make_unique<Module>();
    mod->ctx_ = ctx.get();

    u16 fid = ctx->load_file(fs::path(file_name));

    Lexer::lex_file_into_module(ctx->get_file(fid), mod.get());

    return 0;
}
