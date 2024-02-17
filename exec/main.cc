#include "lib/common/base.hh"
#include "lib/common/support.hh"
#include "lib/frontend/lexer.hh"
#include "lib/frontend/parser.hh"

using namespace fiska::assembler;

constexpr StrRef test_file = "/home/jawad/fiska/dev/x86_assembler/tests/mov.fiska";

auto main(i32 argc, char* argv[]) -> i32 {
    auto ctx = std::make_unique<Ctx>();

    File* file = ctx->load_file(fs::path(test_file));

    frontend::Parser::parse(ctx.get(), file->fid_);
    //frontend::Lexer lxr(ctx.get(), file->fid_);
    //frontend::TokStream ts = frontend::Lexer::lex(ctx.get(), file->fid_);

    //for (const auto& tok : ts.storage_) {
    //    fmt::print("tok.kind = {}\n", tok.kind_);
    //}

    return 0;
}
