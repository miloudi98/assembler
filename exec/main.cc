#include "lib/core.hh"
#include "lib/x86_core.hh"
#include "lib/front_end.hh"
#include "lib/elf.hh"

using namespace fiska;

//constexpr std::string_view file_name = "/home/jawad/fiska/dev/x86_assembler/tests/mov.fiska";

//auto main(i32 argc, char* argv[]) -> i32 {
//    auto ctx = std::make_unique<Ctx>();
//    auto mod = std::make_unique<Module>();
//    mod->name_ = "main_mod";
//    mod->ctx_ = ctx.get();
//
//    u16 fid = ctx->load_file(fs::path(file_name));
//
//    Lexer::lex_file_into_module(ctx->get_file(fid), mod.get());
//
//    //for (auto t : mod->tokens_.storage_) {
//    //    fmt::print("<{}>\n", Tok::spelling(t.kind_));
//    //}
//
//    Parser::parse_file_into_module(ctx->get_file(fid), mod.get());
//
//    for (Expr* expr : mod->top_level_exprs_) {
//        auto proc_expr = static_cast<ProcExpr*>(expr);
//        for (X86Instruction* instr : proc_expr->instructions_) {
//            fmt::print("{}\n", translate_x86_instr_to_gas_syntax(instr));
//        }
//    }
//
//    auto proc_expr = static_cast<ProcExpr*>(mod->top_level_exprs_[0]);
//
//    auto m = get_encoding_of(ctx.get(), proc_expr->instructions_);
//
//    for (const auto& [k, v] : m) {
//        fmt::print("name = {}, ", k);
//
//        fmt::print("[");
//        for (u8 byte : v) {
//            fmt::print("{:#04x}, ", byte);
//        }
//        fmt::print("]\n");
//    }
//
//
//    ModulePrinter::print_module(mod.get());
//
//    return 0;
//}

auto main(i32 argc, char* argv[]) -> i32 {
    using enum BW;
    run_global_tests();
    return 0;
}
