#include "lib/core.hh"
#include "lib/x86_patterns.hh"
#include "lib/x86_utils.hh"
#include "lib/x86_assembler.hh"
#include "lib/testing/elf.hh"
#include "lib/instructions/emitter.hh"

#include <cstring>

using namespace fiska::x86;

constexpr StrRef test_out_path = "/home/jawad/fiska/dev/x86_assembler/lib/instructions/test_x86_instructions";

auto main(i32 argc, char* argv[]) -> i32 {
    AsCtx::init();

    //elf::build_testing_asm_and_elf_file<
    //    instructions::mov,
    //    instructions::add,
    //    instructions::adc
    //>(fs::path(test_out_path));

    auto x86_instructions_raw = utils::load_file(fs::path(fmt::format("{}.cpp.x86.instructions", test_out_path)));
    auto elf_file = utils::load_file(fs::path(fmt::format("{}.elf", test_out_path)));

    u64 x86_instructions_sz = x86_instructions_raw.size() / sizeof(X86Instruction);
    auto x86_instructions = reinterpret_cast<const X86Instruction*>(x86_instructions_raw.data());
    auto as_encodings = elf::read_symbols_from_elf({elf_file.data(), elf_file.size()});
    
    auto t1 = chr::high_resolution_clock::now();
    assert(x86_instructions_sz == as_encodings.size());
    for (u64 instr_idx{}; const InstructionBuf& as_encoding : as_encodings) {
        InstructionBuf fiska_encoding = instructions::emit(x86_instructions[instr_idx++]);
        assert(fiska_encoding == as_encoding);
    }
    auto t2 = chr::high_resolution_clock::now();

    fmt::print("took {}ms\n", chr::duration_cast<chr::milliseconds>(t2 - t1).count());
    fmt::print("{} tests passed successfuly!\n", x86_instructions_sz);
}
