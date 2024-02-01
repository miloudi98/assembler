//#include "lib/core.hh"
//#include "lib/x86_core.hh"
//#include "lib/front_end.hh"
//#include "lib/elf.hh"

#include "lib/core.hh"
#include "lib/x86_patterns.hh"
#include "lib/x86_utils.hh"
#include "lib/x86_assembler.hh"
#include "lib/instructions/mov.hh"
#include "lib/testing/elf.hh"

#include <cstring>

using namespace fiska::x86;

auto main(i32 argc, char* argv[]) -> i32 {
    AsCtx::init();
    using enum BW;

    Vec<X86Op::List> insts = instructions::mov::instances();

    X86Instruction::List x86_instructions;
    x86_instructions.reserve(insts.size());

    for (X86Op::ListRef op_list : insts) {
        x86_instructions.push_back(X86Instruction{X86IK::Mov, op_list});
    }

    auto encodings = elf::assemble_instructions_with_gas(x86_instructions);

    for (u64 instr_idx{}; const auto& encoding : encodings) {
        bool wrong_assembly = false;

        InstructionBuf fiska_encoding = instructions::mov::emit(x86_instructions[instr_idx++].op_list);

        wrong_assembly |= fiska_encoding.sz_ != encoding.size();

        if (fiska_encoding.sz_ == encoding.size()) {
            wrong_assembly |= std::memcmp(fiska_encoding.buf_, encoding.data(), encoding.size()) != 0;
        }

        

        if (wrong_assembly) {

            fmt::print("Wrong assembly generated for instr_idx = {}\n", instr_idx);

            fmt::print("[");
            for (auto [idx, byte] : encoding | vws::enumerate) {
                fmt::print("{:#04x}", byte);
                fmt::print("{}", u32(idx) == encoding.size() - 1 ? "" : ", ");
            }
            fmt::print("]");

            fmt::print("; fiska encoding = ");
            fmt::print("[");
            for (u8 idx = 0; idx < fiska_encoding.sz_; ++idx) {
                fmt::print("{:#04x}", fiska_encoding.buf_[idx]);
                fmt::print("{}", u32(idx) == fiska_encoding.sz_ - 1 ? "" : ", ");
            }
            fmt::print("]\n");
            std::exit(1);

        }

    }

    return 0;
}
