#include "lib/backend/x86instructions/assembler.hh"
#include "lib/backend/codegen/emitters.hh"
#include "lib/backend/ir.hh"
#include "lib/common/base.hh"
#include "lib/common/x86/types.hh"

// Bring all instructions to scope.
namespace fiska::assembler::backend {
#include "lib/backend/x86instructions/x86-instructions-shard-0.hh"
} // namespace fiska::assembler::backend


namespace fiska::assembler::backend {
#define CASE(mnemonic) case X86Mnemonic::mnemonic: return mnemonic::emit(i.ops_)

auto assemble(const IRX86Instr& i) -> X86ByteCode {
    switch (i.mnemonic_) {
        CASE(Mov);
        CASE(Add);
        CASE(Adc);
        CASE(Syscall);
        CASE(And);

        default: unreachable("Instruction not supported.");
    }
}

} // namespace fiska::assembler::backend
