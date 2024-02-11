#include "lib/codegen/instrs/emitter.hh"
#include "lib/support/core.hh"

auto fiska::x86::codegen::instrs::emit(IRX86Instr::Ref i) -> ByteVec {

#define CASE(mmic) case X86Mnemonic::mmic: return mmic::emit(i.ops_)

    switch (i.mmic_) {
    CASE(Mov);
    CASE(Adc);
    CASE(Add);
    CASE(Syscall);

    default: unreachable("unsupported instruction.");
    } // switch

#undef CASE
}
