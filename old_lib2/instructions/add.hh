#ifndef __X86_ASSEMBLER_LIB_INSTRUCTIONS_ADD_HH__
#define __X86_ASSEMBLER_LIB_INSTRUCTIONS_ADD_HH__

#include "lib/core.hh"
#include "lib/x86_assembler.hh"

namespace fiska::x86::instructions {

using namespace patterns;

using add = InstrExprList<
    X86IK::Add,

    // 0x04 ib ADD AL, imm8 -- I
    InstrExpr<
        OpCode<0x04>,
        Pat<Rex_W::No, B16OpSz::No, al, imm8>,
        Emitter<OpEn::I>
    >,
    // 0x05 iw ADD AX, imm16  -- I
    // 0x05 id ADD EAX, imm32 -- I
    // 0x05 id ADD RAX, imm32 -- I
    InstrExpr<
        OpCode<0x05>,
        Or<
            Pat<Rex_W::No, B16OpSz::Yes, ax, imm16>,
            Pat<Rex_W::No, B16OpSz::No, eax, imm32>,
            Pat<Rex_W::Yes, B16OpSz::No, rax, imm32>
        >,
        Emitter<OpEn::I>
    >,
    // 0x80 /0 ib ADD r/m8, imm8  -- MI
    InstrExpr<
        OpCode<0x80>,
        Pat<Rex_W::No, B16OpSz::No, rm8, imm8>,
        Emitter<OpEn::MI, SlashDigit::Zero>
    >,
    // 0x81 /0 iw ADD r/m16, imm16 -- MI
    // 0x81 /0 id ADD r/m32, imm32 -- MI
    // 0x81 /0 id ADD r/m64, imm32 -- MI
    InstrExpr<
        OpCode<0x81>,
        Or<
            Pat<Rex_W::No, B16OpSz::Yes, rm16, imm16>,
            Pat<Rex_W::No, B16OpSz::No, rm32, imm32>,
            Pat<Rex_W::Yes, B16OpSz::No, rm64, imm32>
        >,
        Emitter<OpEn::MI, SlashDigit::Zero>
    >,
    // 0x83 /0 ib ADD r/m16, imm8 -- MI
    // 0x83 /0 ib ADD r/m32, imm8 -- MI
    // 0x83 /0 ib ADD r/m64, imm8 -- MI
    InstrExpr<
        OpCode<0x83>,
        Or<
            Pat<Rex_W::No, B16OpSz::Yes, rm16, imm8>,
            Pat<Rex_W::No, B16OpSz::No, rm32, imm8>,
            Pat<Rex_W::Yes, B16OpSz::No, rm64, imm8>
        >,
        Emitter<OpEn::MI, SlashDigit::Zero>
    >,
    // 0x00 /r ADD r/m8, r8  -- MR
    InstrExpr<
        OpCode<0x00>,
        Pat<Rex_W::No, B16OpSz::No, rm8, r8>,
        Emitter<OpEn::MR>
    >,
    // 0x01 /r ADD r/m16, r16  -- MR
    // 0x01 /r ADD r/m32, r32  -- MR
    // 0x01 /r ADD r/m64, r64  -- MR
    InstrExpr<
        OpCode<0x01>,
        Or<
            Pat<Rex_W::No, B16OpSz::Yes, rm16, r16>,
            Pat<Rex_W::No, B16OpSz::No, rm32, r32>,
            Pat<Rex_W::Yes, B16OpSz::No, rm64, r64>
        >,
        Emitter<OpEn::MR>
    >,
    // 0x02 /r ADD r8, r/m8  -- RM
    InstrExpr<
        OpCode<0x02>,
        Pat<Rex_W::No, B16OpSz::No, rm8, r8>,
        Emitter<OpEn::MR>
    >,
    // 0x03 /r ADD r16, r/m16  -- RM
    // 0x03 /r ADD r32, r/m32  -- RM
    // 0x03 /r ADD r64, r/m64  -- RM
    InstrExpr<
        OpCode<0x03>,
        Or<
            Pat<Rex_W::No, B16OpSz::Yes, r16, rm16>,
            Pat<Rex_W::No, B16OpSz::No, r32, rm32>,
            Pat<Rex_W::Yes, B16OpSz::No, r64, rm64>
        >,
        Emitter<OpEn::RM>
    >
>;

using adc = InstrExprList<
    X86IK::Adc,

    // 0x14 ib ADD AL, imm8 -- I
    InstrExpr<
        OpCode<0x14>,
        Pat<Rex_W::No, B16OpSz::No, al, imm8>,
        Emitter<OpEn::I>
    >,
    // 0x15 iw ADD AX, imm16  -- I
    // 0x15 id ADD EAX, imm32 -- I
    // 0x15 id ADD RAX, imm32 -- I
    InstrExpr<
        OpCode<0x15>,
        Or<
            Pat<Rex_W::No, B16OpSz::Yes, ax, imm16>,
            Pat<Rex_W::No, B16OpSz::No, eax, imm32>,
            Pat<Rex_W::Yes, B16OpSz::No, rax, imm32>
        >,
        Emitter<OpEn::I>
    >,
    // 0x80 /2 ib ADC r/m8, imm8  -- MI
    InstrExpr<
        OpCode<0x80>,
        Pat<Rex_W::No, B16OpSz::No, rm8, imm8>,
        Emitter<OpEn::MI, SlashDigit::Two>
    >,
    // 0x81 /2 iw ADC r/m16, imm16 -- MI
    // 0x81 /2 id ADC r/m32, imm32 -- MI
    // 0x81 /2 id ADC r/m64, imm32 -- MI
    InstrExpr<
        OpCode<0x81>,
        Or<
            Pat<Rex_W::No, B16OpSz::Yes, rm16, imm16>,
            Pat<Rex_W::No, B16OpSz::No, rm32, imm32>,
            Pat<Rex_W::Yes, B16OpSz::No, rm64, imm32>
        >,
        Emitter<OpEn::MI, SlashDigit::Two>
    >,
    // 0x83 /2 ib ADC r/m16, imm8 -- MI
    // 0x83 /2 ib ADC r/m32, imm8 -- MI
    // 0x83 /2 ib ADC r/m64, imm8 -- MI
    InstrExpr<
        OpCode<0x83>,
        Or<
            Pat<Rex_W::No, B16OpSz::Yes, rm16, imm8>,
            Pat<Rex_W::No, B16OpSz::No, rm32, imm8>,
            Pat<Rex_W::Yes, B16OpSz::No, rm64, imm8>
        >,
        Emitter<OpEn::MI, SlashDigit::Two>
    >,
    // 0x10 /r ADC r/m8, r8  -- MR
    InstrExpr<
        OpCode<0x10>,
        Pat<Rex_W::No, B16OpSz::No, rm8, r8>,
        Emitter<OpEn::MR>
    >,
    // 0x11 /r ADD r/m16, r16  -- MR
    // 0x11 /r ADD r/m32, r32  -- MR
    // 0x11 /r ADD r/m64, r64  -- MR
    InstrExpr<
        OpCode<0x11>,
        Or<
            Pat<Rex_W::No, B16OpSz::Yes, rm16, r16>,
            Pat<Rex_W::No, B16OpSz::No, rm32, r32>,
            Pat<Rex_W::Yes, B16OpSz::No, rm64, r64>
        >,
        Emitter<OpEn::MR>
    >,
    // 0x12 /r ADC r8, r/m8  -- RM
    InstrExpr<
        OpCode<0x12>,
        Pat<Rex_W::No, B16OpSz::No, rm8, r8>,
        Emitter<OpEn::MR>
    >,
    // 0x13 /r ADC r16, r/m16  -- RM
    // 0x13 /r ADC r32, r/m32  -- RM
    // 0x13 /r ADC r64, r/m64  -- RM
    InstrExpr<
        OpCode<0x13>,
        Or<
            Pat<Rex_W::No, B16OpSz::Yes, r16, rm16>,
            Pat<Rex_W::No, B16OpSz::No, r32, rm32>,
            Pat<Rex_W::Yes, B16OpSz::No, r64, rm64>
        >,
        Emitter<OpEn::RM>
    >
>;

} // namespace fiska::x86::instructions

#endif // __X86_ASSEMBLER_LIB_INSTRUCTIONS_ADD_HH__
