#ifndef __X86_ASSEMBLER_LIB_CODEGEN_INSTRS_HH__
#define __X86_ASSEMBLER_LIB_CODEGEN_INTSRS_HH__

#include "lib/support/core.hh"
#include "lib/codegen/patterns.hh"
#include "lib/codegen/assembler.hh"

namespace fiska::x86::codegen::instrs {

using Mov = InstrExprList<
    X86Mnemonic::Mov,

    // 0x88 /r MOV r/m8, r8 -- MR
    InstrExpr<
        OpCode<0x88>,
        Pat<OpSz::Default, rm8, r8>,
        Emitter<OpEn::MR>
    >,
    // 0x89 /r MOV r/m16, r16 -- MR
    // 0x89 /r MOV r/m32, r32 -- MR
    // 0x89 /r MOV r/m64, r64 -- MR
    InstrExpr<
        OpCode<0x89>,
        Or<
            Pat<OpSz::B16, rm16, r16>,
            Pat<OpSz::Default, rm32, r32>,
            Pat<OpSz::B64, rm64, r64>
        >,
        Emitter<OpEn::MR>
    >,
    // 0x8A /r MOV r8, r/m8 -- RM
    InstrExpr<
        OpCode<0x8a>,
        Pat<OpSz::Default, r8, rm8>,
        Emitter<OpEn::RM>
    >,
    // 0x8B /r MOV r16, r/m16 -- RM
    // 0x8B /r MOV r32, r/m32 -- RM
    // 0x8B /r MOV r64, r/m64 -- RM
    InstrExpr<
        OpCode<0x8b>,
        Or<
            Pat<OpSz::B16, r16, rm16>,
            Pat<OpSz::Default, r32, rm32>,
            Pat<OpSz::B64, r64, rm64>
        >,
        Emitter<OpEn::RM>
    >,
    // 0x8C /r MOV r/m16, Sreg       -- MR
    // 0x8C /r MOV r16/r32/m16, Sreg -- MR
    // 0x8C /r MOV r64/m16, Seg      -- MR
    InstrExpr<
        OpCode<0x8c>,
        Or<
            Pat<OpSz::Default, Alt<m16, r32>, sreg>,
            Pat<OpSz::B16, r16, sreg>,
            // The reason |Rex_W| is set to no is to automatically demote this instruction
            // to 32-bits to avoid emitting the rex prefix.
            Pat<OpSz::Default, r64, sreg>
        >,
        Emitter<OpEn::MR>
    >,
    // 0x8E /r MOV Sreg, r/m16 -- RM
    // 0x8E /r MOV Sreg, r/m64 -- RM
    InstrExpr<
        OpCode<0x8e>,
        Or<
            Pat<OpSz::Default, sreg, m16>,
            // It seems like we don't need the 16-bit operand size override when
            // moving to a segment register. We do however emit it when moving to
            // a 16-bit GPR.
            Pat<OpSz::Default, sreg, r16>,
            // Remove 'MOV sreg, m64' because gas doens't support it even though
            // it's in the intel manual. It's useless anyways. 
            // The correct pattern is: 'Pat<OpSz::B64, sreg, rm64>'
            //
            // The reason |OpSz| is set to the value |Default| is to automatically demote this instruction
            // to 32-bits to avoid emitting the rex prefix. That's what gas does.
            Pat<OpSz::Default, sreg, r64>
        >,
        Emitter<OpEn::RM>
    >,
    // 0xA0 MOV AL, moffs8   -- FD
    InstrExpr<
        OpCode<0xa0>,
        Pat<OpSz::Default, al, moffs8>,
        Emitter<OpEn::FD>
    >,
    // 0xA1 MOV AX, moffs16  -- FD
    // 0xA1 MOV EAX, moffs32 -- FD
    // 0xA1 MOV RAX, moffs64 -- FD
    InstrExpr<
        OpCode<0xa1>,
        Or<
            Pat<OpSz::B16, ax, moffs16>,
            Pat<OpSz::Default, eax, moffs32>,
            Pat<OpSz::B64, rax, moffs64>
        >,
        Emitter<OpEn::FD>
    >,
    // 0xA2 MOV moffs8, AL -- TD
    InstrExpr<
        OpCode<0xa2>,
        Pat<OpSz::Default, moffs8, al>,
        Emitter<OpEn::TD>
    >,
    // 0xA3 MOV moffs16, AX  -- TD
    // 0xA3 MOV moffs32, EAX -- TD
    // 0xA3 MOV moffs64, RAX -- TD
    InstrExpr<
        OpCode<0xa3>,
        Or<
            Pat<OpSz::B16, moffs16, ax>,
            Pat<OpSz::Default, moffs32, eax>,
            Pat<OpSz::B64, moffs64, rax>
        >,
        Emitter<OpEn::TD>
    >,
    // 0xB0+ rb ib MOV r8, imm8 -- OI
    InstrExpr<
        OpCode<0xb0>,
        Pat<OpSz::Default, r8, imm8>,
        Emitter<OpEn::OI>
    >,
    // 0xB8+ rw iw MOV r16, imm16 -- OI
    // 0xB8+ rd id MOV r32, imm32 -- OI
    // 0xB8+ rd io MOV r64, imm64 -- OI
    InstrExpr<
        OpCode<0xb8>,
        Or<
            Pat<OpSz::B16, r16, imm16>,
            Pat<OpSz::Default, r32, imm32>,
            Pat<OpSz::B64, r64, imm64>
        >,
        Emitter<OpEn::OI>
    >,
    // 0xC6 /0 ib MOV r/m8, imm8 -- MI
    InstrExpr<
        OpCode<0xc6>,
        Pat<OpSz::Default, rm8, imm8>,
        Emitter<OpEn::MI, SlashDigit::Zero>
    >,
    // 0xC7 /0 iw MOV r/m16, imm16 -- MI
    // 0xC7 /0 id MOV r/m32, imm32 -- MI
    // 0xC7 /0 id MOV r/m64, imm32 -- MI
    InstrExpr<
        OpCode<0xc7>,
        Or<
            Pat<OpSz::B16, rm16, imm16>,
            Pat<OpSz::Default, rm32, imm32>,
            Pat<OpSz::B64, rm64, imm32>
        >,
        Emitter<OpEn::MI, SlashDigit::Zero>
    >,
    // 0x0F 0x20 /r MOV r64, CR0-CR7 -- MR
    // 0x0F 0x20 /0 MOV r64, CR8     -- MR
    InstrExpr<
        OpCode<0x0f, 0x20>,
        Pat<OpSz::Default, r64, cr>,
        Emitter<OpEn::MR>
    >,
    // 0x0F 0x22 /r MOV CR0-CR7, r64 -- RM
    // 0x0F 0x22 /0 MOV CR8, r64     -- RM
    InstrExpr<
        OpCode<0x0f, 0x22>,
        Pat<OpSz::Default, cr, r64>,
        Emitter<OpEn::RM>
    >,
    // 0x0F 0x21 /r MOV r64, DR0-DR7 -- MR 
    InstrExpr<
        OpCode<0x0f, 0x21>,
        Pat<OpSz::Default, r64, dbg>,
        Emitter<OpEn::MR>
    >,
    // 0x0F 0x23 /r MOV DR0-DR7, r64 -- RM
    InstrExpr<
        OpCode<0x0f, 0x23>,
        Pat<OpSz::Default, dbg, r64>,
        Emitter<OpEn::RM>
    >
>;

using Add = InstrExprList<
    X86Mnemonic::Add,

    // 0x04 ib ADD AL, imm8 -- I
    InstrExpr<
        OpCode<0x04>,
        Pat<OpSz::Default, al, imm8>,
        Emitter<OpEn::I>
    >,
    // 0x05 iw ADD AX, imm16  -- I
    // 0x05 id ADD EAX, imm32 -- I
    // 0x05 id ADD RAX, imm32 -- I
    InstrExpr<
        OpCode<0x05>,
        Or<
            Pat<OpSz::B16, ax, imm16>,
            Pat<OpSz::Default, eax, imm32>,
            Pat<OpSz::B64, rax, imm32>
        >,
        Emitter<OpEn::I>
    >,
    // 0x80 /0 ib ADD r/m8, imm8  -- MI
    InstrExpr<
        OpCode<0x80>,
        Pat<OpSz::Default, rm8, imm8>,
        Emitter<OpEn::MI, SlashDigit::Zero>
    >,
    // 0x81 /0 iw ADD r/m16, imm16 -- MI
    // 0x81 /0 id ADD r/m32, imm32 -- MI
    // 0x81 /0 id ADD r/m64, imm32 -- MI
    InstrExpr<
        OpCode<0x81>,
        Or<
            Pat<OpSz::B16, rm16, imm16>,
            Pat<OpSz::Default, rm32, imm32>,
            Pat<OpSz::B64, rm64, imm32>
        >,
        Emitter<OpEn::MI, SlashDigit::Zero>
    >,
    // 0x83 /0 ib ADD r/m16, imm8 -- MI
    // 0x83 /0 ib ADD r/m32, imm8 -- MI
    // 0x83 /0 ib ADD r/m64, imm8 -- MI
    InstrExpr<
        OpCode<0x83>,
        Or<
            Pat<OpSz::B16, rm16, imm8>,
            Pat<OpSz::Default, rm32, imm8>,
            Pat<OpSz::B64, rm64, imm8>
        >,
        Emitter<OpEn::MI, SlashDigit::Zero>
    >,
    // 0x00 /r ADD r/m8, r8  -- MR
    InstrExpr<
        OpCode<0x00>,
        Pat<OpSz::Default, rm8, r8>,
        Emitter<OpEn::MR>
    >,
    // 0x01 /r ADD r/m16, r16  -- MR
    // 0x01 /r ADD r/m32, r32  -- MR
    // 0x01 /r ADD r/m64, r64  -- MR
    InstrExpr<
        OpCode<0x01>,
        Or<
            Pat<OpSz::B16, rm16, r16>,
            Pat<OpSz::Default, rm32, r32>,
            Pat<OpSz::B64, rm64, r64>
        >,
        Emitter<OpEn::MR>
    >,
    // 0x02 /r ADD r8, r/m8  -- RM
    InstrExpr<
        OpCode<0x02>,
        Pat<OpSz::Default, rm8, r8>,
        Emitter<OpEn::MR>
    >,
    // 0x03 /r ADD r16, r/m16  -- RM
    // 0x03 /r ADD r32, r/m32  -- RM
    // 0x03 /r ADD r64, r/m64  -- RM
    InstrExpr<
        OpCode<0x03>,
        Or<
            Pat<OpSz::B16, r16, rm16>,
            Pat<OpSz::Default, r32, rm32>,
            Pat<OpSz::B64, r64, rm64>
        >,
        Emitter<OpEn::RM>
    >
>;

using Adc = InstrExprList<
    X86Mnemonic::Adc,

    // 0x14 ib ADD AL, imm8 -- I
    InstrExpr<
        OpCode<0x14>,
        Pat<OpSz::Default, al, imm8>,
        Emitter<OpEn::I>
    >,
    // 0x15 iw ADD AX, imm16  -- I
    // 0x15 id ADD EAX, imm32 -- I
    // 0x15 id ADD RAX, imm32 -- I
    InstrExpr<
        OpCode<0x15>,
        Or<
            Pat<OpSz::B16, ax, imm16>,
            Pat<OpSz::Default, eax, imm32>,
            Pat<OpSz::B64, rax, imm32>
        >,
        Emitter<OpEn::I>
    >,
    // 0x80 /2 ib ADC r/m8, imm8  -- MI
    InstrExpr<
        OpCode<0x80>,
        Pat<OpSz::Default, rm8, imm8>,
        Emitter<OpEn::MI, SlashDigit::Two>
    >,
    // 0x81 /2 iw ADC r/m16, imm16 -- MI
    // 0x81 /2 id ADC r/m32, imm32 -- MI
    // 0x81 /2 id ADC r/m64, imm32 -- MI
    InstrExpr<
        OpCode<0x81>,
        Or<
            Pat<OpSz::B16, rm16, imm16>,
            Pat<OpSz::Default, rm32, imm32>,
            Pat<OpSz::B64, rm64, imm32>
        >,
        Emitter<OpEn::MI, SlashDigit::Two>
    >,
    // 0x83 /2 ib ADC r/m16, imm8 -- MI
    // 0x83 /2 ib ADC r/m32, imm8 -- MI
    // 0x83 /2 ib ADC r/m64, imm8 -- MI
    InstrExpr<
        OpCode<0x83>,
        Or<
            Pat<OpSz::B16, rm16, imm8>,
            Pat<OpSz::Default, rm32, imm8>,
            Pat<OpSz::B64, rm64, imm8>
        >,
        Emitter<OpEn::MI, SlashDigit::Two>
    >,
    // 0x10 /r ADC r/m8, r8  -- MR
    InstrExpr<
        OpCode<0x10>,
        Pat<OpSz::Default, rm8, r8>,
        Emitter<OpEn::MR>
    >,
    // 0x11 /r ADD r/m16, r16  -- MR
    // 0x11 /r ADD r/m32, r32  -- MR
    // 0x11 /r ADD r/m64, r64  -- MR
    InstrExpr<
        OpCode<0x11>,
        Or<
            Pat<OpSz::B16, rm16, r16>,
            Pat<OpSz::Default, rm32, r32>,
            Pat<OpSz::B64, rm64, r64>
        >,
        Emitter<OpEn::MR>
    >,
    // 0x12 /r ADC r8, r/m8  -- RM
    InstrExpr<
        OpCode<0x12>,
        Pat<OpSz::Default, rm8, r8>,
        Emitter<OpEn::MR>
    >,
    // 0x13 /r ADC r16, r/m16  -- RM
    // 0x13 /r ADC r32, r/m32  -- RM
    // 0x13 /r ADC r64, r/m64  -- RM
    InstrExpr<
        OpCode<0x13>,
        Or<
            Pat<OpSz::B16, r16, rm16>,
            Pat<OpSz::Default, r32, rm32>,
            Pat<OpSz::B64, r64, rm64>
        >,
        Emitter<OpEn::RM>
    >
>;

using adc = InstrExprList<
    X86Mnemonic::Adc,

    // 0x14 ib ADD AL, imm8 -- I
    InstrExpr<
        OpCode<0x14>,
        Pat<OpSz::Default, al, imm8>,
        Emitter<OpEn::I>
    >,
    // 0x15 iw ADD AX, imm16  -- I
    // 0x15 id ADD EAX, imm32 -- I
    // 0x15 id ADD RAX, imm32 -- I
    InstrExpr<
        OpCode<0x15>,
        Or<
            Pat<OpSz::B16, ax, imm16>,
            Pat<OpSz::Default, eax, imm32>,
            Pat<OpSz::B64, rax, imm32>
        >,
        Emitter<OpEn::I>
    >,
    // 0x80 /2 ib ADC r/m8, imm8  -- MI
    InstrExpr<
        OpCode<0x80>,
        Pat<OpSz::Default, rm8, imm8>,
        Emitter<OpEn::MI, SlashDigit::Two>
    >,
    // 0x81 /2 iw ADC r/m16, imm16 -- MI
    // 0x81 /2 id ADC r/m32, imm32 -- MI
    // 0x81 /2 id ADC r/m64, imm32 -- MI
    InstrExpr<
        OpCode<0x81>,
        Or<
            Pat<OpSz::B16, rm16, imm16>,
            Pat<OpSz::Default, rm32, imm32>,
            Pat<OpSz::B64, rm64, imm32>
        >,
        Emitter<OpEn::MI, SlashDigit::Two>
    >,
    // 0x83 /2 ib ADC r/m16, imm8 -- MI
    // 0x83 /2 ib ADC r/m32, imm8 -- MI
    // 0x83 /2 ib ADC r/m64, imm8 -- MI
    InstrExpr<
        OpCode<0x83>,
        Or<
            Pat<OpSz::B16, rm16, imm8>,
            Pat<OpSz::Default, rm32, imm8>,
            Pat<OpSz::B64, rm64, imm8>
        >,
        Emitter<OpEn::MI, SlashDigit::Two>
    >,
    // 0x10 /r ADC r/m8, r8  -- MR
    InstrExpr<
        OpCode<0x10>,
        Pat<OpSz::Default, rm8, r8>,
        Emitter<OpEn::MR>
    >,
    // 0x11 /r ADD r/m16, r16  -- MR
    // 0x11 /r ADD r/m32, r32  -- MR
    // 0x11 /r ADD r/m64, r64  -- MR
    InstrExpr<
        OpCode<0x11>,
        Or<
            Pat<OpSz::B16, rm16, r16>,
            Pat<OpSz::Default, rm32, r32>,
            Pat<OpSz::B64, rm64, r64>
        >,
        Emitter<OpEn::MR>
    >,
    // 0x12 /r ADC r8, r/m8  -- RM
    InstrExpr<
        OpCode<0x12>,
        Pat<OpSz::Default, rm8, r8>,
        Emitter<OpEn::MR>
    >,
    // 0x13 /r ADC r16, r/m16  -- RM
    // 0x13 /r ADC r32, r/m32  -- RM
    // 0x13 /r ADC r64, r/m64  -- RM
    InstrExpr<
        OpCode<0x13>,
        Or<
            Pat<OpSz::B16, r16, rm16>,
            Pat<OpSz::Default, r32, rm32>,
            Pat<OpSz::B64, r64, rm64>
        >,
        Emitter<OpEn::RM>
    >
>;

using Syscall = InstrExprList<
    X86Mnemonic::Syscall,

    // 0x0f 0x05 SYSCALL -- ZO
    InstrExpr<
        OpCode<0x0f, 0x05>,
        Pat<>,
        Emitter<OpEn::ZO>
    >
>;

} // namespace fiska::x86::instrs

#endif // __X86_ASSEMBLER_LIB_CODEGEN_INSTRS_HH__
