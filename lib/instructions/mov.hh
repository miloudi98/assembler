#ifndef __X86_ASSEMBLER_LIB_INSTRUCTIONS_MOV_HH__
#define __X86_ASSEMBLER_LIB_INSTRUCTIONS_MOV_HH__

#include "lib/core.hh"
#include "lib/x86_assembler.hh"

namespace fiska::x86::instructions {

using namespace patterns;

using mov = InstrExprList<
    // 0x88 MOV r/m8, r8 -- MR
    InstrExpr<
        OpCode<0x88>,
        Pat<Rex_W::No, B16OpSz::No, rm8, r8>,
        Emitter<OpEn::MR>
    >,
    // 0x89 MOV r/m16, r16 -- MR
    // 0x89 MOV r/m32, r32 -- MR
    // 0x89 MOV r/m64, r64 -- MR
    InstrExpr<
        OpCode<0x89>,
        Or<
            Pat<Rex_W::No, B16OpSz::Yes, rm16, r16>,
            Pat<Rex_W::No, B16OpSz::No, rm32, r32>,
            Pat<Rex_W::Yes, B16OpSz::No, rm64, r64>
        >,
        Emitter<OpEn::MR>
    >,
    // 0x8A MOV r8, r/m8 -- RM
    InstrExpr<
        OpCode<0x8a>,
        Pat<Rex_W::No, B16OpSz::No, r8, rm8>,
        Emitter<OpEn::RM>
    >,
    // 0x8B MOV r16, r/m16 -- RM
    // 0x8B MOV r32, r/m32 -- RM
    // 0x8B MOV r64, r/m64 -- RM
    InstrExpr<
        OpCode<0x8b>,
        Or<
            Pat<Rex_W::No, B16OpSz::Yes, r16, rm16>,
            Pat<Rex_W::No, B16OpSz::No, r32, rm32>,
            Pat<Rex_W::Yes, B16OpSz::No, r64, rm64>
        >,
        Emitter<OpEn::RM>
    >,
    // 0x8C MOV r/m16, Sreg       -- MR
    // 0x8C MOV r16/r32/m16, Sreg -- MR
    // 0x8C MOV r64/m16, Seg      -- MR
    InstrExpr<
        OpCode<0x8c>,
        Or<
            Pat<Rex_W::No, B16OpSz::No, Alt<m16, r32>, sreg>,
            Pat<Rex_W::No, B16OpSz::Yes, r16, sreg>,
            // The reason |Rex_W| is set to no is to automatically demote this instruction
            // to 32-bits to avoid emitting the rex prefix.
            Pat<Rex_W::No, B16OpSz::No, r64, sreg>
        >,
        Emitter<OpEn::MR>
    >,
    // 0x8E MOV Sreg, r/m16 -- RM
    // 0x8E MOV Sreg, r/m64 -- RM
    InstrExpr<
        OpCode<0x8e>,
        Or<
            Pat<Rex_W::No, B16OpSz::No, sreg, m16>,
            // It seems like we don't need the 16-bit operand size override when
            // moving to a segment register. We do however emit it when moving to
            // a 16-bit GPR.
            Pat<Rex_W::No, B16OpSz::No, sreg, r16>,
            // Remove 'MOV sreg, m64' because gas doens't support it even though
            // it's in the intel manual. It's useless anyways. 
            // The correct pattern is: 'Pat<Rex_W::Yes, B16OpSz::No, sreg, rm64>'
            //
            // The reason |Rex_W| is set to no is to automatically demote this instruction
            // to 32-bits to avoid emitting the rex prefix.
            Pat<Rex_W::No, B16OpSz::No, sreg, r64>
        >,
        Emitter<OpEn::RM>
    >,
    // 0xA0 MOV AL, moffs8   -- FD
    InstrExpr<
        OpCode<0xa0>,
        Pat<Rex_W::No, B16OpSz::No, r<BW::B8, RI::Rax>, moffs8>,
        Emitter<OpEn::FD>
    >,
    // 0xA1 MOV AX, moffs16  -- FD
    // 0xA1 MOV EAX, moffs32 -- FD
    // 0xA1 MOV RAX, moffs64 -- FD
    InstrExpr<
        OpCode<0xa1>,
        Or<
            Pat<Rex_W::No, B16OpSz::Yes, r<BW::B16, RI::Rax>, moffs16>,
            Pat<Rex_W::No, B16OpSz::No, r<BW::B32, RI::Rax>, moffs32>,
            Pat<Rex_W::Yes, B16OpSz::No, r<BW::B64, RI::Rax>, moffs64>
        >,
        Emitter<OpEn::FD>
    >,
    // 0xA2 MOV moffs8, AL -- TD
    InstrExpr<
        OpCode<0xa2>,
        Pat<Rex_W::No, B16OpSz::No, moffs8, r<BW::B8, RI::Rax>>,
        Emitter<OpEn::TD>
    >,
    // 0xA3 MOV moffs16, AX  -- TD
    // 0xA3 MOV moffs32, EAX -- TD
    // 0xA3 MOV moffs64, RAX -- TD
    InstrExpr<
        OpCode<0xa3>,
        Or<
            Pat<Rex_W::No, B16OpSz::Yes, moffs16, r<BW::B16, RI::Rax>>,
            Pat<Rex_W::No, B16OpSz::No, moffs32, r<BW::B32, RI::Rax>>,
            Pat<Rex_W::Yes, B16OpSz::No, moffs64, r<BW::B64, RI::Rax>>
        >,
        Emitter<OpEn::TD>
    >,
    // 0xB0 MOV r8, imm8 -- OI
    InstrExpr<
        OpCode<0xb0>,
        Pat<Rex_W::No, B16OpSz::No, r8, imm8>,
        Emitter<OpEn::OI>
    >,
    // 0xB8 MOV r16, imm16 -- OI
    // 0xB8 MOV r32, imm32 -- OI
    // 0xB8 MOV r64, imm64 -- OI
    InstrExpr<
        OpCode<0xb8>,
        Or<
            Pat<Rex_W::No, B16OpSz::Yes, r16, imm16>,
            Pat<Rex_W::No, B16OpSz::No, r32, imm32>,
            Pat<Rex_W::Yes, B16OpSz::No, r64, imm64>
        >,
        Emitter<OpEn::OI>
    >,
    // 0xC6 MOV r/m8, imm8 -- MI
    InstrExpr<
        OpCode<0xc6>,
        Pat<Rex_W::No, B16OpSz::No, rm8, imm8>,
        Emitter<OpEn::MI>
    >,
    // 0xC7 MOV r/m16, imm16 -- MI
    // 0xC7 MOV r/m32, imm32 -- MI
    // 0xC7 MOV r/m64, imm32 -- MI
    InstrExpr<
        OpCode<0xc7>,
        Or<
            Pat<Rex_W::No, B16OpSz::Yes, rm16, imm16>,
            Pat<Rex_W::No, B16OpSz::No, rm32, imm32>,
            Pat<Rex_W::Yes, B16OpSz::No, rm64, imm32>
        >,
        Emitter<OpEn::MI>
    >,
    // 0x0F 0x20 MOV r64, CR0-CR8 -- MR
    InstrExpr<
        OpCode<0x0f, 0x20>,
        Pat<Rex_W::No, B16OpSz::No, r64, cr>,
        Emitter<OpEn::MR>
    >,
    // 0x0F 0x22 MOV CR0-CR8, r64 -- RM
    InstrExpr<
        OpCode<0x0f, 0x22>,
        Pat<Rex_W::No, B16OpSz::No, cr, r64>,
        Emitter<OpEn::RM>
    >,
    // 0x0F 0x21 MOV r64, DR0-DR7 -- MR 
    InstrExpr<
        OpCode<0x0f, 0x21>,
        Pat<Rex_W::No, B16OpSz::No, r64, dbg>,
        Emitter<OpEn::MR>
    >,
    // 0x0F 0x23 MOV DR0-DR7, r64 -- RM
    InstrExpr<
        OpCode<0x0f, 0x23>,
        Pat<Rex_W::No, B16OpSz::No, dbg, r64>,
        Emitter<OpEn::RM>
    >
>;

} // namespace fiska::x86::instructions

#endif // __X86_ASSEMBLER_LIB_INSTRUCTIONS_MOV_HH__
