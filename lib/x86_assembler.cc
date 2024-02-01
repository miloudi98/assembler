#include "lib/x86_assembler.hh"

#include "lib/core.hh"

void fiska::x86::InstructionBuf::append(u8 byte) {
    buf_[sz_++] = byte;
}

void fiska::x86::InstructionBuf::append(BW bw, u64 qword) {
    switch (bw) {
    case BW::B0:
        break;
    case BW::B8: {
        append(u8(qword >> 0) & 0xff);
        break;
    }
    case BW::B16: {
        append(u8(qword >> 0) & 0xff);
        append(u8(qword >> 8) & 0xff);
        break;
    }
    case BW::B24: {
        append(u8(qword >> 0) & 0xff);
        append(u8(qword >> 8) & 0xff);
        append(u8(qword >> 16) & 0xff);
        break;
    }
    case BW::B32: {
        append(u8(qword >> 0) & 0xff);
        append(u8(qword >> 8) & 0xff);
        append(u8(qword >> 16) & 0xff);
        append(u8(qword >> 24) & 0xff);
        break;
    }
    case BW::B64: {
        append(u8(qword >> 0) & 0xff);
        append(u8(qword >> 8) & 0xff);
        append(u8(qword >> 16) & 0xff);
        append(u8(qword >> 24) & 0xff);
        append(u8(qword >> 32) & 0xff);
        append(u8(qword >> 40) & 0xff);
        append(u8(qword >> 48) & 0xff);
        append(u8(qword >> 56) & 0xff);
        break;
    }
    } // switch
}

void fiska::x86::InstructionBuf::append_opcode(u64 opcode) {
    if (utils::fits_in_u8(i64(opcode))) {
        append(BW::B8, opcode);

    } else if(utils::fits_in_u16(i64(opcode))) {
        append(BW::B16, opcode);

    } else if (utils::fits_in_u24(i64(opcode))) {
        append(BW::B24, opcode);
    } else {
        unreachable();
    }
}

auto fiska::x86::rk_of_ri(RI id) -> RK {
    using enum RI;

    switch (id) {
    case Rax:
    case Rcx:
    case Rdx:
    case Rbx:
    case Rsp:
    case Rbp:
    case Rsi:
    case Rdi:
    case R8:
    case R9:
    case R10:
    case R11:
    case R12:
    case R13:
    case R14:
    case R15:
    case Rah:
    case Rch:
    case Rdh:
    case Rbh:
        return RK::Gp;

    case Rip:
        return RK::Ip;

    case Es:
    case Cs:
    case Ss:
    case Ds:
    case Fs:
    case Gs:
        return RK::Seg;

    case Cr0:
    case Cr1:
    case Cr2:
    case Cr3:
    case Cr4:
    case Cr5:
    case Cr6:
    case Cr7:
    case Cr8:
    case Cr9:
    case Cr10:
    case Cr11:
    case Cr12:
    case Cr13:
    case Cr14:
    case Cr15:
        return RK::Ctrl;

    case Dbg0:
    case Dbg1:
    case Dbg2:
    case Dbg3:
    case Dbg4:
    case Dbg5:
    case Dbg6:
    case Dbg7:
    case Dbg8:
    case Dbg9:
    case Dbg10:
    case Dbg11:
    case Dbg12:
    case Dbg13:
    case Dbg14:
    case Dbg15:
        return RK::Dbg;
    } // switch
    unreachable();
}
