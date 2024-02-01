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

