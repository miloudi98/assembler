#include "lib/backend/codegen/emitters.hh"
#include "lib/common/base.hh"
#include "lib/backend/codegen/patterns.hh"

namespace fiska::assembler::backend {

auto X86ByteCode::add(BW bw, u64 qword) -> void {
    assert(bw != BW::Invalid and size() + (+bw / 8) < X86Info::kMaxInstrLength);

    switch (bw) {
    case BW::Invalid: unreachable();

    case BW::B8:
        bytecode_[bytecode_sz_++] = u8(qword >> 0) & 0xff;
        break;
    case BW::B16:
        bytecode_[bytecode_sz_++] = u8(qword >> 0) & 0xff;
        bytecode_[bytecode_sz_++] = u8(qword >> 8) & 0xff;
        break;
    case BW::B24:
        bytecode_[bytecode_sz_++] = u8(qword >> 0) & 0xff;
        bytecode_[bytecode_sz_++] = u8(qword >> 8) & 0xff;
        bytecode_[bytecode_sz_++] = u8(qword >> 16) & 0xff;
        break;
    case BW::B32:
        bytecode_[bytecode_sz_++] = u8(qword >> 0) & 0xff;
        bytecode_[bytecode_sz_++] = u8(qword >> 8) & 0xff;
        bytecode_[bytecode_sz_++] = u8(qword >> 16) & 0xff;
        bytecode_[bytecode_sz_++] = u8(qword >> 24) & 0xff;
        break;
    case BW::B64:
        bytecode_[bytecode_sz_++] = u8(qword >> 0) & 0xff;
        bytecode_[bytecode_sz_++] = u8(qword >> 8) & 0xff;
        bytecode_[bytecode_sz_++] = u8(qword >> 16) & 0xff;
        bytecode_[bytecode_sz_++] = u8(qword >> 24) & 0xff;
        bytecode_[bytecode_sz_++] = u8(qword >> 32) & 0xff;
        bytecode_[bytecode_sz_++] = u8(qword >> 40) & 0xff;
        bytecode_[bytecode_sz_++] = u8(qword >> 48) & 0xff;
        bytecode_[bytecode_sz_++] = u8(qword >> 56) & 0xff;
        break;
    } // switch
}

auto X86ByteCode::add(u64 qword) -> void {
    if (std::bit_width(qword) <= 8) {
        add(BW::B8, qword);
    } else if (std::bit_width(qword) <= 16) {
        add(BW::B16, qword);
    } else if (std::bit_width(qword) <= 24) {
        add(BW::B24, qword);
    } else if (std::bit_width(qword) <= 32) {
        add(BW::B32, qword);
    } else {
        add(BW::B64, qword);
    }
}

auto X86ByteCode::add_relocation(StrRef symbol_name) -> void {
    Relocation* reloc = &rels_.emplace_back();
    reloc->symbol_name_ = symbol_name;
    reloc->i_offset_ = size();
}

} // namespace fiska::assembler::backend
