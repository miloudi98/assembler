#ifndef __X86_ASSEMBLER_LIB_COMMON_SUPPORT_HH__
#define __X86_ASSEMBLER_LIB_COMMON_SUPPORT_HH__

#include "lib/common/base.hh"

namespace fiska::assembler {

struct StringInterner {
    NOT_COPYABLE_NOT_MOVABLE(StringInterner);

    HashSet<StrRef> unique_strings_;
    Vec<char*> storage_;

    StringInterner() = default;
    ~StringInterner();
    auto save(StrRef str) -> StrRef;
};

struct File {
    NOT_COPYABLE_NOT_MOVABLE(File);

    u16 fid_{};
    fs::path path_;
    Vec<char> code_;

    File(u16 fid, fs::path path, Vec<char> code) 
        : fid_(fid), path_(path), code_(std::move(code)) {}

    auto data() const -> const char* { return code_.data(); }
    auto size() const -> u64 { return code_.size(); }
};

struct Span {
    u32 pos_{};
    u16 len_{};
    u16 fid_{};

    constexpr Span() = default;

    constexpr Span(Span a, Span b) {
        assert(a.fid_ == b.fid_);
        pos_ = std::min(a.pos_, b.pos_);
        len_ = u16(std::max(a.pos_ + a.len_, b.pos_ + b.len_) - pos_);
    }

    auto is_eof() const -> i1 { return len_ == 0; }
};

} // namespace fiska::assembler

#endif // __X86_ASSEMBLER_LIB_COMMON_SUPPORT_HH__
