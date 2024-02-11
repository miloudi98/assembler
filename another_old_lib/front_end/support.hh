#ifndef __X86_ASSEMBLER_LIB_FRONT_END_SUPPORT_HH__
#define __X86_ASSEMBLER_LIB_FRONT_END_SUPPORT_HH__

#include "lib/core.hh"

namespace fiska::x86::fe {

struct File {
    u16 fid_{};
    fs::path path_;
    Vec<char> code_;

    File(const File&) = delete;
    File(File&&) = delete;
    File& operator=(const File&) = delete;
    File& operator=(File&&) = delete;

    File(u16 fid, fs::path path, Vec<char> code) 
        : fid_(fid), path_(path), code_(std::move(code)) {}

    auto data() const -> const char* { return code_.data(); }
    auto size() const -> u64 { return code_.size(); }
};

struct Location {
    u32 pos_{};
    u32 len_{};
    u16 fid_{};
};

struct StringInterner {
    std::unordered_set<StrRef> unique_strings_;
    Vec<char*> storage_;

    auto save(StrRef str) -> StrRef; 
    ~StringInterner(); 
};

} // namespace fiska::fe

#endif // __X86_ASSEMBLER_LIB_FRONT_END_SUPPORT_HH__
