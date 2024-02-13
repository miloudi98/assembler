#ifndef __X86_ASSEMBLER_LIB_SUPPORT_FE_UTILS_HH__
#define __X86_ASSEMBLER_LIB_SUPPORT_FE_UTILS_HH__

#include "lib/support/core.hh"

namespace fiska::x86 {

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
    u32 len_ = 1;
    u16 fid_{};
};

namespace fe { struct Ctx; }
struct ErrorSpan {
    static constexpr i8 kCtxSize = 3;

    const char* start_{};
    const char* end_{};
    const char* ctx_start_{};
    const char* ctx_end_{};
    u32 line_ = 1;
    u32 col_ = 1;

    [[nodiscard]] static auto from(fe::Ctx* ctx, Location loc) -> ErrorSpan;
    static auto print(ErrorSpan err_span) -> void;
};

struct StringInterner {
    std::unordered_set<StrRef> unique_strings_;
    Vec<char*> storage_;

    auto save(StrRef str) -> StrRef; 
    ~StringInterner(); 
};

} // namespace fiska::x86

#endif // __X86_ASSEMBLER_LIB_SUPPORT_FE_UTILS_HH__
