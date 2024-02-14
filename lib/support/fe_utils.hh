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

namespace fe { 
struct Ctx;
} // namespace fe

struct ErrorSpan {
    static constexpr i8 kCtxSize = 3;

    const char* start_{};
    const char* end_{};
    const char* ctx_start_{};
    const char* ctx_end_{};
    Str msg_;
    u32 line_ = 1;
    u32 col_ = 1;

    template <typename... Args>
    [[nodiscard]] static auto from(
        fe::Ctx* ctx,
        Location loc,
        fmt::format_string<Args...> fmt,
        Args&&... args
    ) -> ErrorSpan
    {
        return err_span_builder(ctx, loc, fmt::format(fmt, std::forward<Args>(args)...));
    }

    [[nodiscard]] static auto err_span_builder(fe::Ctx* ctx, Location loc, Str msg) -> ErrorSpan;
    [[noreturn]] static auto emit(ErrorSpan err_span) -> void;
};

struct StringInterner {
    std::unordered_set<StrRef> unique_strings_;
    Vec<char*> storage_;

    auto save(StrRef str) -> StrRef; 
    ~StringInterner(); 
};

} // namespace fiska::x86

#endif // __X86_ASSEMBLER_LIB_SUPPORT_FE_UTILS_HH__
