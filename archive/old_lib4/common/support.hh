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

struct Ctx {
    NOT_COPYABLE_NOT_MOVABLE(Ctx);

    // List of files loaded.
    Vec<Box<File>> files_;
    // Pool of interned strings.
    StringInterner str_pool_;

    Ctx() = default;
    // Load file to memory.
    auto load_file(const fs::path& path) -> File*;
    // Return the file with fid |fid|.
    auto read_file(u16 fid) -> File*;
};

// TODO: Styled string and styled char are really messy. Try to clean this up.
struct StyledChar {
    char c_{};
    fmt::text_style ts_{};
};

struct StyledStr {
    using ValueType = Vec<StyledChar>;
    using Iterator = ValueType::iterator;
    ValueType inner_;

    explicit StyledStr(ValueType i) : inner_(std::move(i)) {}
    explicit StyledStr(StrRef s, fmt::text_style ts) {
        std::transform(
            s.begin(),
            s.end(),
            std::back_inserter(inner_),
            [ts](char c) { return StyledChar{c, ts}; }
        );
    }
    StyledStr(const StyledStr& o) = default;
    StyledStr(StyledStr&& o) : inner_(std::move(o.inner_)) {}
    StyledStr& operator=(StyledStr&& o) {
        inner_ = std::move(o.inner_);
        return *this;
    }

    auto begin() -> Iterator { return inner_.begin(); }
    auto end() -> Iterator { return inner_.end(); }

    auto merge(const StyledStr& o) -> StyledStr {
        Vec<StyledChar> ret;
        std::copy(inner_.begin(), inner_.end(), std::back_inserter(ret));
        std::copy(o.inner_.begin(), o.inner_.end(), std::back_inserter(ret));
        return StyledStr{std::move(ret)};
    }
};

struct SpanInfo {
    // Starting line number.
    u32 lnr_{};
    // Starting column number.
    u32 cnr_{};
    // Span split into multiple lines.
    Vec<StyledStr> slines_{};
};

struct Span {
    u32 pos_{};
    u16 len_{};
    u16 fid_{};

    constexpr Span(Span a, Span b) {
        assert(a.fid_ == b.fid_, "Attempting to merge spans from different files.");
        if (a.is_eof() or b.is_eof()) { return; }
        pos_ = std::min(a.pos_, b.pos_);
        len_ = u16(std::max(a.pos_ + a.len_, b.pos_ + b.len_) - pos_);
    }

    auto include(const Span& o) -> Span&;
    auto is_eof() const -> i1 { return len_ == 0; }
    auto file_info(Ctx* ctx) -> SpanInfo;
    auto line(Ctx* ctx) -> Pair<const char*, const char*>;
};

struct Diagnostic {
    [[noreturn]] explicit Diagnostic(Ctx* ctx, StrRef message, Span span);
};

} // namespace fiska::assembler

#endif // __X86_ASSEMBLER_LIB_COMMON_SUPPORT_HH__
