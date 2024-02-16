#ifndef __X86_ASSEMBLER_LIB_COMMON_SUPPORT_HH__
#define __X86_ASSEMBLER_LIB_COMMON_SUPPORT_HH__

#include "lib/common/base.hh"

namespace fiska::assembler {

struct Span {
    u32 pos_{};
    u16 len_{};
    u16 fid_{};

    auto include(const Span& o) -> Span&;
    auto eof() const -> i1 { return len_ == 0; }
};

struct StringInterner {
    NOT_COPYABLE_NOT_MOVABLE(StringInterner);

    HashSet<StrRef> unique_strings_;
    Vec<char> storage_;


    StringInterner() = default;
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

struct Diagnostic {
    enum struct Level {
        Error
    };

    Level level = Level::Error;
    Span span_{};
};

} // namespace fiska::assembler

#endif // __X86_ASSEMBLER_LIB_COMMON_SUPPORT_HH__
