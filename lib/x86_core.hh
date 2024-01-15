#ifndef __X86_ASSEMBLER_X86_CORE_HH__
#define __X86_ASSEMBLER_X86_CORE_HH__

#include <unordered_set>
#include <vector>
#include <algorithm>

#include "lib/core.hh"

namespace fiska {

struct Module;

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

struct StringInterner {
    std::unordered_set<StrRef> unique_strings_;
    Vec<char*> storage_;

    auto save(StrRef str) -> StrRef; 

    ~StringInterner() {
        rgs::for_each(storage_, [](char* alloc) { delete alloc; });
    }

};

struct Context {
    Vec<Box<File>> files_;
    Vec<Box<Module>> modules_;

    Context(const Context&) = delete;
    Context(Context&&) = delete;
    Context& operator=(const Context&) = delete;
    Context& operator=(Context&&) = delete;

    auto get_file(u16 fid) -> File*; 
    auto load_file(const fs::path& path) -> u16; 
};

struct LineColInfo {
    u32 line_{};
    u32 col_{};
    const char* line_start_{};
    const char* line_end_{};
};

struct Location {
    u32 pos_{};
    u32 len_{};
    u16 fid_{};

    auto source_text(Context* ctx) -> StrRef;
    auto line_col_info(Context* ctx) -> LineColInfo;
};

}  // namespace fiska

#endif
