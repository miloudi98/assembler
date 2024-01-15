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
};

struct StringInterner {
    std::unordered_set<StrRef> unique_strings_;
    Vec<char*> storage_;

    auto save(StrRef str) -> StrRef {
        auto [slot, is_inserted] = unique_strings_.insert(str);
        if (is_inserted) {
            char* alloc = new char[str.size() + 1];
            alloc[str.size()] = '\0';
            storage_.push_back(alloc);
        }
        return *slot;
    }

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

    auto view_file(u16 fid) -> File* {
        assert(fid < files_.size(), "Invalid fid '{}' encountered when attempting to view a file.", fid);
        return files_[fid].get();
    }

    auto load_file(const fs::path& path) -> u16 {
        u16 fid = u16(files_.size());
        files_.push_back(
            std::make_unique<File>(fid, path, utils::load_file(path))
        );
        return fid;
    }
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

    auto source_text(Context* ctx) -> StrRef {
        File* file = ctx->view_file(fid_);
        return StrRef{file->code_.data(), file->code_.size()}.substr(pos_, len_);
    }

    auto line_col_info(Context* ctx) -> LineColInfo {
        File* file = ctx->view_file(fid_);
        
        const char* file_start = file->code_.data();
        const char* file_end = file_start + file->code_.size();
        const char* curr_pos = file_start + pos_;

        assert(curr_pos < file_end and curr_pos + len_ < file_end,
            "Invalid location Location{{.pos_ = {}, .len_ = {}, .fid_ = {}}}\n", pos_, len_, fid_);

        LineColInfo info{};
        std::tie(info.line_start_, info.line_end_) = {curr_pos, curr_pos};

        while (info.line_end_ < file_end and *(info.line_end_) != '\n') { info.line_end_++; }
        while (info.line_start_ > file_start and *(info.line_start_) != '\n') { info.line_start_--; }

        info.line_start_ += *(info.line_start_) == '\n';

        for (auto c = file_start; c != curr_pos; ++c) {
            info.line_ += *c == '\n';
            info.col_ = (*c == '\n') ? 0 : info.col_ + 1;
        }

        return info;
    }
};

}  // namespace fiska

#endif
