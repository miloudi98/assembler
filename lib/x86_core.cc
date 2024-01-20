#include "lib/x86_core.hh"

#include "lib/core.hh"
#include "lib/front_end.hh"

auto fiska::Location::source_text(Context* ctx) const -> StrRef {
    File* file = ctx->get_file(fid_);
    return StrRef{file->data(), file->size()}.substr(pos_, len_);
}

auto fiska::Location::line_col_info(Context* ctx) const -> LineColInfo {
    File* file = ctx->get_file(fid_);
    
    const char* file_start = file->data();
    const char* file_end = file_start + file->size();
    const char* curr_pos = file_start + pos_;

    assert(curr_pos < file_end and curr_pos + len_ < file_end,
        "Invalid location Location{{.pos_ = {}, .len_ = {}, .fid_ = {}}}\n", pos_, len_, fid_);

    LineColInfo info {
        .line_ = 0,
        .col_ = 0,
        .line_start_ = curr_pos,
        .line_end_ = curr_pos + len_
    };

    while (info.line_end_ < file_end and *(info.line_end_) != '\n') { info.line_end_++; }
    while (info.line_start_ > file_start and *(info.line_start_) != '\n') { info.line_start_--; }

    info.line_start_ += *(info.line_start_) == '\n';

    for (auto c = file_start; c != curr_pos; ++c) {
        info.line_ += *c == '\n';
        info.col_ = (*c == '\n') ? 0 : info.col_ + 1;
    }

    return info;
}

auto fiska::Location::merge(const Location& other) const -> Location {
    assert(fid_ == other.fid_, "Trying to merge locations from two different files.");

    return Location {
        .pos_ = std::min(pos_, other.pos_),
        .len_ = std::max(pos_ + len_, other.pos_ + other.len_) - std::min(pos_, other.pos_),
        .fid_ = fid_
    };
}

auto fiska::Context::get_file(u16 fid) -> File* {
    assert(fid < files_.size(), "Invalid fid '{}' encountered when attempting to view a file.", fid);
    return files_[fid].get();
}

auto fiska::Context::load_file(const fs::path& path) -> u16 {
    u16 fid = u16(files_.size());
    files_.push_back(
        std::make_unique<File>(fid, path, utils::load_file(path))
    );
    return fid;
}

auto fiska::StringInterner::save(StrRef str) -> StrRef {
    auto [slot, is_inserted] = unique_strings_.insert(str);
    if (is_inserted) {
        char* alloc = new char[str.size() + 1];
        alloc[str.size()] = '\0';
        storage_.push_back(alloc);
    }
    return *slot;
}
