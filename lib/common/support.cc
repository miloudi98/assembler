#include "lib/common/support.hh"
#include "lib/common/base.hh"

auto fiska::assembler::Span::include(const Span& o) -> Span& {
    pos_ = std::min(pos_, o.pos_);
    len_ = u16(std::max(pos_ + len_, o.pos_ + o.len_) - pos_);
    return *this;
}

auto fiska::assembler::StringInterner::save(StrRef str) -> StrRef {
    auto [slot, is_inserted] = unique_strings_.insert(str);
    if (is_inserted) {
        // Intern the string.
        u64 offset = storage_.size();

        storage_.resize(storage_.size() + str.size());
        std::memcpy(storage_.data() + offset, str.data(), str.size());

        return StrRef{storage_.data() + offset, str.size()};
    }
    return *slot;
}

auto fiska::assembler::Ctx::load_file(const fs::path& path) -> File* {
    auto file = std::make_unique<File>(
        u16(files_.size()),
        path,
        utils::load_file(path)
    );

    files_.push_back(std::move(file));
    return files_.back().get();
}

auto fiska::assembler::Ctx::read_file(u16 fid) -> File* {
    assert(fid < files_.size(), "Invalid fid: '{}' encountered.", fid);
    return files_[fid].get();
}

