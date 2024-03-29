#include "lib/front_end/context.hh"
#include "lib/core.hh"

auto fiska::x86::fe::Ctx::load_file(const fs::path& path) -> File* {
    auto file = std::make_unique<File>(
        u16(files_.size()),
        path,
        utils::load_file(path)
    );

    files_.push_back(std::move(file));
    // Create a token stream entry for the file.
    tok_streams_.emplace_back();

    return files_.back().get();
}

auto fiska::x86::fe::Ctx::get_file(u16 fid) -> File* {
    assert(fid < files_.size(), "Invalid fid '{}' encountered.", fid);
    return files_[fid].get();
}

fiska::x86::fe::Ctx::~Ctx() {
    rgs::for_each(ast_, [](Expr* node) { delete node; });
}
