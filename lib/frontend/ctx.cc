#include "lib/frontend/ctx.hh"
#include "lib/common/base.hh"
#include "lib/common/support.hh"
#include "lib/frontend/ast.hh"

namespace fiska::assembler::frontend {

auto Ctx::load_file(const fs::path& path) -> File* {
    u16 fid = u16(files_.size());
    files_.push_back(std::make_unique<File>(fid, path, utils::load_file(path)));
    return files_.back().get();
}

auto Ctx::read_file(u16 fid) -> File* {
    assert(fid < files_.size(), "Index out of bounds");
    return files_[fid].get();
}

Ctx::~Ctx() {
    rgs::for_each(exprs_, [](Expr* e) { delete e; });
}

} // namespace fiska::assembler::frontend
