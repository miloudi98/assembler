#include "lib/front_end/ctx.hh"

fiska::x86::fe::Ctx::~Ctx() {
    rgs::for_each(ast_, [](Expr* e) { delete e; });
}

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
    assert(fid < files_.size(), "Invalid file id encountered: '{}'.", fid);
    return files_[fid].get();
}

auto fiska::x86::StringInterner::save(StrRef str) -> StrRef {
    auto [slot, is_inserted] = unique_strings_.insert(str);
    if (is_inserted) {
        char* alloc = new char[str.size() + 1];
        alloc[str.size()] = '\0';
        storage_.push_back(alloc);
    }
    return *slot;
}

fiska::x86::StringInterner::~StringInterner() {
    rgs::for_each(storage_, [](char* alloc) { delete[] alloc; });
}

auto fiska::x86::Location::merge(const Location& o) -> Location& {
    pos_ = std::min(pos_, o.pos_);
    len_ = std::max(pos_ + len_, o.pos_ + o.len_) - pos_;
    return *this;
}

GCC_DIAG_IGNORE_PUSH(-Wconversion)
auto fiska::x86::ErrorSpan::err_span_builder(
    fe::Ctx* ctx,
    Location loc,
    Str msg
) -> ErrorSpan {

    const char* file_start = ctx->files_[loc.fid_]->data();
    const char* file_end = file_start + ctx->files_[loc.fid_]->size();

    auto seek_prev_line = [&](const char*& pos) {
        if (pos > file_start and *pos == '\n') { pos--; }

        while (pos > file_start and *pos != '\n') { pos--; }
    };
    auto seek_next_line = [&](const char*& pos) {
        if (pos < file_end and *pos == '\n') { pos++; }

        while (pos < file_end and *pos != '\n') { pos++; }
    };

    ErrorSpan err_span {
        .start_ = file_start + loc.pos_,
        .end_ = file_start + loc.pos_ + loc.len_,
        .msg_ = std::move(msg)
    };

    err_span.ctx_start_ = err_span.start_;
    err_span.ctx_end_ = err_span.end_;

    seek_prev_line(err_span.ctx_start_);
    seek_next_line(err_span.ctx_end_);

    for (i64 i = 0; i < ErrorSpan::kCtxSize; ++i) {
        seek_prev_line(err_span.ctx_start_);
    }
    for (i64 i = 0; i < ErrorSpan::kCtxSize; ++i) {
        seek_next_line(err_span.ctx_end_);
    }

    err_span.ctx_start_ += *err_span.ctx_start_ == '\n';

    // Compute the line and column number of where the error context begins.
    const char* curr = file_start;
    while (curr < err_span.ctx_start_) {
        err_span.line_ += *curr == '\n';
        err_span.col_ = *curr == '\n' ? 1 : err_span.col_ + 1;
        curr++;
    }

    return err_span;
}
GCC_DIAG_IGNORE_POP();


[[noreturn]] auto fiska::x86::ErrorSpan::emit(ErrorSpan err_span) -> void {
    using enum fmt::color;
    using enum fmt::emphasis;

    const u32 err_line_width = utils::number_width(err_span.line_);
    const char* curr = err_span.ctx_start_;
    u64 curr_term_offset = 0;

    auto sidebar = [&](i1 no_line_number = false) {
        u64 line_nr_w = utils::number_width(err_span.line_);
        Str line_nr = no_line_number ? Str(line_nr_w, ' ') : fmt::format("{}", err_span.line_);

        if (line_nr_w == err_line_width) {
            return fmt::format(" {}  | ", line_nr);
        }
        return fmt::format(" {} | ", line_nr);
    };

    fmt::print("{}", sidebar());
    // Print the lines before the error.
    while (curr < err_span.start_) {
        if (*curr == '\n') {
            err_span.line_++;
            curr++;
            fmt::print("\n{}", sidebar());
            curr_term_offset = 0;
            continue;
        }

        fmt::print("{}", *curr++);
        curr_term_offset++;
    }

    // Print the error range.
    // Save the offset so that we can underline the error in the next step.
    u64 err_start_off = curr_term_offset;
    while (curr < err_span.end_) {
        fmt::print(fg(red) | bold, "{}", *curr++);
        curr_term_offset++;
    }
    u64 err_end_off = curr_term_offset;

    // Finish the line where the error resides.
    while (curr < err_span.ctx_end_ and *curr != '\n') {
        fmt::print("{}", *curr++);
    }

    // Underline the error.
    // If we are at the end of file, then we already printed a new line since all posix-compliant 
    // files end with a newline.
    // https://stackoverflow.com/questions/729692/why-should-text-files-end-with-a-newline.
    if (curr < err_span.ctx_end_) {
        fmt::print("{}", *curr);
    }
    fmt::print("{}{}", sidebar(/*no_line_number=*/true), Str(err_start_off, ' '));
    u64 err_mid_off = err_start_off + ((err_end_off - err_start_off) >> 1);

    for (u64 i = err_start_off; i < err_mid_off; ++i) {
        fmt::print(fg(cyan), "\u2500");
    }

    fmt::print(fg(cyan), "\u252c");

    for (u64 i = err_mid_off + 1; i < err_end_off; ++i) {
        fmt::print(fg(cyan), "\u2500");
    }

    // Print the error message.
    fmt::print("\n{}{}", sidebar(/*no_line_number=*/true), Str(err_start_off + ((err_end_off - err_start_off) >> 1), ' '));
    fmt::print(fg(cyan), "\u2514\u2500\u2500");
    fmt::print(fg(white) | bold, " {}", err_span.msg_);

    // Print the lines after the error.
    while (curr < err_span.ctx_end_) {
        if (*curr == '\n') {
            err_span.line_++;
            curr++;
            fmt::print("\n{}", sidebar());
            curr_term_offset = 0;
            continue;
        }

        fmt::print("{}", *curr++);
        curr_term_offset++;
    }

    // Done.
    fmt::print("\n");

    // Exit.
    std::exit(1);

}

auto fiska::x86::fe::Expr::operator new(usz sz, Ctx* ctx, StrRef section) -> void* {
    auto expr = static_cast<Expr*>(::operator new(sz)); 
    expr->section_ = section;
    ctx->ast_.push_back(expr);
    return expr;
}

auto fiska::x86::fe::Expr::operator new(usz sz, Ctx* ctx) -> void* {
    auto expr = static_cast<Expr*>(::operator new(sz)); 
    ctx->ast_.push_back(expr);
    return expr;
}

auto fiska::x86::fe::TokStream::view() -> TokStreamView {
    return TokStreamView(begin(), end());
}
