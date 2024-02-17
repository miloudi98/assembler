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

auto fiska::assembler::Span::info(Ctx* ctx) -> SpanInfo {
    SpanInfo info{};

    // Split the span into lines.
    StrRef span_str{ctx->files_[fid_]->data() + pos_, len_};
    for (const auto line : span_str | vws::split('\n')) {
        info.lines_.push_back(Str{line.begin(), line.end()});
    }

    // Find the starting line number and column number.
    info.lnr_ = 1;
    info.cnr_ = 1;

    const char* c = ctx->files_[fid_]->data();
    for (u64 idx = 0; idx < pos_; ++idx) {
        info.lnr_ += *c == '\n';
        info.cnr_ = *c == '\n' ? 1 : info.cnr_ + 1;
        c++;
    }

    return info;
}

auto fiska::assembler::Span::line(Ctx* ctx) -> Pair<const char*, const char*> {
    const char* file_beg = ctx->files_[fid_]->data();
    const char* file_end = file_beg + ctx->files_[fid_]->size();
    const char* lbeg = ctx->files_[fid_]->data() + pos_;
    const char* lend = lbeg + len_;

    while (lbeg > file_beg and *lbeg != '\n') { lbeg--; }
    while (lend < file_end and *lend != '\n') { lend++; }
    lbeg += *lbeg == '\n';

    return { lbeg, lend };
}

fiska::assembler::Diagnostic::Diagnostic(Ctx* ctx, StrRef message, Span span) {
    using enum fmt::color;
    using enum fmt::emphasis;

    // error: message
    //   --> ${path}:${line}:${column}
    //    |
    // 16 |     fn foo(self) -> Self::Bar {
    //    |                     ---------
    Str out;

    // Print the diagnostic level.
    switch (lvl_) {
    case Level::Error:
        out += fmt::format(fg(red) | bold, "error: ");
        break;
    } // switch

    // Print the error message.
    out += fmt::format("{}\n", message);

    // Print the file path, line number and column number.
    SpanInfo span_info = span.info(ctx);
    out += fmt::format("  --> {}:{}:{}\n", ctx->files_[span.fid_]->path_.string(), span_info.lnr_, span_info.cnr_);

    // Print the error line with one empty line above and one below.
    //
    // Sidebar printing logic.
    u8 lnr_width = u8(utils::number_width(span_info.lnr_));
    auto sidebar = [&](i64 lnr = 0) -> Str {
        Str lnr_placeholder = Str(lnr_width + 1, ' ');

        if (lnr) {
            Str str_of_lnr = fmt::format("{}", lnr);
            std::memcpy(lnr_placeholder.data(), str_of_lnr.data(), str_of_lnr.size());
        }

        return fmt::format(" {} | ", lnr_placeholder);
    };

    // Empty line above.
    out += fmt::format("{}\n", sidebar());
    // Error line.
    auto [span_lbeg, span_lend] = span.line(ctx);
    auto before = StrRef{span_lbeg, span_info.cnr_ - 1};
    auto after = span_lbeg + span_info.cnr_ + span.len_ >= span_lend
        ? ""
        : StrRef{span_lbeg + span_info.cnr_ + span.len_, span_lend};

    // Print what's before the range.
    out += fmt::format("{}{}", sidebar(span_info.lnr_), before);
    // Print the range.
    //
    // Print the first line since we need to account for the characters before the start 
    // of the range.
    out += fmt::format(fg(red), "{}\n", span_info.lines_[0]); 
    // Underline the first line.
    //
    // Account for before's offset and sidebar's offset.
    out += fmt::format("{}{}", sidebar(), Str(before.size(), ' '));
    // Underline.
    assert(span_info.lines_[0].size() > 0, "Overflow");
    out += fmt::format(fg(red), "^{}\n", Str(span_info.lines_[0].size() - 1, '~'));
    span_info.lnr_++;
    // Remove the first line.
    span_info.lines_.erase(span_info.lines_.begin());
    // Underline the rest of the lines now.
    //
    // Truncate the lines if the error is too long...
    if (span_info.lines_.size() > 20) {
        span_info.lines_ = Vec<Str>{span_info.lines_.begin(), span_info.lines_.begin() + 20};
    }
    for (const auto& line : span_info.lines_) {
        if (line.empty()) { continue; }

        out += sidebar(span_info.lnr_);
        out += fmt::format(fg(red), "{}\n", line);
        // Underline the line.
        //
        // Don't underline whitespace.
        out += sidebar();
        for (i1 underline = false; char c : line) {
            underline |= not std::isspace(static_cast<u8>(c));
            out += fmt::format(fg(red), "{}", underline ? '~' : ' ');
        }
        out += "\n";
        span_info.lnr_++;
    }
    // Remove the last newline to print what's after the range.
    out.pop_back();
    // Print what's after the range.
    out += fmt::format("{}\n", after);
    // Empty line below.
    out += fmt::format("{}\n", sidebar());

    // Print the diagnostic.
    fmt::print("{}\n", out);
    std::exit(1);
}
