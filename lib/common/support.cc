#include "lib/common/support.hh"
#include "lib/common/base.hh"

auto fiska::assembler::Span::include(const Span& o) -> Span& {
    pos_ = std::min(pos_, o.pos_);
    len_ = u16(std::max(pos_ + len_, o.pos_ + o.len_) - pos_);
    return *this;
}

auto fiska::assembler::StringInterner::save(StrRef str) -> StrRef {
    if (not unique_strings_.contains(str)) {
        // Intern the string.
        storage_.push_back(new char[str.size()]);
        std::memcpy(storage_.back(), str.data(), str.size());
        unique_strings_.insert(StrRef{storage_.back(), str.size()});
    }

    return *unique_strings_.find(str);
}

fiska::assembler::StringInterner::~StringInterner() {
    rgs::for_each(storage_, [](char* alloc) { delete[] alloc; });
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
    using enum fmt::color;
    using enum fmt::emphasis;

    SpanInfo info{};

    // Find the starting line number and column number.
    info.lnr_ = 1;
    info.cnr_ = 1;

    const char* c = ctx->files_[fid_]->data();
    for (u64 idx = 0; idx < pos_; ++idx) {
        info.lnr_ += *c == '\n';
        info.cnr_ = *c == '\n' ? 1 : info.cnr_ + 1;
        c++;
    }

    // Return early if this is the span of the EOF token.
    if (eof()) { return info; }


    // Find the start and the end of the line where the error occured.
    auto [span_lbeg, span_lend] = line(ctx);
    auto before = StyledStr({span_lbeg, info.cnr_ - 1}, fmt::emphasis(0));
    auto after = StyledStr(
        span_lbeg + (info.cnr_ - 1) + len_ >= span_lend ? "" : StrRef{span_lbeg + (info.cnr_ - 1) + len_, span_lend},
        fmt::emphasis(0)
    );

    // Split the span into lines.
    StrRef span_str{ctx->files_[fid_]->data() + pos_, len_};
    auto styled_lines = span_str 
        | vws::split('\n')
        | vws::transform([&](auto vw) { return StyledStr(StrRef{vw}, fg(red) | bold); });

    for (const auto& line : styled_lines) {
        if (line.inner_.empty()) { continue; }
        info.slines_.push_back(std::move(line));
    }

    info.slines_[0] = before.merge(info.slines_[0]);
    info.slines_.back() = info.slines_.back().merge(after);

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
    //    |                     ~~~~~~~~~
    Str out;

    // Print the diagnostic level.
    out += fmt::format(fg(red) | bold, "error: ");
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
    // Print the range.
    for (StyledStr& styled_line : span_info.slines_) {
        out += sidebar(span_info.lnr_);
        for (const StyledChar& sc : styled_line) {
            out += fmt::format(sc.ts_, "{}", sc.c_);
        }
        out += "\n";

        // Underline the line.
        out += sidebar();
        for (i1 underline = false; StyledChar sc : styled_line) {
            underline |= not std::isspace(static_cast<u8>(sc.c_));
            out += fmt::format(sc.ts_, "{}", 
                    underline and sc.ts_.has_emphasis() ? '~' : ' ');
        }
        out += "\n";
        span_info.lnr_++;
    }
    // Empty line below.
    out += fmt::format("{}\n", sidebar());

    // Print the diagnostic.
    fmt::print("{}\n", out);
    std::exit(1);
}
