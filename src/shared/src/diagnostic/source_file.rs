use super::Span;

/// A 1-based line and column, as an editor would report it.
///
/// `column` counts *characters*, not bytes: a line with a multi-byte character
/// before the span would otherwise report a column no editor agrees with.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub line: u32,
    pub column: u32,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// One source file, and the line table needed to turn offsets into locations.
#[derive(Debug, Clone)]
pub struct SourceFile {
    pub name: String,
    pub source: String,
    /// Byte offset of the first character of each line. Always starts with 0,
    /// so it is never empty and line numbers are `index + 1`.
    line_starts: Vec<u32>,
}

impl SourceFile {
    pub fn new(name: impl Into<String>, source: impl Into<String>) -> Self {
        let source = source.into();

        let mut line_starts = vec![0u32];
        line_starts.extend(
            source
                .bytes()
                .enumerate()
                .filter(|(_, byte)| *byte == b'\n')
                .map(|(offset, _)| offset as u32 + 1),
        );

        Self {
            name: name.into(),
            source,
            line_starts,
        }
    }

    pub fn line_count(&self) -> u32 {
        self.line_starts.len() as u32
    }

    /// The location of `offset`, clamped into the file.
    ///
    /// Clamping rather than failing: a diagnostic pointing just past the last
    /// character — an unexpected end of file, say — still has to render.
    pub fn location(&self, offset: u32) -> Location {
        let offset = offset.min(self.source.len() as u32);

        let index = match self.line_starts.binary_search(&offset) {
            Ok(index) => index,
            Err(index) => index - 1, // never 0: line_starts[0] == 0 <= offset
        };

        let line_start = self.line_starts[index] as usize;
        let column = self.source[line_start..offset as usize].chars().count() as u32 + 1;

        Location {
            line: index as u32 + 1,
            column,
        }
    }

    /// The byte range of `line` (1-based), excluding its line terminator.
    pub fn line_range(&self, line: u32) -> Option<(u32, u32)> {
        let index = line.checked_sub(1)? as usize;
        let start = *self.line_starts.get(index)?;

        let end = self
            .line_starts
            .get(index + 1)
            .map(|next| next - 1) // drop the '\n'
            .unwrap_or(self.source.len() as u32);

        // A CRLF file keeps its '\r' in the slice otherwise, which would put a
        // stray carriage return in the middle of a rendered diagnostic.
        let end = if end > start && self.source.as_bytes()[end as usize - 1] == b'\r' {
            end - 1
        } else {
            end
        };

        Some((start, end))
    }

    /// The text of `line` (1-based), without its line terminator.
    pub fn line(&self, line: u32) -> Option<&str> {
        let (start, end) = self.line_range(line)?;
        self.source.get(start as usize..end as usize)
    }

    /// The source a span covers.
    ///
    /// `None` rather than a panic on a bad range: spans only ever originate at
    /// token boundaries, but the emitter must not be able to panic *while
    /// reporting an error*.
    pub fn snippet(&self, span: Span) -> Option<&str> {
        self.source.get(span.start as usize..span.end as usize)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn file(source: &str) -> SourceFile {
        SourceFile::new("t.ar", source)
    }

    #[test]
    fn locates_the_first_character() {
        assert_eq!(
            file("let x = 1;").location(0),
            Location { line: 1, column: 1 }
        );
    }

    #[test]
    fn locates_within_the_first_line() {
        assert_eq!(
            file("let x = 1;").location(4),
            Location { line: 1, column: 5 }
        );
    }

    #[test]
    fn locates_the_start_of_a_later_line() {
        assert_eq!(
            file("a\nbb\nccc").location(5),
            Location { line: 3, column: 1 }
        );
    }

    #[test]
    fn locates_within_a_later_line() {
        assert_eq!(
            file("a\nbb\nccc").location(7),
            Location { line: 3, column: 3 }
        );
    }

    /// Columns count characters, so a multi-byte character ahead of the offset
    /// must not push the column past where an editor would report it.
    #[test]
    fn counts_columns_in_characters_not_bytes() {
        let source = "let emoji = \"🦀\"; let x = 1;";
        let offset = source.find("let x").unwrap() as u32;

        assert_eq!(
            file(source).location(offset),
            Location {
                line: 1,
                column: source[..offset as usize].chars().count() as u32 + 1
            }
        );
    }

    /// An unexpected end of file points one past the last character, which
    /// still has to render.
    #[test]
    fn clamps_an_offset_past_the_end() {
        assert_eq!(
            file("ab").location(999),
            Location { line: 1, column: 3 }
        );
    }

    #[test]
    fn handles_a_file_with_no_trailing_newline() {
        let file = file("a\nb");

        assert_eq!(file.line_count(), 2);
        assert_eq!(file.line(2), Some("b"));
    }

    #[test]
    fn handles_a_trailing_newline() {
        let file = file("a\n");

        assert_eq!(file.line_count(), 2);
        assert_eq!(file.line(1), Some("a"));
        assert_eq!(file.line(2), Some(""));
    }

    #[test]
    fn strips_the_carriage_return_of_a_crlf_line() {
        let file = file("a\r\nb\r\n");

        assert_eq!(file.line(1), Some("a"));
        assert_eq!(file.line(2), Some("b"));
    }

    #[test]
    fn has_no_line_zero() {
        assert_eq!(file("a").line(0), None);
    }

    #[test]
    fn has_no_line_past_the_end() {
        assert_eq!(file("a").line(2), None);
    }

    #[test]
    fn empty_file_has_one_empty_line() {
        let file = file("");

        assert_eq!(file.line_count(), 1);
        assert_eq!(file.line(1), Some(""));
        assert_eq!(file.location(0), Location { line: 1, column: 1 });
    }

    #[test]
    fn snippet_returns_the_spanned_source() {
        assert_eq!(file("let x = 1;").snippet(Span::new(4, 5)), Some("x"));
    }

    /// Spans only ever originate at token boundaries, but the emitter must not
    /// be able to panic while reporting an error.
    #[test]
    fn snippet_of_a_bad_range_is_none() {
        assert_eq!(file("🦀").snippet(Span::new(0, 1)), None);
        assert_eq!(file("ab").snippet(Span::new(0, 99)), None);
    }
}
