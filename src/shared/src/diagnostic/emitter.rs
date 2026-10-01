use super::{Diagnostic, Label, SourceFile};

/// How wide a tab is rendered. Both the source line and the caret row expand
/// tabs the same way, or the carets do not line up with what they point at.
const TAB_WIDTH: usize = 4;

/// Renders a diagnostic against the file it came from.
///
/// ```text
/// error: expected `Int`, found `String`
///  --> examples.ar:6:24
///   |
/// 6 | let result = x:mul_add("nope");
///   |                        ^^^^^^ this argument is `String`
///   |
/// ```
pub fn render(diagnostic: &Diagnostic, file: &SourceFile) -> String {
    let mut out = format!("error: {}", diagnostic.message);

    let Some(primary) = &diagnostic.primary else {
        // Nothing to point at. This is what every not-yet-converted error site
        // produces, and it renders exactly like the string it replaced.
        for note in &diagnostic.notes {
            out.push_str(&format!("\n note: {note}"));
        }

        return out;
    };

    let mut lines = annotate(diagnostic, file);
    lines.sort_by_key(|line| line.number);

    let width = lines
        .last()
        .map(|line| line.number.to_string().len())
        .unwrap_or(1);

    let pad = " ".repeat(width);
    let location = file.location(primary.span.start);

    out.push_str(&format!(
        "\n{pad}--> {}:{}",
        file.name,
        // A zero-width span at the end of the file still has to name a place.
        location
    ));
    out.push_str(&format!("\n{pad} |"));

    let mut previous: Option<u32> = None;

    for line in &lines {
        // Non-adjacent annotated lines are separated rather than printing
        // everything in between, which can be the whole file.
        if previous.is_some_and(|previous| line.number > previous + 1) {
            out.push_str("\n...");
        }

        out.push_str(&render_line(line, width));
        previous = Some(line.number);
    }

    out.push_str(&format!("\n{pad} |"));

    for note in &diagnostic.notes {
        out.push_str(&format!("\nnote: {note}"));
    }

    out
}

/// One mark under a source line: where it starts, how wide it is, and what it
/// says.
struct Mark<'a> {
    column: usize,
    width: usize,
    primary: bool,
    /// The span continued past the end of this line.
    truncated: bool,
    message: &'a str,
}

struct AnnotatedLine<'a> {
    number: u32,
    text: String,
    marks: Vec<Mark<'a>>,
}

/// Groups a diagnostic's labels by the line they start on.
fn annotate<'a>(diagnostic: &'a Diagnostic, file: &SourceFile) -> Vec<AnnotatedLine<'a>> {
    let labels = diagnostic
        .primary
        .iter()
        .map(|label| (label, true))
        .chain(diagnostic.secondary.iter().map(|label| (label, false)));

    let mut lines: Vec<AnnotatedLine> = Vec::new();

    for (label, primary) in labels {
        let Some((number, mark)) = mark(label, primary, file) else {
            continue;
        };

        match lines.iter_mut().find(|line| line.number == number) {
            Some(line) => line.marks.push(mark),
            None => {
                let Some(text) = file.line(number) else {
                    continue;
                };

                lines.push(AnnotatedLine {
                    number,
                    text: expand_tabs(text),
                    marks: vec![mark],
                });
            }
        }
    }

    for line in &mut lines {
        line.marks.sort_by_key(|mark| mark.column);
    }

    lines
}

/// Where a label's underline goes on its starting line.
///
/// `None` for a label that cannot be placed — an offset past the end of this
/// file, which is what a span from a *different* file looks like. Dropping it
/// is the only safe move: rendering it here would point at unrelated code.
fn mark<'a>(label: &'a Label, primary: bool, file: &SourceFile) -> Option<(u32, Mark<'a>)> {
    if label.span.start as usize > file.source.len() {
        return None;
    }

    let number = file.location(label.span.start).line;
    let (line_start, line_end) = file.line_range(number)?;

    let start = label.span.start.max(line_start).min(line_end);
    let truncated = label.span.end > line_end;
    let end = label.span.end.min(line_end).max(start);

    let column = visual_width(file.source.get(line_start as usize..start as usize)?);
    let width = visual_width(file.source.get(start as usize..end as usize)?);

    Some((
        number,
        Mark {
            column,
            // A zero-width span — at the end of a line, or at the end of the
            // file — still needs one caret to be visible.
            width: width.max(1),
            primary,
            truncated,
            message: &label.message,
        },
    ))
}

fn render_line(line: &AnnotatedLine, width: usize) -> String {
    let pad = " ".repeat(width);

    let mut out = format!("\n{:>width$} | {}", line.number, line.text);

    // The rightmost mark's message goes inline after the carets; the rest stack
    // below it, each on its own row, connected by vertical bars.
    let inline = line
        .marks
        .last()
        .filter(|mark| !mark.message.is_empty())
        .map(|_| line.marks.len() - 1);

    let mut carets = String::new();

    for mark in &line.marks {
        // Overlapping marks keep their own underline; the later one simply
        // continues from wherever the previous one ended.
        if mark.column > carets.chars().count() {
            carets.push_str(&" ".repeat(mark.column - carets.chars().count()));
        }

        let caret = if mark.primary { '^' } else { '-' };
        carets.push_str(&caret.to_string().repeat(mark.width));

        if mark.truncated {
            carets.push_str("...");
        }
    }

    if let Some(inline) = inline {
        carets.push(' ');
        carets.push_str(line.marks[inline].message);
    }

    out.push_str(&format!("\n{pad} | {}", carets.trim_end()));

    let stacked: Vec<&Mark> = line
        .marks
        .iter()
        .enumerate()
        .filter(|(index, mark)| Some(*index) != inline && !mark.message.is_empty())
        .map(|(_, mark)| mark)
        .collect();

    // Bottom up: the leftmost message ends up furthest down, so every bar above
    // it has something to connect to.
    for index in (0..stacked.len()).rev() {
        let columns: Vec<usize> = stacked[..=index].iter().map(|mark| mark.column).collect();
        out.push_str(&format!("\n{pad} | {}", bars(&columns, None).trim_end()));

        let columns: Vec<usize> = stacked[..index].iter().map(|mark| mark.column).collect();
        let message = Some((stacked[index].column, stacked[index].message));
        out.push_str(&format!("\n{pad} | {}", bars(&columns, message).trim_end()));
    }

    out
}

/// A row of vertical bars at `columns`, optionally ending with a message.
fn bars(columns: &[usize], message: Option<(usize, &str)>) -> String {
    let mut row = String::new();

    for column in columns {
        if *column > row.chars().count() {
            row.push_str(&" ".repeat(column - row.chars().count()));
        }

        row.push('|');
    }

    if let Some((column, message)) = message {
        if column > row.chars().count() {
            row.push_str(&" ".repeat(column - row.chars().count()));
        }

        row.push_str(message);
    }

    row
}

fn expand_tabs(text: &str) -> String {
    text.replace('\t', &" ".repeat(TAB_WIDTH))
}

/// Display width in columns, counting a tab as [`TAB_WIDTH`].
///
/// Every other character counts as one. East Asian wide characters will be
/// off by one per character; correcting that needs a Unicode width table, and
/// the workspace has no dependency for it.
fn visual_width(text: &str) -> usize {
    text.chars()
        .map(|character| if character == '\t' { TAB_WIDTH } else { 1 })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::Span;

    fn at(source: &str, needle: &str) -> Span {
        let start = source.find(needle).expect("needle not in source") as u32;
        Span::new(start, start + needle.len() as u32)
    }

    fn render_against(source: &str, diagnostic: Diagnostic) -> String {
        render(&diagnostic, &SourceFile::new("t.ar", source))
    }

    #[test]
    fn a_diagnostic_with_no_primary_is_just_its_message() {
        assert_eq!(
            render_against("let x = 1;", Diagnostic::error("something went wrong")),
            "error: something went wrong"
        );
    }

    #[test]
    fn a_bare_primary_points_with_carets_and_says_nothing() {
        let source = "let x = y + 1;";

        assert_eq!(
            render_against(source, Diagnostic::error("Unknown variable `y`").at(at(source, "y"))),
            "\
error: Unknown variable `y`
 --> t.ar:1:9
  |
1 | let x = y + 1;
  |         ^
  |"
        );
    }

    #[test]
    fn a_labelled_primary_puts_its_message_after_the_carets() {
        let source = "let result = x:mul_add(\"nope\");";

        assert_eq!(
            render_against(
                source,
                Diagnostic::error("expected `Int`, found `String`")
                    .labelled(at(source, "\"nope\""), "this argument is `String`")
            ),
            "\
error: expected `Int`, found `String`
 --> t.ar:1:24
  |
1 | let result = x:mul_add(\"nope\");
  |                        ^^^^^^ this argument is `String`
  |"
        );
    }

    /// Two labels on one line share a caret row, and the left one's message
    /// drops below so the two do not collide.
    #[test]
    fn two_labels_on_one_line_stack_their_messages() {
        let source = "    Point { x: a, y: a } => a,";

        assert_eq!(
            render_against(
                source,
                Diagnostic::error("`a` is bound more than once in the same pattern")
                    .labelled(Span::new(21, 22), "bound again here")
                    .and(Span::new(15, 16), "first bound here")
            ),
            "\
error: `a` is bound more than once in the same pattern
 --> t.ar:1:22
  |
1 |     Point { x: a, y: a } => a,
  |                -     ^ bound again here
  |                |
  |                first bound here
  |"
        );
    }

    #[test]
    fn labels_on_distant_lines_get_their_own_blocks() {
        let source = "fun f(x: Int): Int => x;\n\n\n\n\nlet y = f(\"nope\");";

        assert_eq!(
            render_against(
                source,
                Diagnostic::error("expected `Int`, found `String`")
                    .labelled(at(source, "\"nope\""), "this argument is `String`")
                    .and(at(source, "x: Int"), "parameter declared here")
            ),
            "\
error: expected `Int`, found `String`
 --> t.ar:6:11
  |
1 | fun f(x: Int): Int => x;
  |       ------ parameter declared here
...
6 | let y = f(\"nope\");
  |           ^^^^^^ this argument is `String`
  |"
        );
    }

    #[test]
    fn notes_follow_the_snippet() {
        let source = "let x = 1;";

        assert_eq!(
            render_against(
                source,
                Diagnostic::error("something").at(at(source, "x")).note("a note")
            ),
            "\
error: something
 --> t.ar:1:5
  |
1 | let x = 1;
  |     ^
  |
note: a note"
        );
    }

    /// The carets have to land under what they point at, so the source line and
    /// the caret row must expand tabs identically.
    #[test]
    fn tabs_are_expanded_in_both_rows() {
        let source = "\tlet x = y;";

        assert_eq!(
            render_against(source, Diagnostic::error("Unknown variable `y`").at(at(source, "y"))),
            "\
error: Unknown variable `y`
 --> t.ar:1:10
  |
1 |     let x = y;
  |             ^
  |"
        );
    }

    #[test]
    fn a_multi_line_span_underlines_the_first_line_only() {
        let source = "match x {\n    1 => 1,\n}";

        assert_eq!(
            render_against(
                source,
                Diagnostic::error("match is not exhaustive")
                    .labelled(Span::new(0, source.len() as u32), "not all cases are covered")
            ),
            "\
error: match is not exhaustive
 --> t.ar:1:1
  |
1 | match x {
  | ^^^^^^^^^... not all cases are covered
  |"
        );
    }

    #[test]
    fn the_gutter_widens_for_longer_line_numbers() {
        let source = "\n".repeat(99) + "let x = y;";

        assert_eq!(
            render_against(&source, Diagnostic::error("Unknown variable `y`").at(at(&source, "y"))),
            "\
error: Unknown variable `y`
   --> t.ar:100:9
    |
100 | let x = y;
    |         ^
    |"
        );
    }

    /// A span from another file has an offset that means nothing here, so it is
    /// dropped rather than pointed at unrelated code.
    #[test]
    fn an_unplaceable_secondary_is_dropped() {
        let source = "let x = y;";

        assert_eq!(
            render_against(
                source,
                Diagnostic::error("Unknown variable `y`")
                    .at(at(source, "y"))
                    .and(Span::new(9_000, 9_010), "declared in another file")
            ),
            "\
error: Unknown variable `y`
 --> t.ar:1:9
  |
1 | let x = y;
  |         ^
  |"
        );
    }
}
