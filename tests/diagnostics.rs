//! Diagnostics, end to end: a program goes in, a rendered error comes out.
//!
//! These assert the *whole* block rather than a substring, because the point of
//! a diagnostic is the layout — that the carets sit under the right characters
//! is the thing being tested, and a `contains` check would not notice if they
//! drifted.

mod common;

use common::render_error;

#[test]
fn a_lexer_error_points_at_the_character() {
    assert_eq!(
        render_error("let x = 1;\nlet y = `nope;\n"),
        "\
error: Unrecognized character: `
 --> t.ar:2:9
  |
2 | let y = `nope;
  |         ^
  |"
    );
}

#[test]
fn a_parse_error_points_at_the_token_that_was_found() {
    assert_eq!(
        render_error("let x = 10;\nlet y = add(1 2);\n"),
        "\
error: Expected primary expression but found CloseParen
 --> t.ar:2:16
  |
2 | let y = add(1 2);
  |                ^
  |"
    );
}

#[test]
fn an_unknown_variable_points_at_the_name() {
    assert_eq!(
        render_error("let x = 1;\nlet z = x + nope;\n"),
        "\
error: Unexpected variable: nope
 --> t.ar:2:13
  |
2 | let z = x + nope;
  |             ^^^^
  |"
    );
}

#[test]
fn an_unknown_field_points_at_the_access_that_failed() {
    assert_eq!(
        render_error(
            "struct Point { x: Int, y: Int }\nlet p = Point { x: 1, y: 2 };\nlet bad = p.z;\n"
        ),
        "\
error: Struct 'Point' does not have a field called 'z'
 --> t.ar:3:11
  |
3 | let bad = p.z;
  |           ^^^
  |"
    );
}

/// Both halves of the mistake are shown: the binding that failed carries the
/// carets, and the one it collides with gets a secondary label beneath.
#[test]
fn a_duplicate_binding_points_at_both_bindings() {
    assert_eq!(
        render_error(
            "struct Point { x: Int, y: Int }\n\
             let p = Point { x: 1, y: 2 };\n\
             let n = p match\n    \
             | { x: a, y: a } => a;\n"
        ),
        "\
error: Pattern `{ x: a, y: a }` binds `a` more than once
 --> t.ar:4:18
  |
4 |     | { x: a, y: a } => a;
  |            -     ^ bound again here
  |            |
  |            first bound here
  |"
    );
}

/// A `match` spans many lines, and twenty rows of carets would say nothing. The
/// first line is underlined and the rest elided.
#[test]
fn a_multi_line_span_is_truncated_to_its_first_line() {
    assert_eq!(
        render_error(
            "enum MyEnum { First, Second }\n\
             let e: MyEnum = MyEnum::First;\n\
             let n = e match\n    \
             | ::First => 1;\n"
        ),
        "\
error: Match is not exhaustive: no arm covers `MyEnum::Second`
 --> t.ar:3:9
  |
3 | let n = e match
  |         ^^^^^^^...
  |"
    );
}

/// Not every error has a location yet — runtime errors have none at all, since
/// the typed tree carries no spans. Those must still print, as the bare message
/// they have always been.
#[test]
fn an_error_with_no_span_is_just_its_message() {
    let error = shared::diagnostic::Diagnostic::error("something went wrong");

    assert_eq!(
        error.render(&shared::diagnostic::SourceFile::new("t.ar", "let x = 1;")),
        "error: something went wrong"
    );
}
