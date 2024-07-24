use common::TokenExt;
use shared::lexer::token::TokenKind;

mod common;

#[test]
fn line_comment_is_line_comment() {
    // Arrange
    let input = "// This is a comment";

    // Act
    let tokens = common::tokenize(input);

    // Assert
    let comment = tokens.nth_token(0);

    assert_eq!(comment.kind, TokenKind::LineComment);
}

#[test]
fn block_comment_is_block_comment() {
    // Arrange
    let input = "/- This is a comment -/";

    // Act
    let tokens = common::tokenize(input);

    // Assert
    let comment = tokens.nth_token(0);

    assert_eq!(comment.kind, TokenKind::BlockComment);
}

#[test]
#[ignore = "Nested comments are not supported yet"]
fn nested_block_comment_is_block_comment() {
    // Arrange
    let input = "/- This is a comment /- This is a nested comment -/ -/";

    // Act
    let tokens = common::tokenize(input);

    // Assert
    let comment = tokens.nth_token(0);
    println!("{:?}", tokens);

    assert_eq!(tokens.len(), 1);
    assert_eq!(comment.kind, TokenKind::BlockComment);
}

#[test]
fn consecutive_block_comment_is_block_comment() {
    // Arrange
    let input = "/- This is a comment -/ /- This is a nested comment -/";

    // Act
    let tokens = common::tokenize(input);

    // Assert
    let comment = tokens.nth_token(0);

    assert_eq!(comment.kind, TokenKind::BlockComment);
}
