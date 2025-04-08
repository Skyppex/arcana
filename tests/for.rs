mod common;

use common::{create_env, create_typed_ast, evaluate_expression, StatementExt, VecStatementExt};

use interpreter::{value::Number, Value};
use shared::type_checker::{
    ast::{Typed, TypedExpression},
    Type,
};

#[test]
fn for_is_for() {
    // Arrange
    let input = "for x in 0..1 => {}";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::For { .. }));
}

#[test]
fn for_has_correct_type() {
    // Arrange
    let input = "for x in 0..1 => { 1 }";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Void);
}

#[test]
fn for_has_correct_type_when_using_break() {
    // Arrange
    let input = r#"
        for x in 0..1 => {
            break 1 + 1
        } else {
            2 + 2
        }
        "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}

#[test]
fn for_does_not_require_body_or_else_body_to_be_a_block() {
    // Arrange
    let input = r#"
        for x in 0..1 => 1 + 1
        else => 2 + 2
        "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}

#[test]
#[should_panic]
fn for_should_panic_if_else_body_has_different_type_than_body() {
    // Arrange
    let input = r#"
        for x in 0..1 => {
            break 1 + 1
        } else {
            "Hello, World!"
        }
        "#;

    // Act
    create_typed_ast(input); // panics
}

#[test]
fn for_returns_void() {
    // Arrange
    let input = r#"
        for x in 0..1 => {
            1 + 1
        }
        "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Void);
}

#[test]
fn for_returns_break_value() {
    // Arrange
    let input = r#"
        for x in 0..1 => {
            break 1 + 1
        } else {
            2 + 2
        }
        "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(2)));
}

#[test]
fn for_returns_else_value_after_iteration() {
    // Arrange
    let input = r#"
        for x in 0..1 => {
            if x > 10 => {
                break 1 + 1
            }
        } else {
            2 + 2
        }
        "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(4)));
}
