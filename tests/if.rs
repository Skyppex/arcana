mod common;

use common::{create_typed_ast, evaluate_expression, StatementExt, VecStatementExt};

use interpreter::{value, Value};
use shared::type_checker::{
    ast::{Typed, TypedExpression},
    Type,
};

use crate::common::create_env;

#[test]
fn if_is_if() {
    // Arrange
    let input = "if true => { 1 } else { 2 }";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::If { .. }));
}

#[test]
fn if_has_correct_type() {
    // Arrange
    let input = "if true => { 1 } else { 2 }";

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
fn if_has_correct_type_2() {
    // Arrange
    let input = "if true => 1 else => 2";

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
fn if_returns_if_block() {
    // Arrange
    let input = "if true => { 1 } else { 2 }";

    // Act
    let value = evaluate_expression(input, create_env(), false);
    // Assert
    assert_eq!(value, Value::Number(value::Number::Int(1)));
}

#[test]
fn if_returns_else_block() {
    // Arrange
    let input = "if false => { 1 } else { 2 }";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(value::Number::Int(2)));
}

#[test]
fn if_returns_else_if_block() {
    // Arrange
    let input = "if false => { 1 } else if true => { 2 } else { 3 }";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(value::Number::Int(2)));
}

#[test]
fn if_returns_option_some() {
    // Arrange
    let input = "if true => { 1 }";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(
        value,
        Value::option_some(Value::Number(value::Number::Int(1)))
    );
}

#[ignore]
fn if_returns_option_some_2() {
    // Arrange
    let input = "if false => { 1 } else if true => { 2 }";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(
        value,
        Value::option_some(Value::Number(value::Number::Int(2)))
    );
}

#[test]
fn if_returns_option_none() {
    // Arrange
    let input = "if false => { 1 }";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::option_none());
}

#[ignore]
fn if_returns_option_none_2() {
    // Arrange
    let input = "if false => { 1 } else if false => { 2 }";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::option_none());
}

#[test]
fn if_does_not_require_brackets() {
    // Arrange
    let input = "if true => 1 else => 2";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(value::Number::Int(1)));
}

#[test]
fn if_should_correctly_evaluate_this_nonsense() {
    // Arrange
    let input = "if true => if false => false else => true else => false";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(true));
}

#[test]
fn ifs_are_equals() {
    // Arrange
    let input = "if false => { 1 } else if true => { 2 } else { 3 }";
    let input2 = "if false => { 1 } else { if true => { 2 } else { 3 } }";

    // Act
    let value = evaluate_expression(input, create_env(), false);
    let value2 = evaluate_expression(input2, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(value::Number::Int(2)));
    assert_eq!(value2, Value::Number(value::Number::Int(2)));
    assert_eq!(value, value2);
}
