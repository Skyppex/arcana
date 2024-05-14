mod common;

use common::{create_typed_ast, evaluate_expression, StatementExt, VecStatementExt};

use shared::type_checker::{ast::TypedExpression, Type};
use interpreter::{value, Value};

use crate::common::create_env;

#[test]
fn if_is_if() {
    // Arrange
    let input = "if true { 1 } else { 2 }";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast.unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::If { .. }));
}

#[test]
fn if_has_correct_type() {
    // Arrange
    let input = "if true { 1 } else { 2 }";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast.unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    match expression {
        TypedExpression::If { type_, ..} => {
            assert_eq!(type_, Type::Literal { name: "1".to_owned(), type_: Box::new(Type::Int) });
        },
        _ => panic!("Expected an if expression, but found {:?}", expression),
    }
}

#[test]
fn if_returns_if_block() {
    // Arrange
    let input = "if true { 1 } else { 2 }";
    let environment = create_env();

    // Act
    let value = evaluate_expression(input, environment, false);

    // Assert
    assert_eq!(value, Value::Number(value::Number::Int(1)));
}

#[test]
fn if_returns_else_block() {
    // Arrange
    let input = "if false { 1 } else { 2 }";
    let environment = create_env();

    // Act
    let value = evaluate_expression(input, environment, false);

    // Assert
    assert_eq!(value, Value::Number(value::Number::Int(2)));
}

#[test]
fn if_returns_else_if_block() {
    // Arrange
    let input = "if false { 1 } else if true { 2 } else { 3 }";
    let environment = create_env();

    // Act
    let value = evaluate_expression(input, environment, false);

    // Assert
    assert_eq!(value, Value::Number(value::Number::Int(2)));
}

#[test]
fn if_returns_option_some() {
    // Arrange
    let input = "if true { 1 }";
    let environment = create_env();

    // Act
    let value = evaluate_expression(input, environment, false);

    // Assert
    assert_eq!(value, Value::option_some(Value::Number(value::Number::Int(1))));
}

#[test]
fn if_returns_option_some_2() {
    // Arrange
    let input = "if false { 1 } else if true { 2 }";
    let environment = create_env();

    // Act
    let value = evaluate_expression(input, environment, false);

    // Assert
    assert_eq!(value, Value::option_some(Value::Number(value::Number::Int(2))));
}

#[test]
fn if_returns_option_none() {
    // Arrange
    let input = "if false { 1 }";
    let environment = create_env();

    // Act
    let value = evaluate_expression(input, environment, false);

    // Assert
    assert_eq!(value, Value::option_none());
}

#[test]
fn if_returns_option_none_2() {
    // Arrange
    let input = "if false { 1 } else if false { 2 }";
    let environment = create_env();

    // Act
    let value = evaluate_expression(input, environment, false);

    // Assert
    assert_eq!(value, Value::option_none());
}
