mod common;

use common::{create_typed_ast, evaluate_expression, StatementExt, VecStatementExt};

use interpreter::{value, Value};
use shared::type_checker::{
    model::{Typed, TypedExpression},
    LiteralType, Type,
};

use crate::common::create_env;

#[test]
fn match_is_match() {
    // Arrange
    let input = r#"
        unit match
        | _ => unit
        "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Match { .. }));
}

#[test]
fn match_has_correct_type() {
    // Arrange
    let input = r#"
        unit match
        | _ => unit
        "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Unit);
}

#[test]
fn match_has_correct_type_2() {
    // Arrange
    let input = r#"
        unit match
        | _ => 1
        "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(
        expression.get_type(),
        Type::Literal {
            name: "1".to_owned(),
            type_: Box::new(LiteralType::IntValue(1))
        }
    );
}

#[test]
fn match_returns_arm() {
    // Arrange
    let input = r#"
        unit match
        | _ => "Hello"
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::String("Hello".to_string()));
}

#[test]
fn match_returns_earliest_arm() {
    // Arrange
    let input = r#"
        1 match
        | 1 => "Hello",
        | 1 => "World",
        | _ => "!"
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::String("Hello".to_string()));
}

#[test]
fn match_variable_declaration() {
    // Arrange
    let input = r#"
        3 match
        | 1 => 100,
        | 2 => 200,
        | x => x
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(3)));
}

#[test]
fn match_on_unit() {
    // Arrange
    let input = r#"
        unit match
        | unit => 1,
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

#[test]
fn match_on_bool() {
    // Arrange
    let input = r#"
        true match
        | true => 1,
        | false => 2,
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

#[test]
fn match_on_int() {
    // Arrange
    let input = r#"
        1 match
        | 1 => 1,
        | 2 => 2,
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

#[test]
fn match_on_uint() {
    // Arrange
    let input = r#"
        1u match
        | 1u => 1,
        | 2u => 2,
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

#[test]
fn match_on_float() {
    // Arrange
    let input = r#"
        1f match
        | 1f => 1,
        | 2f => 2,
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

#[test]
fn match_on_char() {
    // Arrange
    let input = r#"
        'a' match
        | 'a' => 1,
        | 'b' => 2,
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

#[test]
fn match_on_string() {
    // Arrange
    let input = r#"
        "Hello" match
        | "Hello" => 1,
        | "World!" => 2,
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

#[test]
fn match_wildcard() {
    // Arrange
    let input = r#"
        unit match
        | _ => 1
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

#[test]
fn match_variable() {
    // Arrange
    let input = r#"
        unit match
        | x => 1
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}
