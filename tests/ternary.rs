mod common;

use common::{create_typed_ast, StatementExt, VecStatementExt};
use interpreter::{value, Value};
use shared::type_checker::ast::{Typed, TypedExpression};
use shared::type_checker::Type;

use crate::common::{create_env, evaluate_expression};

#[test]
fn ternary_is_ternary() {
    // Arrange
    let input = "true ? 1 : 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Ternary { .. }));
}

#[test]
fn ternary_has_correct_type() {
    // Arrange
    let input = "true ? 1 : 2";

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
            type_: Box::new(Type::Int)
        }
    );
}

#[test]
fn ternary_returns_true_expression() {
    // Arrange
    let input = "true ? 1 : 2";
    // Act
    let value = evaluate_expression(input, create_env(), false);
    // Assert
    assert_eq!(value, Value::Number(value::Number::Int(1)));
}

#[test]
fn ternary_returns_false_expression() {
    // Arrange
    let input = "false ? 1 : 2";
    // Act
    let value = evaluate_expression(input, create_env(), false);
    // Assert
    assert_eq!(value, Value::Number(value::Number::Int(2)));
}

#[test]
fn ternary_should_correctly_evaluate_this_nonsense() {
    // Arrange
    let input = "true ? false ? false : true : false";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(true));
}
