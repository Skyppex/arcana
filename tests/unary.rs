mod common;

use common::{create_typed_ast, StatementExt, VecStatementExt};

use interpreter::{value::Number, Value};
use shared::type_checker::{
    ast::{Typed, TypedExpression, UnaryOperator},
    Type,
};

#[test]
fn identity_is_identity() {
    // Arrange
    let input = "+1";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Unary { operator: UnaryOperator::Identity, .. }));
}

#[test]
fn negate_is_negate() {
    // Arrange
    let input = "-1";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Unary { operator: UnaryOperator::Negate, .. }));
}

#[test]
fn logical_not_is_logical_not() {
    // Arrange
    let input = "!true";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Unary { operator: UnaryOperator::LogicalNot, .. }));
}

#[test]
fn bitwise_not_is_bitwise_not() {
    // Arrange
    let input = "~1";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Unary { operator: UnaryOperator::BitwiseNot, .. }));
}

#[test]
fn identity_has_correct_type() {
    // Arrange
    let input = "+1";

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
fn negate_has_correct_type() {
    // Arrange
    let input = "-1";

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
fn logical_not_has_correct_type() {
    // Arrange
    let input = "!true";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Bool);
}

#[test]
fn bitwise_not_has_correct_type() {
    // Arrange
    let input = "~1";

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
fn identity_returns_correct_value() {
    // Arrange
    let input = "+1";

    // Act
    let value = common::evaluate_expression(input, common::create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(1)));
}

#[test]
fn negate_returns_correct_value() {
    // Arrange
    let input = "-1";

    // Act
    let value = common::evaluate_expression(input, common::create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(-1)));
}

#[test]
fn logical_not_returns_correct_value() {
    // Arrange
    let input = "!true";

    // Act
    let value = common::evaluate_expression(input, common::create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(false));
}

#[test]
fn bitwise_not_returns_correct_value() {
    // Arrange
    let input = "~1";

    // Act
    let value = common::evaluate_expression(input, common::create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(-2)));
}
