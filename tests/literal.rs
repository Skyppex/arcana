mod common;

use common::{create_typed_ast, evaluate_expression, StatementExt, VecStatementExt};

use interpreter::{value, Value};
use shared::type_checker::ast::{TypedExpression, ValueLiteral};

use crate::common::create_env;

#[test]
fn literal_is_literal() {
    // Arrange
    let input = "1";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Literal { .. }));
}

#[test]
fn literal_has_correct_type() {
    // Arrange
    let input = "1";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    match expression {
        TypedExpression::Literal(literal) => {
            assert!(matches!(literal, ValueLiteral::Int(_)));
        }
        _ => panic!("Expected a literal expression, but found {:?}", expression),
    }
}

#[test]
fn literal_returns_correct_value() {
    // Arrange
    let input = "1";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(value::Number::Int(1)));
}
