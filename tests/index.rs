mod common;

use common::{create_typed_ast, StatementExt, VecStatementExt};

use interpreter::{value::Number, Value};
use shared::type_checker::{
    ast::{Typed, TypedExpression},
    Type,
};

use crate::common::{create_env, evaluate_expression};

#[test]
fn index_is_index() {
    // Arrange
    let input = r#"
        let x: [int] = [1];
        x[0u]
    "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(1)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Index { .. }));
}

#[test]
fn index_has_correct_type() {
    // Arrange
    let input = r#"
        let x: [int] = [1];
        x[0u]
    "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(1)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}

#[test]
fn index_returns_correct_value() {
    // Arrange
    let input = r#"
        let x: [int] = [1];
        x[0u]
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(1)));
}
