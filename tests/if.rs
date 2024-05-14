mod common;

use common::{create_rcrc, create_typed_ast, evaluate_expression, StatementExt, VecStatementExt};

use shared::type_checker::{ast::{Literal, TypedExpression}, Type};
use interpreter::{Environment, Value};

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
            assert_eq!(type_, Type::Int);
        },
        _ => panic!("Expected an if expression, but found {:?}", expression),
    }
}
