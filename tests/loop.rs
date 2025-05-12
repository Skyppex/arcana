mod common;

use common::{create_typed_ast, StatementExt, VecStatementExt};

use shared::type_checker::{
    model::{Typed, TypedExpression},
    Type,
};

#[test]
fn loop_is_loop() {
    // Arrange
    let input = "loop {}";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Loop { .. }));
}

#[test]
fn loop_has_correct_type() {
    // Arrange
    let input = "loop { 1 }";

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
fn loop_has_correct_type_when_using_break() {
    // Arrange
    let input = "loop { break 1 + 1 }";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}
