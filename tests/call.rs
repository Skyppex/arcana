mod common;

use common::{create_typed_ast, StatementExt, VecStatementExt};

use shared::type_checker::{
    ast::{Typed, TypedExpression},
    Type,
};


#[test]
fn call_is_call() {
    // Arrange
    let input = r#"
        func a() {}
        a()
    "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(1)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Call { .. }));
}

#[test]
fn call_has_void_return_type() {
    // Arrange
    let input = r#"
        func a() {}
        a()
    "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(1)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Void);
}

#[test]
fn call_has_return_type() {
    // Arrange
    let input = r#"
        func a(): bool { true }
        a()
    "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(1)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Bool);
}
