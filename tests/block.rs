mod common;

use common::{create_typed_ast, StatementExt, VecStatementExt};

use shared::type_checker::{
    model::{Typed, TypedExpression},
    Type,
};

#[test]
fn block_is_block() {
    // Arrange
    let input = "{}";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Block { .. }));
}

#[test]
fn block_has_correct_type() {
    // Arrange
    let input = "{ 1 }";

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
            name: "1".into(),
            type_: Box::new(shared::type_checker::LiteralType::IntValue(1))
        }
    );
}
