mod common;

use common::{create_typed_ast, evaluate_expression, StatementExt, VecStatementExt};

use interpreter::{value, Value};
use shared::type_checker::{
    ast::{Member, Typed, TypedExpression},
    Type,
};

use crate::common::create_env;

#[test]
fn identifier_is_identifier() {
    // Arrange
    let input = r#"
        let a: int = 1;
        a
    "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(1)
        .unwrap_expression();

    assert!(matches!(
        expression,
        TypedExpression::Member(Member::Identifier { .. })
    ));
}

#[test]
fn member_access_is_member_access() {
    // Arrange
    let input = r#"
        struct A { a: int }
        let a: A = A { a: 1 };
        a.a
    "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(2)
        .unwrap_expression();

    assert!(matches!(
        expression,
        TypedExpression::Member(Member::MemberAccess { .. })
    ));
}

#[test]
fn identifier_has_correct_type() {
    // Arrange
    let input = r#"
        let a: int = 1;
        a
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
fn member_access_has_correct_type() {
    // Arrange
    let input = r#"
        struct A { a: int }
        let a: A = A { a: 1 };
        a.a
    "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(2)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}

#[test]
fn identifier_returns_correct_value() {
    // Arrange
    let input = r#"
        let a: int = 1;
        a
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(value::Number::Int(1)));
}

#[test]
fn member_access_returns_correct_value() {
    // Arrange
    let input = r#"
        struct A { a: int }
        let a: A = A { a: 1 };
        a.a
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(value::Number::Int(1)));
}
