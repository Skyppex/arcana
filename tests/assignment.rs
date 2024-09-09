mod common;

use common::{create_typed_ast, evaluate_expression, StatementExt, VecStatementExt};

use interpreter::{value, Value};
use shared::type_checker::{
    ast::{Literal, Member, TypedExpression},
    Type,
};

use crate::common::create_env;

#[test]
fn assignment_is_assignment() {
    // Arrange
    let input = r#"
        let x: Int;
        x = 1
    "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(1)
        .unwrap_expression();

    assert_eq!(
        expression,
        TypedExpression::Assignment {
            member: Box::new(Member::Identifier {
                symbol: "x".to_owned(),
                type_: Type::Int
            }),
            initializer: Box::new(TypedExpression::Literal(Literal::Int(1))),
            type_: Type::Literal {
                name: "1".to_owned(),
                type_: Box::new(Type::Int)
            }
        }
    );
}

#[test]
fn assignment_has_correct_type() {
    // Arrange
    let input = r#"
        let x: Int;
        x = 1
    "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(1)
        .unwrap_expression();

    match expression {
        TypedExpression::Assignment { type_, .. } => {
            assert_eq!(
                type_,
                Type::Literal {
                    name: "1".to_owned(),
                    type_: Box::new(Type::Int)
                }
            );
        }
        _ => panic!(
            "Expected an assignment expression, but found {:?}",
            expression
        ),
    }
}

#[test]
fn assignment_returns_assigned_value() {
    // Arrange
    let input = r#"
        let x: Int;
        x = 1
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(value::Number::Int(1)));
}
