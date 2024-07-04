mod common;

use common::{create_typed_ast, StatementExt, VecStatementExt};
use interpreter::{value::Number, Value};
use shared::type_checker::{
    ast::{Typed, TypedExpression},
    Function, Parameter, Type,
};

#[test]
fn closure_is_closure() {
    // Arrange

    let input = "|| {}";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Closure { .. }));
}

#[test]
fn closure_has_correct_type() {
    // Arrange
    let input = "|| {}";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(
        expression.get_type(),
        Type::Function(Function {
            identifier: None,
            param: None,
            return_type: Box::new(Type::Void),
        })
    );
}

#[test]
fn closure_has_correct_type_with_param_and_return() {
    // Arrange
    let input = "|x: int|: int x";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(
        expression.get_type(),
        Type::Function(Function {
            identifier: None,
            param: Some(Parameter {
                identifier: "x".to_string(),
                type_: Box::new(Type::Int),
            }),
            return_type: Box::new(Type::Int)
        })
    );
}

#[test]
fn closure_evalutes_correctly() {
    // Arrange
    let input = "(|x: int|: int x)(1)";

    // Act
    let value = common::evaluate_expression(input, common::create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(1)));
}

#[test]
fn closure_captures_environment() {
    // Arrange
    let input = r#"
        let x: int = 1;
        (||: int x)()
    "#;

    // Act
    let value = common::evaluate_expression(input, common::create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(1)));
}

#[test]
fn closure_captures_function_environment() {
    // Arrange
    let input = r#"
        fun add(x: int): fun(int): int => {
            |y: int|: int x + y
        }

        add(1)(2)
    "#;

    // Act
    let value = common::evaluate_expression(input, common::create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(3)));
}
