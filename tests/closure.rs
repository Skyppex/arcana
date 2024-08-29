mod common;

use common::{create_env, create_typed_ast, evaluate_expression, StatementExt, VecStatementExt};
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
    let value = evaluate_expression(input, create_env(), false);

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
    let value = evaluate_expression(input, create_env(), false);

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
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(3)));
}

#[test]
fn closure_with_multiple_params_evalutes_correctly() {
    // Arrange
    let input = r#"
        (|x: int, y: int, z: int|: int x + y + z)(1, 2, 3)
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(6)));
}

#[test]
fn closure_return_type_is_inferred() {
    // Arrange
    let input = r#"
        fun a(op: fun(): int): int => op()
        a(|| 5)
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(5)));
}

#[test]
fn closure_param_type_is_inferred() {
    // Arrange
    let input = r#"
        fun a(op: fun(int): int): int => op(8)
        a(|x| x)
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(8)));
}

#[test]
fn closure_voids_body_if_return_type_is_void() {
    // Arrange
    let input = r#"
        fun a(op: fun(int)) => op(0)
        a(|x| x)
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Void);
}

#[test]
fn trailing_closres_can_be_chained() {
    // Arrange
    // lang=arcana
    let input = r#"
        fun add(left: fun(): int, right: fun(): int): int => left() + right()
        add -> 1 -> 2
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(3)));
}
