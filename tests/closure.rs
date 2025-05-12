mod common;

use common::{create_env, create_typed_ast, evaluate_expression, StatementExt, VecStatementExt};
use interpreter::{value::Number, Value};
use shared::type_checker::{
    model::{Typed, TypedExpression},
    Function, Parameter, Type,
};

#[test]
fn closure_is_closure() {
    // Arrange
    // lang=arcana
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
    // lang=arcana
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
    // lang=arcana
    let input = "|x: Int|: Int x";

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
fn closure_evaluates_correctly() {
    // Arrange
    // lang=arcana
    let input = "(|x: Int|: Int x)(1)";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(1)));
}

#[test]
fn closure_captures_environment() {
    // Arrange
    // lang=arcana
    let input = r#"
        let x: Int = 1;
        (||: Int x)()
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(1)));
}

#[test]
fn closure_captures_function_environment() {
    // Arrange
    // lang=arcana
    let input = r#"
        fun add(x: Int): fun(Int): Int => {
            |y: Int|: Int x + y
        }

        add(1)(2)
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(3)));
}

#[test]
fn closure_with_multiple_params_evaluates_correctly() {
    // Arrange
    // lang=arcana
    let input = r#"
        (|x: Int, y: Int, z: Int|: Int x + y + z)(1, 2, 3)
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(6)));
}

#[test]
fn closure_return_type_is_inferred() {
    // Arrange
    // lang=arcana
    let input = r#"
        fun a(op: fun(): Int): Int => op()
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
    // lang=arcana
    let input = r#"
        fun a(op: fun(Int): Int): Int => op(8)
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
    // lang=arcana
    let input = r#"
        fun a(op: fun(Int)) => op(0)
        a(|x| x)
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Void);
}

#[test]
fn trailing_closures_can_be_chained() {
    // Arrange
    // lang=arcana
    let input = r#"
        fun add(left: fun(): Int, right: fun(): Int): Int => left() + right()
        add -> 1 -> 2
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(3)));
}
