mod common;

use common::{create_env, create_typed_ast, evaluate_expression, StatementExt, VecStatementExt};

use interpreter::{value::Number, Value};
use shared::type_checker::{
    ast::{Typed, TypedExpression},
    Type,
};

#[test]
fn call_is_call() {
    // Arrange
    let input = r#"
        fun a() => {}
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
        fun a() =>  {}
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
        fun a(): bool => { true }
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

#[test]
fn call_adds_variables_to_called_function() {
    // Arrange
    let input = r#"
        fun a(x: int): int => x
        let y: int = 5;
        a(y)
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(5)))
}

#[test]
#[should_panic(expected = "Variable 'x' not found")]
fn call_doesnt_use_external_environment_inside_function_which_is_called() {
    // Arrange
    let input = r#"
        fun a(x: int): int => x
        let x: int = 5;
        a()
    "#;

    // Act
    evaluate_expression(input, create_env(), false); // This should panic
}

#[test]
fn call_takes_caller_variable_as_first_argument_using_function_propagation() {
    // Arrange
    let input = r#"
        fun a(x: int): int => x
        let y: int = 5;
        y:a()
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(5)))
}

#[test]
fn function_propagation_works_with_multiple_arguments() {
    // Arrange
    let input = r#"
        fun a(x: int, y: int): int => x + y
        let y: int = 5;
        y:a(3)
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(8)))
}

#[test]
fn function_propagation_can_be_chained() {
    // Arrange
    let input = r#"
        fun a(x: int): int => x
        let y: int = 5;
        y:a():a()
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(5)))
}

#[test]
fn function_propagation_can_be_chained_using_function_with_multiple_arguments() {
    // Arrange
    let input = r#"
        fun a(x: int, y: int): int => x + y
        let y: int = 5;
        y:a(3):a(2)
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(10)))
}

#[test]
fn function_propagation_has_correct_type_using_function_with_multiple_arguments() {
    // Arrange
    let input = r#"
        fun a(x: int, y: int): int => x + y
        let y: int = 5;
        y:a(3)
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
fn call_takes_caller_expression_as_first_argument() {
    // Arrange
    let input = r#"
        fun a(x: int): int => x
        8:a()
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(8)))
}
