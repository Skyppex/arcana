mod common;

use common::{create_env, create_typed_ast, evaluate_expression, StatementExt, VecStatementExt};

use interpreter::{value::Number, Value};
use shared::type_checker::{
    model::{Typed, TypedExpression},
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
        fun a(): Bool => { true }
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
        fun a(x: Int): Int => x
        let y: Int = 5;
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
        fun a(x: Int): Int => x
        let x: Int = 5;
        a()
    "#;

    // Act
    evaluate_expression(input, create_env(), false); // This should panic
}

#[test]
fn call_takes_caller_variable_as_first_argument_using_function_propagation() {
    // Arrange
    let input = r#"
        fun a(x: Int): Int => x
        let y: Int = 5;
        y:a
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
        fun a(x: Int, y: Int): Int => x + y
        let y: Int = 5;
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
        fun a(x: Int): Int => x + 1
        let y: Int = 5;
        y:a:a
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(7)))
}

#[test]
fn function_propagation_can_be_chained_using_function_with_multiple_arguments() {
    // Arrange
    let input = r#"
        fun a(x: Int, y: Int): Int => x + y
        let y: Int = 5;
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
        fun a(x: Int, y: Int): Int => x + y
        let y: Int = 5;
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
        fun a(x: Int): Int => x
        8:a
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(8)))
}

#[test]
fn trailing_closure_call() {
    // Arrange
    let input = r#"
        fun a(op: fun(): Int): Int => op()
        a -> :Int 8
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(8)))
}

#[test]
fn trailing_closure_call_return_type_is_inferred() {
    // Arrange
    let input = r#"
        fun a(op: fun(): Int): Int => op()
        a -> 5
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(5)))
}

#[test]
fn call_with_multiple_params_expects_args_in_correct_order() {
    // Arrange
    let input = r#"
        fun a(x: String, y: Int, z: Float): Int => 1
        a("String", 5, 0.5)
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(1)))
}
