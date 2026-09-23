mod common;

use common::{
    create_env, create_typed_ast, evaluate_expression, try_create_typed_ast, StatementExt,
    VecStatementExt,
};

use interpreter::{value::Number, Value};
use shared::type_checker::{
    model::{Typed, TypedExpression},
    Type,
};

#[test]
fn for_is_for() {
    // Arrange
    let input = "for x in 0..1 => {}";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::For { .. }));
}

#[test]
fn for_has_correct_type() {
    // Arrange
    let input = "for x in 0..1 => { 1 }";

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
fn for_has_correct_type_when_using_break() {
    // Arrange
    let input = r#"
        for x in 0..1 => {
            break 1 + 1
        } else {
            2 + 2
        }
        "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}

#[test]
fn for_does_not_require_body_or_else_body_to_be_a_block() {
    // Arrange
    let input = r#"
        for x in 0..1 => 1 + 1
        else => 2 + 2
        "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}

#[test]
#[should_panic]
fn for_should_panic_if_else_body_has_different_type_than_body() {
    // Arrange
    let input = r#"
        for x in 0..1 => {
            break 1 + 1
        } else {
            "Hello, World!"
        }
        "#;

    // Act
    create_typed_ast(input); // panics
}

#[test]
fn for_returns_void() {
    // Arrange
    let input = r#"
        for x in 0..1 => {
            1 + 1
        }
        "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Void);
}

#[test]
fn for_returns_break_value() {
    // Arrange
    let input = r#"
        for x in 0..1 => {
            break 1 + 1
        } else {
            2 + 2
        }
        "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(2)));
}

#[test]
fn for_returns_else_value_after_iteration() {
    // Arrange
    let input = r#"
        for x in 0..1 => {
            if x > 10 => {
                break 1 + 1
            }
        } else {
            2 + 2
        }
        "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(4)));
}

// ---------------------------------------------------------------------------
// Destructuring bindings
//
// A loop binds each element through the same irrefutable-pattern path that a
// declaration uses, so the same forms are available.
// ---------------------------------------------------------------------------

#[test]
fn for_destructures_a_tuple() {
    // Arrange
    let input = r#"
        let pairs: [(Int, Int)] = [(1, 2)];
        for (a, b) in pairs => { break a + b } else { 0 }
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(Number::Int(3)));
}

#[test]
fn for_tuple_elements_may_be_wildcards() {
    // Arrange
    let input = r#"
        let pairs: [(Int, Int)] = [(1, 2)];
        for (_, b) in pairs => { break b } else { 0 }
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(Number::Int(2)));
}

#[test]
fn for_destructures_a_struct() {
    // Arrange
    let input = r#"
        struct Point { x: Int }
        let points: [Point] = [Point { x: 7 }];
        for { x } in points => { break x } else { 0 }
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(Number::Int(7)));
}

#[test]
fn for_destructures_a_named_struct() {
    // Arrange
    let input = r#"
        struct Point { x: Int }
        let points: [Point] = [Point { x: 7 }];
        for Point { x } in points => { break x } else { 0 }
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(Number::Int(7)));
}

#[test]
fn for_destructures_nested_patterns() {
    // Arrange
    let input = r#"
        struct Point { x: Int }
        let pairs: [(Point, Int)] = [(Point { x: 5 }, 2)];
        for ({ x }, b) in pairs => { break x + b } else { 0 }
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(Number::Int(7)));
}

#[test]
fn for_rejects_a_refutable_pattern() {
    // Arrange
    // A loop binds unconditionally, so a pattern that can fail has nowhere to
    // fail to.
    let input = r#"
        let xs: [Int] = [1, 2];
        for 1 in xs => { break 1 } else { 0 }
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}
