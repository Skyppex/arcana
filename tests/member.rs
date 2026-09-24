mod common;

use common::{
    create_typed_ast, evaluate_expression, try_create_typed_ast, StatementExt, VecStatementExt,
};

use interpreter::{value, Value};
use shared::type_checker::{
    model::{Member, Typed, TypedExpression},
    Type,
};

use crate::common::create_env;

#[test]
fn identifier_is_identifier() {
    // Arrange
    let input = r#"
        let a: Int = 1;
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
        struct A { a: Int }
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
        let a: Int = 1;
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
        struct A { a: Int }
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
        let a: Int = 1;
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
        struct A { a: Int }
        let a: A = A { a: 1 };
        a.a
    "#;

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(value::Number::Int(1)));
}

// ---------------------------------------------------------------------------
// Field access after every kind of expression
//
// `.field` is a postfix operator, so it has to chain off whatever produced the
// value — not only off a plain identifier.
// ---------------------------------------------------------------------------

/// A struct with a static constructor, a nested struct, and a plain function
/// returning one, to hang the access forms off.
const POINTS: &str = r#"
    proto Newable { fun new(): Self; }
    struct Point { x: Float, y: Float }
    imp Newable for Point { fun new(): Self { Point { x: 1f, y: 2f } } }
    struct Line { start: Point }
    fun make(): Point => Point { x: 1f, y: 2f }
    fun same(p: Point): Point => p
    fun line(): Line => Line { start: Point { x: 1f, y: 2f } }
"#;

fn float(v: f64) -> Value {
    Value::Number(value::Number::Float(v))
}

fn eval_point_field(expression: &str) -> Value {
    evaluate_expression(&format!("{POINTS}{expression}"), create_env(), false)
}

#[test]
fn field_access_on_a_variable() {
    // Act
    let result = eval_point_field("let p = Point::new();\np.x");

    // Assert
    assert_eq!(result, float(1.0));
}

#[test]
fn field_access_on_a_static_call() {
    // Act
    let result = eval_point_field("Point::new().x");

    // Assert
    assert_eq!(result, float(1.0));
}

#[test]
fn field_access_on_a_plain_call() {
    // Act
    let result = eval_point_field("make().x");

    // Assert
    assert_eq!(result, float(1.0));
}

#[test]
fn field_access_on_a_call_with_an_argument() {
    // Act
    let result = eval_point_field("same(Point::new()).x");

    // Assert
    assert_eq!(result, float(1.0));
}

#[test]
fn field_access_on_a_struct_literal() {
    // Act
    let result = eval_point_field("Point { x: 1f, y: 2f }.x");

    // Assert
    assert_eq!(result, float(1.0));
}

#[test]
fn field_access_on_a_generic_call() {
    // Act
    let result = eval_point_field("fun id<T>(v: T): T => v\nid::<Point>(Point::new()).x");

    // Assert
    assert_eq!(result, float(1.0));
}

#[test]
fn field_access_on_an_index() {
    // Act
    let result = eval_point_field("let ps: [Point] = [Point::new()];\nps:[0].x");

    // Assert
    assert_eq!(result, float(1.0));
}

#[test]
fn field_access_on_a_propagated_call() {
    // Act
    let result = eval_point_field("let p = Point::new();\np:same().x");

    // Assert
    assert_eq!(result, float(1.0));
}

#[test]
fn field_access_on_a_parenthesised_expression() {
    // Act
    let result = eval_point_field("(Point::new()).x");

    // Assert
    assert_eq!(result, float(1.0));
}

#[test]
fn field_access_on_a_block() {
    // Act
    let result = eval_point_field("{ Point::new() }.x");

    // Assert
    assert_eq!(result, float(1.0));
}

#[test]
fn field_access_on_a_match() {
    // Act
    let result = eval_point_field("let p = Point::new();\n(p match | _ => p).x");

    // Assert
    assert_eq!(result, float(1.0));
}

// --- Chaining ---------------------------------------------------------------

#[test]
fn nested_field_access_on_a_variable() {
    // Act
    let result = eval_point_field("let l = line();\nl.start.x");

    // Assert
    assert_eq!(result, float(1.0));
}

#[test]
fn nested_field_access_on_a_call() {
    // Act
    let result = eval_point_field("line().start.x");

    // Assert
    assert_eq!(result, float(1.0));
}

#[test]
fn a_call_can_follow_a_field_access() {
    // Act
    let result = eval_point_field("let l = line();\nl.start:same().x");

    // Assert
    assert_eq!(result, float(1.0));
}

#[test]
fn an_unknown_field_is_still_an_error_after_a_call() {
    // Arrange
    let input = format!("{POINTS}Point::new().nope");

    // Act & Assert
    assert!(try_create_typed_ast(&input).is_err());
}
