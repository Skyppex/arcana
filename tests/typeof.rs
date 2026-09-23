mod common;

use common::{
    create_env, create_typed_ast, evaluate_expression, try_create_typed_ast, StatementExt,
    VecStatementExt,
};

use interpreter::Value;
use shared::type_checker::{model::Typed, LiteralType, Type};

fn string(v: &str) -> Value {
    Value::String(v.to_owned())
}

fn assert_error_contains(input: &str, needle: &str) {
    match try_create_typed_ast(input) {
        Ok(_) => panic!("expected an error mentioning {needle:?}, but the program type-checked"),
        Err(e) => assert!(
            e.contains(needle),
            "expected an error mentioning {needle:?}, got: {e}"
        ),
    }
}

// --- Both call forms --------------------------------------------------------

#[test]
fn typeof_can_be_called_directly() {
    // Arrange
    let input = r#"
        let x: Int = 4;
        typeof(x)
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("Int"));
}

#[test]
fn typeof_can_be_called_by_propagation() {
    // Arrange
    let input = r#"
        let x: Int = 4;
        x:typeof()
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("Int"));
}

#[test]
fn both_call_forms_agree_on_a_literal() {
    // Arrange
    // Propagation must fold the same way a direct call does, rather than
    // falling through to the runtime, which only knows the value's shape.
    let direct = evaluate_expression("typeof(4)", create_env(), false);
    let propagated = evaluate_expression("4:typeof()", create_env(), false);

    // Assert
    assert_eq!(direct, string("#4"));
    assert_eq!(propagated, direct);
}

#[test]
fn both_call_forms_agree_on_a_struct() {
    // Arrange
    let direct = evaluate_expression(
        r#"
        struct Point { x: Int }
        let p: Point = Point { x: 1 };
        typeof(p)
        "#,
        create_env(),
        false,
    );

    let propagated = evaluate_expression(
        r#"
        struct Point { x: Int }
        let p: Point = Point { x: 1 };
        p:typeof()
        "#,
        create_env(),
        false,
    );

    // Assert
    assert_eq!(direct, string("Point { x }"));
    assert_eq!(propagated, direct);
}

// --- It cannot escape into the program --------------------------------------
//
// `typeof` is answered while checking, so there is no value to carry into the
// running program. Every use that would need one is rejected rather than
// silently answered from the value's runtime shape, which knows nothing of
// literal types or generics.

#[test]
fn typeof_cannot_be_bound_as_a_value() {
    // Arrange
    let input = "let f = typeof;";

    // Act & Assert
    assert_error_contains(input, "cannot be used as a value");
}

#[test]
fn typeof_cannot_be_referenced_bare() {
    // Arrange
    let input = "typeof";

    // Act & Assert
    assert_error_contains(input, "cannot be used as a value");
}

#[test]
fn typeof_cannot_be_called_through_a_binding() {
    // Arrange
    let input = r#"
        let f = typeof;
        f(4)
    "#;

    // Act & Assert
    assert_error_contains(input, "cannot be used as a value");
}

#[test]
fn typeof_propagation_must_be_called() {
    // Arrange
    // `:typeof` without parentheses would leave it standing as a value.
    let input = "4:typeof";

    // Act & Assert
    assert_error_contains(input, "must be called");
}

#[test]
fn typeof_needs_an_argument() {
    // Arrange
    let input = "typeof()";

    // Act & Assert
    assert_error_contains(input, "needs a value to report on");
}

// --- The result is a string literal type ------------------------------------

#[test]
fn typeof_has_a_string_literal_type() {
    // Arrange
    // `typeof` is answered while checking, so the call folds into the string it
    // produced and carries that string as its type.
    let input = r#"
        let x: Int = 4;
        typeof(x)
    "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(1)
        .unwrap_expression();

    assert_eq!(
        expression.get_type(),
        Type::Literal {
            name: "Int".to_owned(),
            type_: Box::new(LiteralType::StringValue("Int".to_owned()))
        }
    );
}

#[test]
fn typeof_by_propagation_has_a_string_literal_type() {
    // Arrange
    let input = "4:typeof()";

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
            name: "#4".to_owned(),
            type_: Box::new(LiteralType::StringValue("#4".to_owned()))
        }
    );
}

#[test]
fn typeof_a_literal_has_a_string_literal_type_of_the_literal_type() {
    // Arrange
    let input = "typeof(4)";

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
            name: "#4".to_owned(),
            type_: Box::new(LiteralType::StringValue("#4".to_owned()))
        }
    );
}

// --- What it reports --------------------------------------------------------

#[test]
fn typeof_reports_a_literal_type() {
    // Arrange
    let input = "typeof(4)";

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("#4"));
}

#[test]
fn typeof_quotes_string_and_rune_literal_types() {
    // Arrange
    // A literal type renders the way the literal is written, so a string keeps
    // its quotes and a rune its ticks — `#"hello"`, not `#hello`.
    let string_literal = evaluate_expression(r#"typeof("hello")"#, create_env(), false);
    let rune_literal = evaluate_expression("typeof('a')", create_env(), false);

    // Assert
    assert_eq!(string_literal, string("#\"hello\""));
    assert_eq!(rune_literal, string("#'a'"));
}

#[test]
fn typeof_of_typeof_is_a_string_literal_of_a_literal() {
    // Arrange
    let input = "typeof(typeof(4))";

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("#\"#4\""));
}

#[test]
fn typeof_reports_primitive_types() {
    // Arrange
    let input = r#"
        let i: Int = 1;
        let f: Float = 1f;
        let s: String = "a";
        let b: Bool = true;
        typeof(i) + typeof(f) + typeof(s) + typeof(b)
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("IntFloatStringBool"));
}

#[test]
fn typeof_reports_an_array_type() {
    // Arrange
    let input = r#"
        let xs: [Int] = [1, 2];
        typeof(xs)
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("[Int]"));
}

#[test]
fn typeof_reports_a_struct_type() {
    // Arrange
    let input = r#"
        struct Point { x: Int }
        let p: Point = Point { x: 1 };
        typeof(p)
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("Point { x }"));
}

#[test]
fn typeof_reports_an_enum_variant_type() {
    // Arrange
    // Without an annotation the value keeps its variant type, so `typeof`
    // reports the variant rather than the enum.
    let input = r#"
        enum MyEnum { First, Second }
        let e = MyEnum::First;
        typeof(e)
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("MyEnum::First"));
}

// --- Composition ------------------------------------------------------------

#[test]
fn typeof_can_be_printed() {
    // Arrange
    let input = r#"
        let x: Int = 4;
        println(typeof(x))
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Void);
}

#[test]
fn typeof_can_be_bound_to_a_variable() {
    // Arrange
    let input = r#"
        let t = typeof(4);
        t
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("#4"));
}
