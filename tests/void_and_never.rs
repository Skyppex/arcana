//! `Void`, `Never` and `Unit`.
//!
//! Three types that used to do each other's jobs:
//!
//! - **`Void`** — there is no value here. Uninhabited, produced by the compiler
//!   for anything that does not yield one, and bindable to nothing.
//! - **`Never`** — control never gets here. Uninhabited, and satisfies every
//!   expectation precisely because no value ever arrives to contradict it.
//! - **`Unit`** — the ordinary value `unit`. Belongs to the programmer; nothing
//!   in the compiler should produce it on their behalf.

mod common;

use common::{
    create_env, create_typed_ast, evaluate_expression, try_create_typed_ast, StatementExt,
};
use interpreter::Value;
use shared::type_checker::{model::Typed, Type};

/// The type of the last statement of a program.
fn type_of(input: &str) -> Type {
    create_typed_ast(input)
        .unwrap_program()
        .last()
        .unwrap()
        .clone()
        .get_type()
}

// ---------------------------------------------------------------------------
// Void
// ---------------------------------------------------------------------------

#[test]
fn a_function_with_no_return_annotation_returns_void() {
    assert_eq!(type_of("fun noop() => { }; noop()"), Type::Void);
}

#[test]
fn void_may_be_written_explicitly() {
    assert_eq!(type_of("fun noop(): Void => { }; noop()"), Type::Void);
}

/// The whole point of `Void` being unrepresentable.
#[test]
fn a_void_cannot_be_bound() {
    let error = try_create_typed_ast("fun noop() => { }; let x = noop();")
        .unwrap_err()
        .to_string();

    assert!(error.contains("no value to bind"), "{error}");
}

#[test]
fn a_void_cannot_be_assigned() {
    let error = try_create_typed_ast("let mut x = 1; fun noop() => { }; x = noop();")
        .unwrap_err()
        .to_string();

    assert!(error.contains("no value to assign"), "{error}");
}

/// Calling a `Void` function is the normal thing to do with one — it is only
/// keeping the result that is rejected.
#[test]
fn a_void_call_is_fine_as_a_statement() {
    assert!(try_create_typed_ast(r#"fun noop() => { }; noop(); "ok""#).is_ok());
}

#[test]
fn the_printing_builtins_are_void() {
    assert_eq!(type_of(r#"println("hi")"#), Type::Void);
    assert_eq!(
        evaluate_expression(r#"println("hi")"#, create_env(), false),
        Value::None
    );
}

#[test]
fn a_void_function_yields_no_value_whatever_its_body_computed() {
    // `op` is annotated `fun(Int)` — no return type, so `Void` — and the
    // closure passed in returns its argument. The call still has nothing.
    let input = r#"
        fun a(op: fun(Int)) => op(0)
        a(|x| x)
    "#;

    assert_eq!(evaluate_expression(input, create_env(), false), Value::None);
}

// ---------------------------------------------------------------------------
// Never
// ---------------------------------------------------------------------------

#[test]
fn a_loop_with_no_break_is_never() {
    assert_eq!(type_of("loop { }"), Type::Never);

    // A `Never` body satisfies any declared return type, including `Never`.
    assert!(try_create_typed_ast("fun spin(): Never => loop { }; 0").is_ok());
    assert!(try_create_typed_ast("fun spin(): Int => loop { }; 0").is_ok());
}

#[test]
fn a_loop_that_breaks_without_a_value_is_void_not_never() {
    let error = try_create_typed_ast("fun f(): Int => loop { break; }; 0")
        .unwrap_err()
        .to_string();

    assert!(error.contains("Void"), "{error}");
}

#[test]
fn a_loop_that_breaks_with_a_value_has_that_type() {
    assert_eq!(
        evaluate_expression("loop { break 5; }", create_env(), false),
        Value::Number(interpreter::value::Number::Int(5))
    );
}

/// `Never` satisfies every expectation, which is what makes an early `return`
/// usable inside an expression. Before the split this was an error.
#[test]
fn an_early_return_can_appear_in_a_branch() {
    let input = r#"
        fun pick(c: Bool): Int => {
            let x = if c { 1 } else { return 0 };
            x + 10
        };
        pick(true)
    "#;

    assert_eq!(
        evaluate_expression(input, create_env(), false),
        Value::Number(interpreter::value::Number::Int(11))
    );
}

/// Deliberately *not* checked: proving a `Never` function never returns needs
/// reachability analysis. The annotation is accepted and the body is not
/// verified — this test pins that as a decision rather than an oversight.
#[test]
fn a_never_function_whose_body_returns_is_not_rejected_yet() {
    assert!(try_create_typed_ast("fun wrong(): Never => 1; 0").is_ok());
}

// ---------------------------------------------------------------------------
// Unit
// ---------------------------------------------------------------------------

#[test]
fn unit_is_an_ordinary_value() {
    assert_eq!(type_of("let u = unit; u"), Type::Unit);
    assert_eq!(
        evaluate_expression("let u = unit; u", create_env(), false),
        Value::Unit
    );
}

#[test]
fn unit_may_be_annotated_and_compared() {
    assert_eq!(
        evaluate_expression("let u: Unit = unit; u == unit", create_env(), false),
        Value::Bool(true)
    );
}

/// The distinction that was lost before: a function that returns nothing is not
/// a function that returns `unit`.
#[test]
fn void_and_unit_are_not_the_same_type() {
    assert_ne!(type_of("fun noop() => { }; noop()"), Type::Unit);

    let error = try_create_typed_ast("fun noop(): Unit => { }; 0")
        .unwrap_err()
        .to_string();

    assert!(error.contains("Unit"), "{error}");
}
