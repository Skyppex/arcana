//! The simplification pass replaces expressions whose value the type system
//! already knows.
//!
//! Its correctness condition is that it changes no observable result, which the
//! rest of the suite covers by running it before every evaluation. These tests
//! are about the other half: that it actually rewrites what it should, and
//! leaves alone what it must.

mod common;

use common::{create_env, create_simplified_ast, evaluate_expression, StatementExt, VecStatementExt};

use interpreter::{value::Number, Value};
use shared::type_checker::model::{Typed, TypedExpression, TypedStatement};

fn int(v: i64) -> Value {
    Value::Number(Number::Int(v))
}

// --- What it rewrites -------------------------------------------------------

#[test]
fn a_binding_whose_value_is_known_is_substituted_and_dropped() {
    // Arrange: `x` has type `#10`, so every use of it is that value and the
    // binding itself has nothing left to do.
    let input = r#"
        let x = 10;
        x + 1
    "#;

    // Act
    let simplified = create_simplified_ast(input);
    let statements = simplified.unwrap_program();

    // Assert: the declaration is gone, and `x + 1` folded to a single literal.
    assert_eq!(statements.len(), 1, "the binding should have been dropped");
    assert!(matches!(
        statements[0].clone().unwrap_expression(),
        TypedExpression::Literal { .. }
    ));
}

#[test]
fn a_known_condition_drops_the_branch_it_never_takes() {
    // Arrange: `a || b` is `#true`, so the else can never run.
    let input = r#"
        let a = true;
        let b = false;
        if a || b { "yup" } else { "nope" }
    "#;

    // Act
    let simplified = create_simplified_ast(input);
    let statements = simplified.unwrap_program();

    // Assert: both bindings dropped, and the `if` replaced by the taken block.
    assert_eq!(statements.len(), 1);
    assert!(
        matches!(
            statements[0].clone().unwrap_expression(),
            TypedExpression::Block(_)
        ),
        "the if should have collapsed to the block it takes"
    );
}

#[test]
fn arithmetic_over_known_values_folds_to_one_literal() {
    // Arrange
    let input = "1 + 2 * 3";

    // Act
    let simplified = create_simplified_ast(input);
    let expression = simplified.unwrap_program().nth_statement(0).unwrap_expression();

    // Assert
    assert!(matches!(expression, TypedExpression::Literal { .. }));
}

// --- What it leaves alone ---------------------------------------------------

#[test]
fn a_mutable_binding_is_not_substituted() {
    // Arrange: a mutable binding widens to `Int`, so its value is not known and
    // the pass has nothing to go on. This is what keeps the rewrite sound
    // without any analysis of assignments.
    let input = r#"
        let mut x = 10;
        x = 20;
        x
    "#;

    // Act
    let simplified = create_simplified_ast(input);
    let statements = simplified.unwrap_program();

    // Assert: nothing was dropped.
    assert_eq!(statements.len(), 3);
}

#[test]
fn a_binding_whose_initializer_does_something_is_kept() {
    // Arrange: the block's type is `#10`, but replacing it would throw away the
    // call inside it, so neither the block nor the binding may go.
    let input = r#"
        let x = { "hi":println(); 10 };
        x
    "#;

    // Act
    let simplified = create_simplified_ast(input);
    let statements = simplified.unwrap_program();

    // Assert
    assert_eq!(
        statements.len(),
        2,
        "the binding must survive because its initializer does something"
    );
}

#[test]
fn a_call_is_not_replaced_by_its_known_result() {
    // Arrange: a function may declare a literal return type, but calling it is
    // still a call — replacing it would skip whatever else it does.
    let input = r#"
        fun f(): #1 => { "hi":println(); 1 }
        f()
    "#;

    // Act
    let simplified = create_simplified_ast(input);
    let statements = simplified.unwrap_program();
    let last = statements.last().unwrap().clone().unwrap_expression();

    // Assert
    assert!(matches!(last, TypedExpression::Call { .. }));
}

// --- The condition that matters ---------------------------------------------

#[test]
fn simplifying_does_not_change_what_a_program_evaluates_to() {
    // Arrange
    let input = r#"
        fun mul_add(left: Int, right: Int): Int => left * right + left;
        let x = 10;
        let y = 20;
        x:mul_add(y)
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(210));
}

#[test]
fn a_collapsed_if_still_yields_its_blocks_value() {
    // Arrange
    let input = r#"
        let a = true;
        let b = false;
        if a || b { "yup" } else { "nope" }
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::String("yup".to_owned()));
}
