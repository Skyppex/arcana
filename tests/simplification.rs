//! The simplification pass replaces expressions whose value the type system
//! already knows.
//!
//! Its correctness condition is that it changes no observable result, which the
//! rest of the suite covers by running it before every evaluation. These tests
//! are about the other half: that it actually rewrites what it should, and
//! leaves alone what it must.

mod common;

use common::{
    create_env, create_simplified_ast, evaluate_expression, StatementExt, VecStatementExt,
};

use interpreter::{value::Number, Value};
use shared::type_checker::model::{TypedExpression, TypedStatement, ValueLiteral};

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

    // Assert: both bindings dropped, and the `if` replaced by the value of the
    // branch it takes. The block collapses too — it is pure, so there is
    // nothing in it to preserve.
    assert_eq!(statements.len(), 1);
    assert!(
        matches!(
            statements[0].clone().unwrap_expression(),
            TypedExpression::Literal {
                literal: ValueLiteral::String(ref s),
                ..
            } if s == "yup"
        ),
        "the if should have collapsed to the value of the branch it takes"
    );
}

#[test]
fn arithmetic_over_known_values_folds_to_one_literal() {
    // Arrange
    let input = "1 + 2 * 3";

    // Act
    let simplified = create_simplified_ast(input);
    let expression = simplified
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

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

// ---------------------------------------------------------------------------
// Purity
//
// What the pass is allowed to throw away. The first group is what purity buys;
// the rest are the cases that are *pure and still must be kept*, each of which
// would be a miscompile if the pass got it wrong.
// ---------------------------------------------------------------------------

/// Counts statements at the top level of the simplified program.
fn statement_count(input: &str) -> usize {
    create_simplified_ast(input).unwrap_program().len()
}

#[test]
fn a_pure_statement_whose_value_is_unused_is_dropped() {
    // `len` has no effects, and nothing uses the result.
    assert_eq!(statement_count(r#"{ [1, 2, 3]:len(); }; "done""#), 1);
}

#[test]
fn an_impure_statement_is_kept() {
    assert_eq!(statement_count(r#"{ "hi":println(); }; "done""#), 2);
}

/// The old shape-based test could never fold a call, however pure: a call might
/// do something besides produce its value. Purity is exactly the question it
/// could not ask.
#[test]
fn a_call_to_a_pure_function_folds_to_its_value() {
    let simplified = create_simplified_ast("fun one(): #1 => 1; one()");
    let statements = simplified.unwrap_program();

    assert!(
        matches!(
            statements.last().unwrap().clone().unwrap_expression(),
            TypedExpression::Literal {
                literal: ValueLiteral::Int(1),
                ..
            }
        ),
        "a call to a pure function with a known value should fold"
    );
}

#[test]
fn a_call_to_an_impure_function_is_not_folded() {
    let simplified = create_simplified_ast(r#"fun one(): #1 => { "hi":println(); 1 }; one()"#);
    let statements = simplified.unwrap_program();

    assert!(
        matches!(
            statements.last().unwrap().clone().unwrap_expression(),
            TypedExpression::Call { .. }
        ),
        "a call that prints must survive, whatever its type says its value is"
    );
}

/// Pure by every rule, and deleting it would change which value the function
/// returns. Purity alone is not enough to delete something.
#[test]
fn a_block_that_returns_is_kept() {
    assert_eq!(
        evaluate_expression(
            "fun f(): Int => { { return 1; }; 2 }; f()",
            create_env(),
            false
        ),
        int(1)
    );
}

/// Writing to a binding declared outside the block is observable, so the block
/// stays — see the note on `Assignment` in `purity.rs`.
#[test]
fn a_block_that_assigns_to_an_outer_binding_is_kept() {
    let environment = create_env();

    assert_eq!(
        evaluate_expression(
            "let mut total = 0; { total = total + 1; }; total",
            environment,
            false
        ),
        int(1)
    );
}

/// A loop is never discarded, however pure, because it may not finish.
///
/// `loop { break 5; }` is pure *and* has type `#5`, so without the guard the
/// whole loop would be replaced by its value. That it happens to terminate here
/// is exactly what makes it a usable test: the case the guard really protects —
/// a loop that never ends — cannot be asserted by a test that returns.
#[test]
fn a_pure_loop_is_not_replaced_by_its_value() {
    let simplified = create_simplified_ast("let x = loop { break 5; }; x");
    let statements = simplified.unwrap_program();

    let TypedStatement::Semi(declaration) = statements[0].clone() else {
        panic!("expected the binding to survive");
    };

    let TypedExpression::VariableDeclaration {
        initializer: Some(initializer),
        ..
    } = declaration.unwrap_expression()
    else {
        panic!("expected a binding with an initializer");
    };

    assert!(
        matches!(*initializer, TypedExpression::Loop { .. }),
        "a pure loop must survive, even one whose value is known"
    );
}

/// An impure loop runs as written.
#[test]
fn a_loop_with_effects_still_runs() {
    let input = "let mut n = 0; while n < 3 { n = n + 1; }; n";

    assert_eq!(evaluate_expression(input, create_env(), false), int(3));
}

/// Dividing by zero traps, and deleting the statement would remove the trap.
#[test]
fn a_division_by_a_known_zero_is_kept() {
    assert_eq!(statement_count("{ 1 / 0; }; 2"), 2);
}

#[test]
fn a_division_by_a_known_non_zero_is_dropped() {
    assert_eq!(statement_count("{ 1 / 2; }; 2"), 1);
}

/// Building a closure runs nothing, however dirty its body — the body's purity
/// belongs to the closure's type, for whoever calls it.
#[test]
fn creating_an_impure_closure_is_pure_but_calling_it_is_not() {
    // The binding is unused and its initializer only builds a closure, so the
    // statement goes.
    assert_eq!(
        statement_count(r#"{ |x: Int| { "hi":println(); x } }; 2"#),
        1
    );

    // Calling one does not.
    assert_eq!(
        statement_count(r#"{ (|x: Int| { "hi":println(); x })(1); }; 2"#),
        2
    );
}
