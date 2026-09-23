mod common;

use common::{
    create_typed_ast, evaluate_expression, try_create_typed_ast, StatementExt, VecStatementExt,
};

use interpreter::{value, Value};
use shared::type_checker::{
    model::{Typed, TypedExpression},
    LiteralType, Type,
};

use crate::common::create_env;

fn int(v: i64) -> Value {
    Value::Number(value::Number::Int(v))
}

fn assert_error_contains(input: &str, needle: &str) {
    match try_create_typed_ast(input) {
        Ok(_) => panic!("expected an error mentioning {needle:?}, but the program type-checked"),
        Err(e) => assert!(
            e.to_lowercase().contains(&needle.to_lowercase()),
            "expected an error mentioning {needle:?}, got: {e}"
        ),
    }
}

#[test]
fn match_is_match() {
    // Arrange
    let input = r#"
        unit match
        | _ => unit
        "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Match { .. }));
}

#[test]
fn match_has_correct_type() {
    // Arrange
    let input = r#"
        unit match
        | _ => unit
        "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Unit);
}

#[test]
fn match_has_correct_type_2() {
    // Arrange
    let input = r#"
        unit match
        | _ => 1
        "#;

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
            name: "1".to_owned(),
            type_: Box::new(LiteralType::IntValue(1))
        }
    );
}

#[test]
fn match_returns_arm() {
    // Arrange
    let input = r#"
        unit match
        | _ => "Hello"
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::String("Hello".to_string()));
}

#[test]
fn match_returns_earliest_arm() {
    // Arrange
    let input = r#"
        1 match
        | 1 => "Hello",
        | < 5 => "World",
        | _ => "!"
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::String("Hello".to_string()));
}

#[test]
fn match_variable_declaration() {
    // Arrange
    let input = r#"
        3 match
        | 1 => 100,
        | 2 => 200,
        | x => x
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(3)));
}

#[test]
fn match_on_unit() {
    // Arrange
    let input = r#"
        unit match
        | unit => 1,
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

#[test]
fn match_on_bool() {
    // Arrange
    let input = r#"
        true match
        | true => 1,
        | false => 2,
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

#[test]
fn match_on_int() {
    // Arrange
    let input = r#"
        1 match
        | 1 => 1,
        | 2 => 2,
        | _ => 0
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

#[test]
fn match_on_uint() {
    // Arrange
    let input = r#"
        1u match
        | 1u => 1,
        | 2u => 2,
        | _ => 0
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

#[test]
fn match_on_float() {
    // Arrange
    let input = r#"
        1f match
        | 1f => 1,
        | 2f => 2,
        | _ => 0
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

#[test]
fn match_on_char() {
    // Arrange
    let input = r#"
        'a' match
        | 'a' => 1,
        | 'b' => 2,
        | _ => 0
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

#[test]
fn match_on_string() {
    // Arrange
    let input = r#"
        "Hello" match
        | "Hello" => 1,
        | "World!" => 2,
        | _ => 0
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

#[test]
fn match_wildcard() {
    // Arrange
    let input = r#"
        unit match
        | _ => 1
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

#[test]
fn match_variable() {
    // Arrange
    let input = r#"
        unit match
        | x => 1
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(value::Number::Int(1)));
}

// ---------------------------------------------------------------------------
// Constructor patterns, exhaustiveness and reachability.
// ---------------------------------------------------------------------------

// --- Structs: the type is optional, `::`-free means struct ------------------

#[test]
fn match_struct_pattern_without_type() {
    // Arrange
    let input = r#"
        struct Point { x: Int, y: Int }
        let p: Point = Point { x: 0, y: 0 };
        p match
        | { x: 0, y: 0 } => 1,
        | _ => 2
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn match_struct_pattern_with_type() {
    // Arrange
    let input = r#"
        struct Point { x: Int, y: Int }
        let p: Point = Point { x: 0, y: 0 };
        p match
        | Point { x: 0, y: 0 } => 1,
        | _ => 2
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn match_struct_pattern_binds_fields() {
    // Arrange
    let input = r#"
        struct Point { x: Int, y: Int }
        let p: Point = Point { x: 2, y: 40 };
        p match
        | { x, y } => x + y
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(42));
}

#[test]
fn match_struct_pattern_ignores_omitted_fields() {
    // Arrange
    let input = r#"
        struct Point { x: Int, y: Int }
        let p: Point = Point { x: 7, y: 9 };
        p match
        | { x } => x
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(7));
}

#[test]
fn match_struct_pattern_tests_each_field_against_its_own_value() {
    // Arrange
    // Regression for the old lowering, which tested `p.x` against `1` and then
    // `p.x` against `2` — so this matched the first arm.
    let input = r#"
        struct Point { x: Int, y: Int }
        let p: Point = Point { x: 2, y: 1 };
        p match
        | { x: 1, y: 2 } => 1,
        | _ => 2
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(2));
}

#[test]
fn match_nested_struct_pattern() {
    // Arrange
    let input = r#"
        struct Inner { v: Int }
        struct Outer { inner: Inner }
        let o: Outer = Outer { inner: Inner { v: 5 } };
        o match
        | { inner: { v } } => v
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(5));
}

#[test]
fn match_struct_pattern_with_wrong_type_is_error() {
    // Arrange
    let input = r#"
        struct Point { x: Int }
        struct Other { x: Int }
        let p: Point = Point { x: 1 };
        p match
        | Other { x } => x,
        | _ => 0
        "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

#[test]
fn match_struct_pattern_with_unknown_field_is_error() {
    // Arrange
    let input = r#"
        struct Point { x: Int }
        let p: Point = Point { x: 1 };
        p match
        | { nope } => nope,
        | _ => 0
        "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

// --- Enums: `::` means variant ----------------------------------------------

#[test]
fn match_enum_qualified_variant() {
    // Arrange
    let input = r#"
        enum MyEnum { First, Second }
        let e: MyEnum = MyEnum::Second;
        e match
        | MyEnum::First => 1,
        | MyEnum::Second => 2
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(2));
}

#[test]
fn match_enum_unqualified_variant() {
    // Arrange
    let input = r#"
        enum MyEnum { First, Second }
        let e: MyEnum = MyEnum::Second;
        e match
        | ::First => 1,
        | ::Second => 2
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(2));
}

#[test]
fn match_enum_variant_is_evaluated() {
    // Arrange
    // `if` without an `else` yields an Option, so this is a real enum value.
    let input = r#"
        let o = if false { 1 };
        o match
        | ::Some { value } => value,
        | ::None => 0
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(0));
}

#[test]
fn match_enum_variant_binds_its_fields() {
    // Arrange
    let input = r#"
        let o = if true { 7 };
        o match
        | ::Some { value } => value,
        | ::None => 0
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(7));
}

#[test]
fn match_enum_variant_with_nested_pattern() {
    // Arrange
    let input = r#"
        let o = if true { 7 };
        o match
        | ::Some { value: > 0 } => 1,
        | ::Some { value } => 2,
        | ::None => 3
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn match_enum_variant_with_fields() {
    // Arrange
    let input = r#"
        enum MyEnum { First { value: Int }, Second }
        let e: MyEnum = MyEnum::First { value: 5 };
        e match
        | ::First { value } => value,
        | ::Second => 0
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(5));
}

#[test]
fn match_enum_wrong_enum_is_error() {
    // Arrange
    let input = r#"
        enum MyEnum { First, Second }
        enum Other { First }
        let e: MyEnum = MyEnum::First;
        e match
        | Other::First => 1,
        | _ => 2
        "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

#[test]
fn match_enum_unknown_variant_is_error() {
    // Arrange
    let input = r#"
        enum MyEnum { First, Second }
        let e: MyEnum = MyEnum::First;
        e match
        | ::Third => 1,
        | _ => 2
        "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

// --- An enum can be matched as a struct over its shared fields --------------

#[test]
fn match_enum_as_struct_over_shared_fields() {
    // Arrange
    // `e` is MyEnum, not a single variant, but `id` exists on every variant so
    // a `::`-free struct pattern can see it without discriminating.
    let input = r#"
        enum MyEnum { id: Int, First, Second }
        let e: MyEnum = MyEnum::Second { id: 4 };
        e match
        | { id: 0 } => 100,
        | { id: other } => other
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(4));
}

#[test]
fn match_enum_as_struct_is_exhaustive_without_variant_arms() {
    // Arrange
    let input = r#"
        enum MyEnum { id: Int, First, Second }
        let e: MyEnum = MyEnum::Second { id: 7 };
        e match
        | { id } => id
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(7));
}

#[test]
fn match_enum_as_struct_with_explicit_type() {
    // Arrange
    let input = r#"
        enum MyEnum { id: Int, First, Second }
        let e: MyEnum = MyEnum::First { id: 1 };
        e match
        | MyEnum { id } => id
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn match_enum_mixes_shared_field_and_variant_patterns() {
    // Arrange
    // The shared-field arm carries no variant test, so it has to be tried
    // against every variant rather than belonging to one branch.
    let input = r#"
        enum MyEnum { id: Int, First, Second }
        let e: MyEnum = MyEnum::First { id: 0 };
        e match
        | { id: 0 } => 1,
        | ::First => 2,
        | ::Second => 3
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn match_enum_as_struct_cannot_see_variant_fields() {
    // Arrange
    // `value` is on First but not Second, so it is not shared and cannot be
    // reached without discriminating the variant first.
    let input = r#"
        enum MyEnum { id: Int, First { value: Int }, Second }
        let e: MyEnum = MyEnum::First { id: 1, value: 2 };
        e match
        | { value } => value,
        | _ => 0
        "#;

    // Act & Assert
    assert_error_contains(input, "shared");
}

// --- A variant is a struct --------------------------------------------------

#[test]
fn match_narrowed_variant_with_struct_pattern() {
    // Arrange
    // `e` is typed as the variant itself, so the variant is known statically
    // and this is an ordinary struct pattern.
    let input = r#"
        enum MyEnum { First { value: Int }, Second }
        let e = MyEnum::First { value: 5 };
        e match
        | { value: > 0 } => 1,
        | _ => 2
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn match_narrowed_variant_with_variant_pattern() {
    // Arrange
    let input = r#"
        enum MyEnum { First { value: Int }, Second }
        let e = MyEnum::First { value: 5 };
        e match
        | ::First { value: > 0 } => 1,
        | _ => 2
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn match_narrowed_variant_is_exhaustive_without_wildcard() {
    // Arrange
    let input = r#"
        enum MyEnum { First { value: Int }, Second }
        let e = MyEnum::First { value: 5 };
        e match
        | ::First { value } => value
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(5));
}

#[test]
fn match_narrowed_variant_with_wrong_variant_is_error() {
    // Arrange
    let input = r#"
        enum MyEnum { First { value: Int }, Second }
        let e = MyEnum::First { value: 5 };
        e match
        | ::Second => 1,
        | _ => 2
        "#;

    // Act & Assert
    assert_error_contains(input, "never match");
}

// --- Tuples -----------------------------------------------------------------

#[test]
fn match_tuple_pattern() {
    // Arrange
    let input = r#"
        let t: (Int, Int) = (1, 2);
        t match
        | (1, 2) => 1,
        | _ => 2
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn match_tuple_pattern_with_binding() {
    // Arrange
    // The old lowering rejected this outright: a tuple element that isn't a
    // guard produced "Expected a guard decision".
    let input = r#"
        let t: (Int, Int) = (1, 40);
        t match
        | (_, y) => y
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(40));
}

#[test]
fn match_nested_tuple_pattern() {
    // Arrange
    let input = r#"
        let t: (Int, (Int, Int)) = (1, (2, 3));
        t match
        | (_, (_, z)) => z
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(3));
}

#[test]
fn match_tuple_pattern_wrong_arity_is_error() {
    // Arrange
    let input = r#"
        let t: (Int, Int) = (1, 2);
        t match
        | (a, b, c) => a,
        | _ => 0
        "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

// --- Ranges and comparisons -------------------------------------------------

#[test]
fn match_range_exclusive() {
    // Arrange
    let input = r#"
        10 match
        | 1..10 => 1,
        | _ => 2
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(2));
}

#[test]
fn match_range_inclusive() {
    // Arrange
    let input = r#"
        10 match
        | 1..=10 => 1,
        | _ => 2
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn match_comparison_pattern() {
    // Arrange
    let input = r#"
        5 match
        | < 0 => 1,
        | <= 5 => 2,
        | _ => 3
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(2));
}

#[test]
fn match_comparison_pattern_with_variable_bound() {
    // Arrange
    let input = r#"
        let limit: Int = 3;
        5 match
        | < limit => 1,
        | _ => 2
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(2));
}

// --- Exhaustiveness ---------------------------------------------------------

#[test]
fn match_non_exhaustive_enum_is_error() {
    // Arrange
    let input = r#"
        enum MyEnum { First, Second }
        let e: MyEnum = MyEnum::First;
        e match
        | ::First => 1
        "#;

    // Act & Assert
    assert_error_contains(input, "exhaustive");
}

#[test]
fn match_non_exhaustive_bool_is_error() {
    // Arrange
    let input = r#"
        true match
        | true => 1
        "#;

    // Act & Assert
    assert_error_contains(input, "exhaustive");
}

#[test]
fn match_non_exhaustive_int_is_error() {
    // Arrange
    let input = r#"
        1 match
        | 1 => 1,
        | 2 => 2
        "#;

    // Act & Assert
    assert_error_contains(input, "exhaustive");
}

#[test]
fn match_exhaustive_bool_needs_no_wildcard() {
    // Arrange
    let input = r#"
        true match
        | true => 1,
        | false => 2
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn match_nested_non_exhaustive_is_error() {
    // Arrange
    let input = r#"
        enum MyEnum { First { value: Int }, Second }
        let e: MyEnum = MyEnum::First { value: 1 };
        e match
        | ::First { value: 1 } => 1,
        | ::Second => 2
        "#;

    // Act & Assert
    assert_error_contains(input, "exhaustive");
}

// --- Reachability -----------------------------------------------------------

#[test]
fn match_duplicate_arm_is_error() {
    // Arrange
    // Supersedes `match_returns_earliest_arm`, which asserted the first of two
    // identical arms wins.
    let input = r#"
        1 match
        | 1 => 1,
        | 1 => 2,
        | _ => 3
        "#;

    // Act & Assert
    assert_error_contains(input, "unreachable");
}

#[test]
fn match_arm_after_wildcard_is_error() {
    // Arrange
    let input = r#"
        1 match
        | _ => 1,
        | 2 => 2
        "#;

    // Act & Assert
    assert_error_contains(input, "unreachable");
}

#[test]
fn match_repeated_variant_is_error() {
    // Arrange
    let input = r#"
        enum MyEnum { First, Second }
        let e: MyEnum = MyEnum::First;
        e match
        | ::First => 1,
        | ::Second => 2,
        | ::First => 3
        "#;

    // Act & Assert
    assert_error_contains(input, "unreachable");
}

// --- Semantics --------------------------------------------------------------

#[test]
fn match_evaluates_matchee_once() {
    // Arrange
    // The old lowering cloned the matchee into every guard condition, so an
    // effectful matchee ran once per arm. The harness here just needs *some*
    // observable side effect; adapt it if closures capture differently.
    let input = r#"
        let mut calls: Int = 0;
        let bump = || { calls = calls + 1; calls };
        bump() match
        | 1 => calls,
        | _ => calls
        "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn match_joins_arm_types() {
    // Arrange
    let input = r#"
        true match
        | true => 1,
        | false => 2
        "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    // Two different int literals meet at #Int, one step up from #1 and #2.
    assert_eq!(
        expression.get_type(),
        Type::Literal {
            name: "Int".to_owned(),
            type_: Box::new(LiteralType::Int)
        }
    );
}

#[test]
fn match_incompatible_arm_types_is_error() {
    // Arrange
    let input = r#"
        true match
        | true => 1,
        | false => "two"
        "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

// ---------------------------------------------------------------------------
// Nested matches
//
// An arm body is parsed as a full expression, and `|` both separates arms and
// is the bitwise-or operator. So an unbraced `match` inside an arm keeps
// consuming `|` arms that were meant for the outer match, three ways:
//
//   * with a trailing comma, the outer arms are silently absorbed by the inner
//     match, which surfaces as an unreachable-arm error about an arm the reader
//     believes belongs to the outer one;
//   * without one, `body | pattern` is parsed as a bitwise-or expression;
//   * an outer arm that is not a wildcard is absorbed just the same.
//
// Bracing the inner match is the way to write it today. The ignored tests below
// are what the unbraced form should mean, and are the spec for whenever the
// grammar settles this.
// ---------------------------------------------------------------------------

#[test]
fn a_braced_arm_body_can_hold_a_nested_match() {
    // Arrange
    let input = r#"
        struct Point { x: Int, y: Int }
        struct Line { start: Point, end: Point }

        let line: Line = Line {
            start: Point { x: 0, y: 0 },
            end: Point { x: 1, y: 2 },
        };

        line match
        | {
            start: { x: 0, y: 0 },
            end,
        } => { end match
            | { x: > 0, y: > 0 } => "end is positive",
            | _ => "end is not only positive" },
        | _ => "line doesn't start at 0,0"
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::String("end is positive".to_owned()));
}

#[test]
#[ignore = "grammar: an unbraced nested match absorbs the outer match's arms"]
fn an_unbraced_nested_match_leaves_the_outer_arms_alone() {
    // Arrange
    // Fails with "Match arm `_` is unreachable" — the final arm was taken by
    // the inner match rather than the outer one.
    let input = r#"
        struct Point { x: Int, y: Int }
        struct Line { start: Point, end: Point }

        let line: Line = Line {
            start: Point { x: 0, y: 0 },
            end: Point { x: 1, y: 2 },
        };

        line match
        | {
            start: { x: 0, y: 0 },
            end,
        } => end match
            | { x: > 0, y: > 0 } => "end is positive",
            | _ => "end is not only positive",
        | _ => "line doesn't start at 0,0"
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::String("end is positive".to_owned()));
}

#[test]
#[ignore = "grammar: an unbraced nested match absorbs the outer match's arms"]
fn a_named_arm_after_a_nested_match_belongs_to_the_outer_match() {
    // Arrange
    // The absorbed arm need not be a wildcard; `| { x: 2 }` is taken too.
    let input = r#"
        struct P { x: Int }
        let p: P = P { x: 2 };
        p match
        | { x: 1 } => p match
            | { x: 1 } => "inner one",
            | _ => "inner other",
        | { x: 2 } => "outer two",
        | _ => "outer other"
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::String("outer two".to_owned()));
}

#[test]
#[ignore = "grammar: `|` is also bitwise-or, so an unbraced body swallows it"]
fn an_arm_separator_is_not_parsed_as_bitwise_or() {
    // Arrange
    // Without a trailing comma this fails with "Expected primary expression but
    // found Underscore": `"inner other" | _` is read as an expression.
    let input = r#"
        struct P { x: Int }
        let p: P = P { x: 1 };
        p match
        | { x: 1 } => p match
            | { x: 1 } => "inner one",
            | _ => "inner other"
        | _ => "outer other"
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::String("inner one".to_owned()));
}
