mod common;

use common::{create_env, evaluate_expression, try_create_typed_ast};

use interpreter::{value::Number, Value};

fn int(v: i64) -> Value {
    Value::Number(Number::Int(v))
}

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

// ---------------------------------------------------------------------------
// Declarations
//
// Every generic form parses. What happens when you try to *use* one is a
// different matter — see the ignored tests further down.
// ---------------------------------------------------------------------------

#[test]
fn generic_struct_can_be_declared() {
    // Arrange
    let input = r#"
        struct Foo<T> { bar: T }
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn generic_struct_with_several_parameters_can_be_declared() {
    // Arrange
    let input = r#"
        struct Pair<A, B> { a: A, b: B }
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn generic_enum_can_be_declared() {
    // Arrange
    let input = r#"
        enum Result<T> { Ok { value: T }, No }
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn generic_function_can_be_declared() {
    // Arrange
    let input = r#"
        fun id<T>(x: T): T => x
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn generic_type_alias_can_be_declared() {
    // Arrange
    let input = r#"
        type Alias<T> = T;
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn a_concrete_generic_type_can_be_a_parameter_type() {
    // Arrange
    let input = r#"
        struct Foo<T> { bar: T }
        fun f(x: Foo<Int>): Int => 1
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

// ---------------------------------------------------------------------------
// Generic functions, called with an explicit type argument
//
// This is the one corner of the system that works end to end.
// ---------------------------------------------------------------------------

#[test]
fn generic_function_can_be_called_with_a_type_argument() {
    // Arrange
    let input = r#"
        fun id<T>(x: T): T => x
        id::<Int>(1)
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn a_type_argument_can_be_any_primitive() {
    // Arrange
    let input = r#"
        fun id<T>(x: T): T => x
        id::<String>("a")
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("a"));
}

#[test]
fn a_type_argument_can_be_a_struct() {
    // Arrange
    let input = r#"
        struct Foo { a: Int }
        fun id<T>(x: T): T => x
        let f: Foo = id::<Foo>(Foo { a: 1 });
        f.a
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn a_type_argument_substitutes_the_return_type() {
    // Arrange
    let input = r#"
        fun id<T>(x: T): T => x
        typeof(id::<Int>(1))
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("Int"));
}

#[test]
fn a_type_argument_is_enforced_on_the_argument() {
    // Arrange
    let input = r#"
        fun id<T>(x: T): T => x
        id::<Int>("not an int")
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

#[test]
fn generic_calls_can_be_nested() {
    // Arrange
    let input = r#"
        fun id<T>(x: T): T => x
        id::<Int>(id::<Int>(3))
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(3));
}

#[test]
fn a_generic_call_can_be_bound_to_an_annotated_variable() {
    // Arrange
    let input = r#"
        fun id<T>(x: T): T => x
        let r: Int = id::<Int>(1);
        r
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn a_generic_function_can_take_several_type_parameters() {
    // Arrange
    let input = r#"
        fun first<A, B>(a: A): A => a
        first::<Int, String>(1)
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

// ---------------------------------------------------------------------------
// Generic structs
//
// Declaring one works; constructing one does not. The struct literal is
// checked against the *declared* type, where the field is still `T`, so the
// type argument never reaches it:
//
//     Field type T does not match initializer type #1
//
// Nothing below depends on inference — the type is spelled out in the
// annotation and still is not used.
// ---------------------------------------------------------------------------

#[test]
fn a_generic_struct_can_be_constructed() {
    // Arrange
    let input = r#"
        struct Foo<T> { bar: T }
        let f: Foo<Int> = Foo { bar: 1 };
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn a_generic_struct_field_can_be_read() {
    // Arrange
    let input = r#"
        struct Foo<T> { bar: T }
        let f: Foo<Int> = Foo { bar: 1 };
        f.bar
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn a_generic_struct_field_has_the_substituted_type() {
    // Arrange
    let input = r#"
        struct Foo<T> { bar: T }
        let f: Foo<Int> = Foo { bar: 1 };
        typeof(f.bar)
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("Int"));
}

#[test]
fn a_generic_struct_substitutes_each_parameter_separately() {
    // Arrange
    let input = r#"
        struct Pair<A, B> { a: A, b: B }
        let p: Pair<Int, String> = Pair { a: 1, b: "x" };
        typeof(p.b)
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("String"));
}

#[test]
fn a_generic_struct_rejects_a_field_of_the_wrong_type() {
    // Arrange
    // Asserting only that the bad one fails would pass today for the wrong
    // reason: every generic struct literal fails, correct ones included. Both
    // directions are checked so this only goes green once the field is
    // compared against `Int` rather than against `T`.
    let matching = r#"
        struct Foo<T> { bar: T }
        let f: Foo<Int> = Foo { bar: 1 };
        0
    "#;

    let mismatched = r#"
        struct Foo<T> { bar: T }
        let f: Foo<Int> = Foo { bar: "not an int" };
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(matching).is_ok());
    assert!(try_create_typed_ast(mismatched).is_err());
}

#[test]
fn generic_structs_can_nest() {
    // Arrange
    let input = r#"
        struct Foo<T> { bar: T }
        let f: Foo<Foo<Int>> = Foo { bar: Foo { bar: 1 } };
        typeof(f.bar.bar)
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("Int"));
}

#[test]
fn a_generic_struct_can_hold_an_array_of_its_parameter() {
    // Arrange
    let input = r#"
        struct Foo<T> { bar: [T] }
        let f: Foo<Int> = Foo { bar: [1] };
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn a_generic_struct_can_be_constructed_with_a_type_argument() {
    // Arrange
    // The turbofish works on calls but not on struct literals, where it is a
    // parse error: "Expected identifier but found DoubleColon".
    let input = r#"
        struct Foo<T> { bar: T }
        let f = Foo::<Int> { bar: 1 };
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

// ---------------------------------------------------------------------------
// Generic enums
// ---------------------------------------------------------------------------

#[test]
fn a_generic_enum_variant_can_be_constructed() {
    // Arrange
    let input = r#"
        enum Res<T> { Ok { value: T }, No }
        let r: Res<Int> = Res::Ok { value: 1 };
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn a_generic_enum_variant_field_has_the_substituted_type() {
    // Arrange
    let input = r#"
        enum Res<T> { Ok { value: T }, No }
        let r: Res<Int> = Res::Ok { value: 1 };
        r match
        | ::Ok { value } => typeof(value),
        | ::No => "none"
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("Int"));
}

// ---------------------------------------------------------------------------
// Inference
//
// Every working case above spells the type argument out. Nothing infers it.
// ---------------------------------------------------------------------------

#[test]
fn a_type_argument_can_be_inferred_from_the_argument() {
    // Arrange
    // Fails with "Argument type #1 does not match parameter type T".
    let input = r#"
        fun id<T>(x: T): T => x
        id(1)
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn a_struct_type_argument_can_be_inferred_from_its_fields() {
    // Arrange
    let input = r#"
        struct Foo<T> { bar: T }
        let f = Foo { bar: 1 };
        typeof(f.bar)
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("Int"));
}

// ---------------------------------------------------------------------------
// Type aliases
// ---------------------------------------------------------------------------

#[test]
fn a_generic_type_alias_can_be_used() {
    // Arrange
    // Fails with "Cannot clone concrete types for type Alias<T> = T".
    let input = r#"
        type Alias<T> = T;
        let x: Alias<Int> = 1;
        x
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

// ---------------------------------------------------------------------------
// Constraints
//
// `where` clauses are in the design docs and the parser for them exists, but
// the whole thing is commented out (`ast/statements.rs`), so the keyword is a
// syntax error wherever it appears.
// ---------------------------------------------------------------------------

#[test]
fn a_type_parameter_can_be_constrained_on_a_struct() {
    // Arrange
    let input = r#"
        proto Show { fun show(): String; }
        struct Foo<T> where T is Show { bar: T }
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn a_constraint_rejects_a_type_that_does_not_satisfy_it() {
    // Arrange
    // As above, both directions — otherwise this passes merely because `where`
    // is a syntax error and nothing using it can compile at all.
    let satisfying = r#"
        proto Show { fun show(): String; }
        struct Str { }
        imp Show for Str { fun show(): String => "s" }
        struct Foo<T> where T is Show { bar: T }
        let f: Foo<Str> = Foo { bar: Str { } };
        0
    "#;

    let violating = r#"
        proto Show { fun show(): String; }
        struct Foo<T> where T is Show { bar: T }
        let f: Foo<Int> = Foo { bar: 1 };
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(satisfying).is_ok());
    assert!(try_create_typed_ast(violating).is_err());
}

// ---------------------------------------------------------------------------
// Crashes
//
// These do not produce errors — they panic out of the type checker, at
// `expressions.rs`'s `.expect("Failed to clone type with concrete types")`.
// A bad type argument should be a diagnostic, not a compiler crash.
// ---------------------------------------------------------------------------

#[test]
fn a_type_parameter_can_be_used_inside_an_array_parameter() {
    // Arrange
    let input = r#"
        fun id<T>(x: [T]): [T] => x
        id::<Int>([1, 2])
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn a_type_argument_on_a_non_generic_function_is_an_error() {
    // Arrange
    let input = r#"
        fun a(x: Int): Int => x
        a::<Int>(1)
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

#[test]
fn an_unknown_type_argument_is_an_error() {
    // Arrange
    let input = r#"
        fun id<T>(x: T): T => x
        id::<NotAType>(1)
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

#[test]
fn a_generic_function_without_a_parameter_can_be_called() {
    // Arrange
    let input = r#"
        fun mk<T>(): Int => 1
        mk::<Int>()
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn too_many_type_arguments_is_an_error() {
    // Arrange
    // `id` declares one type parameter, so two should not be accepted.
    let input = r#"
        fun id<T>(x: T): T => x
        id::<Int, String>(1)
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

// ---------------------------------------------------------------------------
// Return-position inference
//
// A type parameter that appears only in the return type cannot be pinned by any
// argument. The expected type at the call site supplies it instead.
// ---------------------------------------------------------------------------

#[test]
fn a_type_argument_can_be_inferred_from_the_expected_type() {
    // Arrange
    // `mk` takes nothing, so the annotation is the only thing that can say
    // what `T` is.
    let input = r#"
        struct Box<T> { }
        fun mk<T>(): Box<T> => Box { }
        let b: Box<Int> = mk();
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn return_position_inference_works_through_an_enum() {
    // Arrange
    let input = r#"
        enum Res<T> { Ok { value: T }, No }
        fun mk<T>(): Res<T> => Res::No
        let r: Res<Int> = mk();
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn return_position_inference_works_through_a_tuple() {
    // Arrange
    let input = r#"
        fun pair<T>(x: T): (T, T) => (x, x)
        let p: (Int, Int) = pair(1);
        p
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Tuple(vec![int(1), int(1)]));
}

#[test]
fn return_position_inference_works_through_an_array() {
    // Arrange
    // The body is an empty array, which cannot type itself — the declared
    // return type gives it an element type, and the call site gives `T`.
    let input = r#"
        fun empty<T>(): [T] => []
        let xs: [Int] = empty();
        typeof(xs)
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, string("[Int]"));
}

#[test]
fn a_function_body_is_checked_against_its_declared_return_type() {
    // Arrange
    // Pushing the return type into the body must not stop it being checked.
    let input = r#"
        fun bad(): [Int] => ["not an int"]
        bad()
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

#[test]
fn a_body_that_cannot_produce_the_type_parameter_is_rejected() {
    // Arrange
    // `T` could be anything, so a body returning ints does not satisfy `(T, T)`.
    let input = r#"
        fun mk<T>(): (T, T) => (1, 1)
        let p: (Int, Int) = mk();
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

// ---------------------------------------------------------------------------
// Constraint forms
//
// `and` puts several bounds on one parameter, `,` constrains several
// parameters, and both structs and enums accept a where clause.
// ---------------------------------------------------------------------------

/// Two protocols and a type implementing both, for the constraint tests below.
const PROTOCOLS: &str = r#"
    proto Show { fun show(): String; }
    proto Eq { fun eq(): Bool; }
    struct Both { }
    imp Show for Both { fun show(): String => "s" }
    imp Eq for Both { fun eq(): Bool => true }
    struct OnlyShow { }
    imp Show for OnlyShow { fun show(): String => "o" }
"#;

#[test]
fn a_parameter_can_have_several_bounds() {
    // Arrange
    let input = format!(
        r#"{PROTOCOLS}
        struct Foo<T> where T is Show and Eq {{ bar: T }}
        let f: Foo<Both> = Foo {{ bar: Both {{ }} }};
        0
    "#
    );

    // Act & Assert
    assert!(try_create_typed_ast(&input).is_ok());
}

#[test]
fn every_bound_on_a_parameter_must_be_satisfied() {
    // Arrange
    // `OnlyShow` satisfies the first bound but not the second.
    let input = format!(
        r#"{PROTOCOLS}
        struct Foo<T> where T is Show and Eq {{ bar: T }}
        let f: Foo<OnlyShow> = Foo {{ bar: OnlyShow {{ }} }};
        0
    "#
    );

    // Act & Assert
    assert_error_contains(&input, "does not satisfy");
}

#[test]
fn several_parameters_can_be_constrained() {
    // Arrange
    let input = format!(
        r#"{PROTOCOLS}
        struct Pair<A, B> where A is Show, B is Eq {{ a: A, b: B }}
        let p: Pair<Both, Both> = Pair {{ a: Both {{ }}, b: Both {{ }} }};
        0
    "#
    );

    // Act & Assert
    assert!(try_create_typed_ast(&input).is_ok());
}

#[test]
fn a_violated_bound_is_reported_for_the_parameter_it_belongs_to() {
    // Arrange
    let input = format!(
        r#"{PROTOCOLS}
        struct Pair<A, B> where A is Show, B is Eq {{ a: A, b: B }}
        let p: Pair<Both, Int> = Pair {{ a: Both {{ }}, b: 1 }};
        0
    "#
    );

    // Act & Assert
    assert_error_contains(&input, "`B is Eq`");
}

#[test]
fn an_enum_can_constrain_its_parameter() {
    // Arrange
    let input = format!(
        r#"{PROTOCOLS}
        enum Holder<T> where T is Show {{ Held {{ value: T }}, Empty }}
        let h: Holder<Both> = Holder::Empty;
        0
    "#
    );

    // Act & Assert
    assert!(try_create_typed_ast(&input).is_ok());
}

#[test]
fn an_enum_constraint_rejects_a_type_that_does_not_satisfy_it() {
    // Arrange
    let input = format!(
        r#"{PROTOCOLS}
        enum Holder<T> where T is Show {{ Held {{ value: T }}, Empty }}
        let h: Holder<Int> = Holder::Empty;
        0
    "#
    );

    // Act & Assert
    assert_error_contains(&input, "does not satisfy");
}
