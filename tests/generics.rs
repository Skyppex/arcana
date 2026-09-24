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

// ---------------------------------------------------------------------------
// Implementations
//
// A type has at most one implementation of a protocol, so nothing is ever
// chosen between. `B<Int>` and `B<String>` are distinct types and may each have
// their own; a blanket `B<T>` covers both and so cannot sit beside either.
//
// The overlap check runs during type discovery, before anything is checked,
// because it needs only the names written in the source.
// ---------------------------------------------------------------------------

/// A protocol and a generic type to implement it for.
const SHOW: &str = r#"
    proto Show { fun show(): String; }
    struct B<T> { v: T }
"#;

#[test]
fn distinct_instantiations_may_each_be_implemented() {
    // Arrange
    // `B<Int>` and `B<String>` are different types; no value is covered twice.
    let input = format!(
        r#"{SHOW}
        imp Show for B<Int> {{ fun show(): String => "int" }}
        imp Show for B<String> {{ fun show(): String => "str" }}
        0
    "#
    );

    // Act & Assert
    assert!(try_create_typed_ast(&input).is_ok());
}

#[test]
fn a_blanket_implementation_is_allowed_on_its_own() {
    // Arrange
    let input = format!(
        r#"{SHOW}
        imp<T> Show for B<T> {{ fun show(): String => "any" }}
        0
    "#
    );

    // Act & Assert
    assert!(try_create_typed_ast(&input).is_ok());
}

#[test]
fn a_blanket_conflicts_with_an_instantiation() {
    // Arrange
    // `B<T>` already covers `B<Int>`, so the two collide.
    let input = format!(
        r#"{SHOW}
        imp<T> Show for B<T> {{ fun show(): String => "any" }}
        imp Show for B<Int> {{ fun show(): String => "int" }}
        0
    "#
    );

    // Act & Assert
    assert_error_contains(&input, "Conflicting implementations of `Show`");
}

#[test]
fn two_blankets_conflict() {
    // Arrange
    // Renaming the parameter changes nothing about what they cover.
    let input = format!(
        r#"{SHOW}
        imp<T> Show for B<T> {{ fun show(): String => "one" }}
        imp<U> Show for B<U> {{ fun show(): String => "two" }}
        0
    "#
    );

    // Act & Assert
    assert_error_contains(&input, "Conflicting implementations of `Show`");
}

#[test]
fn the_same_instantiation_cannot_be_implemented_twice() {
    // Arrange
    let input = format!(
        r#"{SHOW}
        imp Show for B<Int> {{ fun show(): String => "one" }}
        imp Show for B<Int> {{ fun show(): String => "two" }}
        0
    "#
    );

    // Act & Assert
    assert_error_contains(&input, "Conflicting implementations of `Show`");
}

#[test]
fn an_alias_cannot_disguise_a_duplicate_implementation() {
    // Arrange
    // `MyInt` and `Int` are the same type spelled two ways.
    let input = format!(
        r#"{SHOW}
        type MyInt = Int;
        imp Show for B<Int> {{ fun show(): String => "one" }}
        imp Show for B<MyInt> {{ fun show(): String => "two" }}
        0
    "#
    );

    // Act & Assert
    assert_error_contains(&input, "Conflicting implementations of `Show`");
}

#[test]
fn different_protocols_on_one_type_do_not_conflict() {
    // Arrange
    let input = format!(
        r#"{SHOW}
        proto Eq {{ fun eq(): Bool; }}
        imp<T> Show for B<T> {{ fun show(): String => "s" }}
        imp<T> Eq for B<T> {{ fun eq(): Bool => true }}
        0
    "#
    );

    // Act & Assert
    assert!(try_create_typed_ast(&input).is_ok());
}

#[test]
fn one_protocol_on_different_types_does_not_conflict() {
    // Arrange
    let input = format!(
        r#"{SHOW}
        struct C<T> {{ v: T }}
        imp<T> Show for B<T> {{ fun show(): String => "b" }}
        imp<T> Show for C<T> {{ fun show(): String => "c" }}
        0
    "#
    );

    // Act & Assert
    assert!(try_create_typed_ast(&input).is_ok());
}

// --- What a bound sees ------------------------------------------------------

#[test]
fn a_blanket_satisfies_a_bound_for_every_instantiation() {
    // Arrange
    let input = format!(
        r#"{SHOW}
        imp<T> Show for B<T> {{ fun show(): String => "any" }}
        struct W<U> where U is Show {{ i: U }}
        let w: W<B<String>> = W {{ i: B {{ v: "s" }} }};
        0
    "#
    );

    // Act & Assert
    assert!(try_create_typed_ast(&input).is_ok());
}

#[test]
fn an_instantiation_implementation_satisfies_a_bound_only_for_that_instantiation() {
    // Arrange
    // Implementations used to be keyed by the bare constructor, so an impl for
    // `B<Int>` made `B<String>` look like it had one too.
    let satisfied = format!(
        r#"{SHOW}
        imp Show for B<Int> {{ fun show(): String => "int" }}
        struct W<U> where U is Show {{ i: U }}
        let w: W<B<Int>> = W {{ i: B {{ v: 1 }} }};
        0
    "#
    );

    let unsatisfied = format!(
        r#"{SHOW}
        imp Show for B<Int> {{ fun show(): String => "int" }}
        struct W<U> where U is Show {{ i: U }}
        let w: W<B<String>> = W {{ i: B {{ v: "s" }} }};
        0
    "#
    );

    // Act & Assert
    assert!(try_create_typed_ast(&satisfied).is_ok());
    assert_error_contains(&unsatisfied, "does not satisfy");
}

// ---------------------------------------------------------------------------
// Constrained functions
//
// A function's `where` clause is checked wherever it is called, against the
// type arguments it was called with — whether those were written out or
// inferred.
// ---------------------------------------------------------------------------

/// A protocol and a type implementing it, for the function bound tests.
const SHOWABLE: &str = r#"
    proto Show { fun show(): String; }
    proto Eq { fun eq(): Bool; }
    struct Dog { n: Int }
    imp Show for Dog { fun show(): String => "woof" }
    imp Eq for Dog { fun eq(): Bool => true }
"#;

#[test]
fn a_function_can_constrain_its_type_parameter() {
    // Arrange
    let input = format!(
        r#"{SHOWABLE}
        fun describe<T>(x: T): Int where T is Show => 1
        describe::<Dog>(Dog {{ n: 1 }})
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn a_function_bound_rejects_an_explicit_type_argument() {
    // Arrange
    let input = format!(
        r#"{SHOWABLE}
        fun describe<T>(x: T): Int where T is Show => 1
        describe::<Int>(1)
    "#
    );

    // Act & Assert
    assert_error_contains(&input, "does not satisfy the bound `T is Show`");
}

#[test]
fn a_function_bound_is_checked_on_an_inferred_type_argument() {
    // Arrange
    // Nothing was written out, so the bound has to be checked against what was
    // inferred from the argument.
    let satisfied = format!(
        r#"{SHOWABLE}
        fun describe<T>(x: T): Int where T is Show => 1
        describe(Dog {{ n: 1 }})
    "#
    );

    let violated = format!(
        r#"{SHOWABLE}
        fun describe<T>(x: T): Int where T is Show => 1
        describe(1)
    "#
    );

    // Act & Assert
    assert_eq!(evaluate_expression(&satisfied, create_env(), false), int(1));
    assert_error_contains(&violated, "does not satisfy the bound `T is Show`");
}

#[test]
fn a_function_parameter_can_have_several_bounds() {
    // Arrange
    let input = format!(
        r#"{SHOWABLE}
        fun describe<T>(x: T): Int where T is Show and Eq => 1
        describe::<Dog>(Dog {{ n: 1 }})
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn a_function_can_constrain_several_parameters() {
    // Arrange
    let input = format!(
        r#"{SHOWABLE}
        fun describe<A, B>(a: A): Int where A is Show, B is Show => 1
        describe::<Dog, Dog>(Dog {{ n: 1 }})
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

#[test]
fn a_protocol_signature_can_carry_a_where_clause() {
    // Arrange
    let input = r#"
        proto Show { fun show(): String; }
        proto Describe { fun describe<T>(x: T): Int where T is Show; }
        0
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn an_unconstrained_generic_function_is_unaffected() {
    // Arrange
    let input = r#"
        fun id<T>(x: T): T => x
        id(1)
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

// ---------------------------------------------------------------------------
// Bounds are satisfied nominally
//
// A type satisfies a protocol because an `imp` says so, not because it happens
// to have methods of the right names. Otherwise a protocol declaring nothing
// would be satisfied by every type, and two protocols declaring the same method
// name would be interchangeable.
// ---------------------------------------------------------------------------

#[test]
fn an_empty_protocol_is_satisfied_by_nothing_in_particular() {
    // Arrange
    // `Listable` requires nothing, but `Int` still does not implement it.
    let input = r#"
        proto Listable;
        fun list<T>(value: T): [T] where T is Listable => [value];
        list(3)
    "#;

    // Act & Assert
    assert_error_contains(input, "does not implement `Listable`");
}

#[test]
fn an_empty_protocol_is_satisfied_by_implementing_it() {
    // Arrange
    // An implementation with no functions to write is still an implementation.
    let input = r#"
        proto Listable;
        struct Dog { n: Int }
        imp Listable for Dog { }
        fun list<T>(value: T): [T] where T is Listable => [value];
        list(Dog { n: 1 })
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn matching_method_names_do_not_satisfy_a_protocol() {
    // Arrange
    // `Sneaky` implements `Show`, which declares the same method as `Other`.
    // Implementing one is not implementing the other.
    let input = r#"
        proto Show { fun show(): String; }
        proto Other { fun show(): String; }
        struct Sneaky { }
        imp Show for Sneaky { fun show(): String => "s" }
        fun f<T>(x: T): Int where T is Other => 1
        f(Sneaky { })
    "#;

    // Act & Assert
    assert_error_contains(input, "does not implement `Other`");
}

#[test]
fn a_blanket_implementation_satisfies_a_bound() {
    // Arrange
    let input = r#"
        proto Show { fun show(): String; }
        struct B<T> { v: T }
        imp<T> Show for B<T> { fun show(): String => "b" }
        fun f<U>(x: U): Int where U is Show => 1
        f(B { v: 1 })
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn an_instantiation_implementation_satisfies_only_its_own_instantiation() {
    // Arrange
    let matching = r#"
        proto Show { fun show(): String; }
        struct B<T> { v: T }
        imp Show for B<Int> { fun show(): String => "i" }
        fun f<U>(x: U): Int where U is Show => 1
        f(B { v: 1 })
    "#;

    let mismatched = r#"
        proto Show { fun show(): String; }
        struct B<T> { v: T }
        imp Show for B<Int> { fun show(): String => "i" }
        fun f<U>(x: U): Int where U is Show => 1
        f(B { v: "s" })
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(matching).is_ok());
    assert_error_contains(mismatched, "does not implement `Show`");
}

// ---------------------------------------------------------------------------
// The semicolon form of an implementation
//
// `imp P for T;` is the empty implementation, the same as `imp P for T {}`.
// ---------------------------------------------------------------------------

#[test]
fn an_implementation_can_end_with_a_semicolon() {
    // Arrange
    let input = r#"
        proto Listable;
        struct Dog { n: Int }
        imp Listable for Dog;
        fun list<T>(value: T): [T] where T is Listable => [value];
        list(Dog { n: 1 })
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn the_semicolon_and_brace_forms_are_equivalent() {
    // Arrange
    let semicolon = r#"
        proto Listable;
        struct Dog { n: Int }
        imp Listable for Dog;
        fun list<T>(value: T): [T] where T is Listable => [value];
        list(Dog { n: 1 })
    "#;

    let braces = r#"
        proto Listable;
        struct Dog { n: Int }
        imp Listable for Dog { }
        fun list<T>(value: T): [T] where T is Listable => [value];
        list(Dog { n: 1 })
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(semicolon).is_ok());
    assert!(try_create_typed_ast(braces).is_ok());
}

#[test]
fn a_semicolon_implementation_can_be_generic() {
    // Arrange
    let input = r#"
        proto Listable;
        struct B<T> { v: T }
        imp<T> Listable for B<T>;
        fun list<U>(value: U): [U] where U is Listable => [value];
        list(B { v: 1 })
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_ok());
}

#[test]
fn a_semicolon_implementation_still_conflicts_with_an_overlapping_one() {
    // Arrange
    let input = r#"
        proto Listable;
        struct B<T> { v: T }
        imp<T> Listable for B<T>;
        imp Listable for B<Int>;
        0
    "#;

    // Act & Assert
    assert_error_contains(input, "Conflicting implementations of `Listable`");
}

#[test]
fn a_semicolon_implementation_cannot_skip_required_functions() {
    // Arrange
    // The short form is for protocols that require nothing; it is not a way to
    // leave a function unimplemented.
    let input = r#"
        proto Show { fun show(): String; }
        struct Dog { n: Int }
        imp Show for Dog;
        0
    "#;

    // Act & Assert
    assert_error_contains(input, "not implemented");
}
