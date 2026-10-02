//! The core library's `Iterator` and `Map`, and the type-checker behaviour they
//! needed.
//!
//! Every test here failed before the work that added them, most of them with an
//! error that named something other than what was wrong.

mod common;

use common::{create_env, evaluate_expression, try_create_typed_ast};
use interpreter::{value::Number, Value};

fn int(v: i64) -> Value {
    Value::Number(Number::Int(v))
}

/// A one-element iterator, which is enough to drive anything that wraps one.
const ONCE: &str = r#"
    proto Iterator { type Item; fun next(self: Self): Option<Item>; }

    struct Once<T> { v: T }
    imp<T> Iterator for Once<T> {
        type Item = T;
        fun next(self: Self): Option<Item> => {
            let r: Option<Item> = Option::Some { value: self.v };
            r
        }
    }
"#;

/// `Map` as the core library declares it.
const MAP: &str = r#"
    struct Map<TIterator, TTo> {
        iterator: TIterator,
        mapper: fun(TIterator::Item): TTo
    }

    imp<TIterator, TTo> Iterator for Map<TIterator, TTo>
    where TIterator is Iterator {
        type Item = TTo;
        fun next(self: Self): Option<Item> => {
            self.iterator:next() match
            | Option::Some { value } => {
                let mapped: Option<Item> = Option::Some { value: self.mapper(value) };
                mapped
            },
            | Option::None => {
                let done: Option<Item> = Option::None;
                done
            }
        }
    }
"#;

/// A pattern names a variant, not an instantiation, so it matches an
/// `Option<Int>` as readily as an `Option<String>`.
#[test]
fn an_instantiated_generic_enum_can_be_matched() {
    // Arrange
    let input = r#"
        enum E<T> { S { value: T }, N }
        let r: E<Int> = E::S { value: 3 };
        r match
        | E::S { value } => value,
        | E::N => 0
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(3));
}

#[test]
fn the_binding_from_a_generic_pattern_has_the_instantiated_type() {
    // Arrange
    let input = r#"
        let r: Option<Int> = Option::Some { value: 3 };
        let n: Int = r match
        | Option::Some { value } => value,
        | Option::None => 0;
        n
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(3));
}

/// Matching by name must not go so far as to accept a different enum.
#[test]
fn a_pattern_naming_another_enum_is_still_rejected() {
    // Arrange
    let input = r#"
        enum E<T> { S { value: T }, N }
        enum F<T> { S { value: T }, N }
        let r: E<Int> = E::S { value: 3 };
        r match
        | F::S { value } => value,
        | F::N => 0
    "#;

    // Act
    let error = try_create_typed_ast(input).expect_err("the pattern names the wrong enum");

    // Assert
    assert!(error.to_string().contains("names enum F"), "got: {error}");
}

/// A function type is structural: it is known when the types it is built from
/// are, and is never registered under a name of its own.
#[test]
fn a_function_held_in_a_struct_field_can_be_called() {
    // Arrange
    let input = r#"
        struct H { f: fun(Int): Int }
        let h = H { f: |x: Int|: Int x + 1 };
        h.f(1)
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(2));
}

/// Two implementations of one protocol are told apart by the value the method
/// is reached on, not by which was declared last.
#[test]
fn two_implementations_of_one_protocol_dispatch_on_the_receiver() {
    // Arrange
    let input = r#"
        proto P { fun get(self: Self): Int; }
        struct A { a: Int }
        struct B { b: Int }
        imp P for A { fun get(self: Self): Int => self.a }
        imp P for B { fun get(self: Self): Int => self.b * 2 }
        let x = A { a: 1 };
        let y = B { b: 5 };
        x:get() + y:get()
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(11));
}

/// The receiver's static type is a type parameter here, so the implementation
/// cannot be chosen while checking. The value knows what it is.
#[test]
fn a_protocol_method_on_a_type_parameter_dispatches_at_run_time() {
    // Arrange
    let input = r#"
        proto P { fun get(self: Self): Int; }
        struct A { a: Int }
        struct Wrapper<T> { inner: T }
        imp P for A { fun get(self: Self): Int => self.a }
        imp<T> P for Wrapper<T> where T is P {
            fun get(self: Self): Int => self.inner:get() * 2
        }
        let w = Wrapper { inner: A { a: 5 } };
        w:get()
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(10));
}

/// `fun(TIterator::Item): TTo` has to become `fun(Int): Int` when the struct is
/// instantiated.
///
/// Asserted by rejection: a field type left symbolic would take any mapper at
/// all, so the interesting evidence that the projection resolved is that the
/// wrong one no longer fits.
#[test]
fn a_projection_in_a_field_type_resolves_on_instantiation() {
    // Arrange
    let input = format!(
        r#"{ONCE}{MAP}
        let m: Map<Once<Int>, Int> = Map {{
            iterator: Once {{ v: 3 }},
            mapper: |x: String|: Int 1
        }};
        m
    "#
    );

    // Act
    let error = try_create_typed_ast(&input)
        .expect_err("a mapper taking a String cannot map an iterator of Int");

    // Assert
    assert!(
        error.to_string().contains("fun(Int): Int"),
        "expected the field to have resolved to `fun(Int): Int`, got: {error}"
    );
}

/// The whole of `core/iterator/map.ar`, as the library declares it.
#[test]
fn a_mapped_iterator_yields_the_mapped_value() {
    // Arrange
    let input = format!(
        r#"{ONCE}{MAP}
        let m: Map<Once<Int>, Int> = Map {{
            iterator: Once {{ v: 4 }},
            mapper: |x: Int|: Int x + 1
        }};
        m:next()
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result.to_string(), "Option::Some { value: 5 }");
}
