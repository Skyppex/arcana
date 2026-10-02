//! `where` clauses on a protocol declaration.
//!
//! A protocol may require things of whatever implements it — `where Self is
//! Iterator` — and of its own type parameters. `Self` carries a bound because
//! inside the declaration it is a type parameter like any other: nobody has
//! chosen it yet, and an implementation is what settles it.

mod common;

use common::{create_env, evaluate_expression, try_create_typed_ast};
use interpreter::{value::Number, Value};

fn int(v: i64) -> Value {
    Value::Number(Number::Int(v))
}

const ITERATOR: &str = r#"
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

const DOUBLE_SIDED: &str = r#"
    proto DoubleSidedIterator
    where Self is Iterator {
        fun prev(self: Self): Option<Item>;
    }
"#;

#[test]
fn a_bound_on_self_is_satisfied_by_an_existing_implementation() {
    // Arrange
    let input = format!(
        r#"{ITERATOR}{DOUBLE_SIDED}
        imp<T> DoubleSidedIterator for Once<T> {{
            fun prev(self: Self): Option<Item> => {{
                let r: Option<Item> = Option::None;
                r
            }}
        }}

        let o = Once {{ v: 7 }};
        o:prev()
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result.to_string(), "Option::None {  }");
}

#[test]
fn a_bound_on_self_rejects_a_type_that_does_not_satisfy_it() {
    // Arrange
    let input = format!(
        r#"{ITERATOR}{DOUBLE_SIDED}
        struct Bare {{ n: Int }}

        imp DoubleSidedIterator for Bare {{
            fun prev(self: Self): Option<Int> => {{
                let r: Option<Int> = Option::None;
                r
            }}
        }}
        0
    "#
    );

    // Act
    let error = try_create_typed_ast(&input).expect_err("`Bare` does not iterate");

    // Assert
    assert!(
        error.to_string().contains(
            "`Bare` cannot implement `DoubleSidedIterator` without implementing `Iterator`"
        ),
        "got: {error}"
    );
}

/// The bound guarantees an `Iterator` implementation, and that implementation
/// already chose `Item` — so the protocol can name it without declaring a
/// second one of its own.
#[test]
fn a_bound_brings_the_associated_types_it_guarantees_into_scope() {
    // Arrange
    // `DoubleSidedIterator` declares no `type Item;`, and still mentions `Item`.
    let input = format!(
        r#"{ITERATOR}{DOUBLE_SIDED}
        imp<T> DoubleSidedIterator for Once<T> {{
            fun prev(self: Self): Option<Item> => {{
                let r: Option<Item> = Option::Some {{ value: self.v }};
                r
            }}
        }}

        let o = Once {{ v: 3 }};
        o:prev()
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    // Asserted on the value rather than by matching: a protocol method's return
    // type is not yet specialised to the receiver, so `o:prev()` is still
    // `Option<T>` to the checker and cannot be matched without an annotation.
    assert_eq!(result.to_string(), "Option::Some { value: 3 }");
}

const SHOW: &str = r#"
    proto Show { fun show(self: Self): String; }
    struct Good { n: Int }
    imp Show for Good { fun show(self: Self): String => "good" }

    proto Holder<T> where T is Show { fun held(self: Self): T; }
"#;

#[test]
fn a_bound_on_a_type_parameter_is_satisfied_by_the_argument() {
    // Arrange
    let input = format!(
        r#"{SHOW}
        struct Box1 {{ v: Good }}
        imp Holder<Good> for Box1 {{ fun held(self: Self): Good => self.v }}

        let b = Box1 {{ v: Good {{ n: 1 }} }};
        b:held():show()
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, Value::String("good".to_owned()));
}

#[test]
fn a_bound_on_a_type_parameter_rejects_an_argument_that_does_not_satisfy_it() {
    // Arrange
    let input = format!(
        r#"{SHOW}
        struct Bad {{ n: Int }}
        struct Box2 {{ v: Bad }}
        imp Holder<Bad> for Box2 {{ fun held(self: Self): Bad => self.v }}
        0
    "#
    );

    // Act
    let error = try_create_typed_ast(&input).expect_err("`Bad` does not implement `Show`");

    // Assert
    assert!(
        error
            .to_string()
            .contains("`Holder` requires `T` to implement `Show`, and `Bad` does not"),
        "got: {error}"
    );
}

const TWO: &str = r#"
    proto A { fun a(self: Self): Int; }
    proto B { fun b(self: Self): Int; }
    struct S { n: Int }
    imp A for S { fun a(self: Self): Int => 1 }
"#;

#[test]
fn every_bound_joined_by_and_has_to_hold() {
    // Arrange
    // `S` implements `A` but not `B`.
    let input = format!(
        r#"{TWO}
        proto Both where Self is A and B {{ fun c(self: Self): Int; }}
        imp Both for S {{ fun c(self: Self): Int => 2 }}
        0
    "#
    );

    // Act
    let error = try_create_typed_ast(&input).expect_err("`S` does not implement `B`");

    // Assert
    assert!(
        error
            .to_string()
            .contains("`S` cannot implement `Both` without implementing `B`"),
        "got: {error}"
    );
}

#[test]
fn several_bounds_are_satisfied_together() {
    // Arrange
    let input = format!(
        r#"{TWO}
        imp B for S {{ fun b(self: Self): Int => 9 }}
        proto Both where Self is A and B {{ fun c(self: Self): Int; }}
        imp Both for S {{ fun c(self: Self): Int => 2 }}

        let s = S {{ n: 0 }};
        s:c()
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(2));
}

/// A protocol with no body still takes a `where` clause.
#[test]
fn a_marker_protocol_may_carry_a_bound() {
    // Arrange
    let input = format!(
        r#"{TWO}
        proto Marker where Self is A;
        imp Marker for S;
        let s = S {{ n: 4 }};
        s:a()
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}

/// Nothing to hold against an implementation, which is the common case.
#[test]
fn a_protocol_without_a_bound_is_unaffected() {
    // Arrange
    let input = format!(
        r#"{TWO}
        struct Other {{ m: Int }}
        imp A for Other {{ fun a(self: Self): Int => 7 }}
        let o = Other {{ m: 0 }};
        o:a()
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(7));
}
