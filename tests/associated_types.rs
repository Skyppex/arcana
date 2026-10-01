//! A protocol may declare a type that each implementation chooses for itself.
//!
//! Unlike a type parameter, which the caller picks, an associated type is
//! settled by the implementation — so a type implements such a protocol once,
//! and everything downstream can refer to the choice without threading it
//! through as another parameter.

mod common;

use common::{create_env, evaluate_expression, try_create_typed_ast};

use interpreter::{value::Number, Value};

fn int(v: i64) -> Value {
    Value::Number(Number::Int(v))
}

/// A protocol with one associated type, and a struct implementing it.
const HOLDER: &str = r#"
    proto Holder { type Item; fun get(self: Self): Item; }
    struct B { v: Int }
    imp Holder for B { type Item = Int; fun get(self: Self): Item => self.v }
"#;

#[test]
fn an_implementation_chooses_the_associated_type() {
    // Arrange
    let input = format!(
        r#"{HOLDER}
        let b = B {{ v: 3 }};
        b:get()
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(3));
}

#[test]
fn the_chosen_type_is_what_the_call_returns() {
    // Arrange: the declared return is `Item`, which for `B` is `Int`.
    let input = format!(
        r#"{HOLDER}
        let b = B {{ v: 3 }};
        let n: Int = b:get();
        n
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(3));
}

#[test]
fn a_projection_names_the_chosen_type() {
    // Arrange: `B::Item` is whatever `B`'s implementation chose.
    let input = format!(
        r#"{HOLDER}
        let b = B {{ v: 3 }};
        let n: B::Item = b:get();
        n
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(3));
}

#[test]
fn a_generic_function_may_return_a_projection_on_its_parameter() {
    // Arrange: `I::Item` is not known while `I` is still a parameter; it is
    // settled when `I` is instantiated. This is the whole point of an
    // associated type — the signature needs no second parameter for it.
    let input = format!(
        r#"{HOLDER}
        fun get2<I>(x: I): I::Item where I is Holder => x:get()
        let b = B {{ v: 3 }};
        get2(b)
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(3));
}

#[test]
fn an_associated_type_may_be_a_struct() {
    // Arrange
    let input = r#"
        proto Holder { type Item; fun get(self: Self): Item; }
        struct Wrapped { n: Int }
        struct B { v: Int }
        imp Holder for B {
            type Item = Wrapped;
            fun get(self: Self): Item => Wrapped { n: self.v }
        }
        let b = B { v: 4 };
        let w: Wrapped = b:get();
        w.n
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(4));
}

// --- Rejections -------------------------------------------------------------

#[test]
fn an_implementation_must_choose_every_associated_type() {
    // Arrange
    let input = r#"
        proto Holder { type Item; fun get(self: Self): Item; }
        struct B { v: Int }
        imp Holder for B { fun get(self: Self): Int => self.v }
        0
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert
    let error = result.unwrap_err().to_string();
    assert!(error.contains("missing associated type"), "{}", error);
    assert!(error.contains("Item"), "{}", error);
}

#[test]
fn an_implementation_cannot_choose_a_type_the_protocol_never_declared() {
    // Arrange
    let input = r#"
        proto Holder { type Item; fun get(self: Self): Item; }
        struct B { v: Int }
        imp Holder for B { type Nope = Int; type Item = Int; fun get(self: Self): Item => self.v }
        0
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("has no associated type `Nope`"));
}

#[test]
fn an_associated_type_is_written_unqualified() {
    // Arrange: `Self::Item` would collide with an enum's variants, which `::`
    // already spells, so the name stands alone.
    let input = r#"
        proto Holder { type Item; fun get(self: Self): Item; }
        struct B { v: Int }
        imp Holder for B { type Item = Int; fun get(self: Self): Self::Item => self.v }
        0
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert
    let error = result.unwrap_err().to_string();
    assert!(error.contains("written `Item`"), "{}", error);
}

#[test]
fn a_variant_and_an_associated_type_cannot_share_a_name() {
    // Arrange: `E::Item` would otherwise mean the variant in one place and the
    // projection in another.
    let input = r#"
        proto Holder { type Item; fun get(self: Self): Item; }
        enum E { Item { v: Int }, Other }
        imp Holder for E { type Item = Int; fun get(self: Self): Item => 1 }
        0
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert
    let error = result.unwrap_err().to_string();
    assert!(error.contains("would be ambiguous"), "{}", error);
}

#[test]
fn an_enum_may_implement_a_protocol_when_no_name_collides() {
    // Arrange
    let input = r#"
        proto Holder { type Item; fun get(self: Self): Item; }
        enum E { First { v: Int }, Other }
        imp Holder for E { type Item = Int; fun get(self: Self): Item => 1 }
        let e: E = E::Other;
        e:get()
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}
