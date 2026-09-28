//! An enum variant may itself be an enum, written `enum Name { .. }` in place
//! of a struct variant.
//!
//! Matching over a nested enum is not implemented yet — see the tests at the
//! bottom, which record that it is rejected rather than silently misbehaving.

mod common;

use common::{create_env, evaluate_expression, try_create_typed_ast};

use interpreter::{
    value::{Enum, Number, Struct, StructField},
    Value,
};

fn int(v: i64) -> Value {
    Value::Number(Number::Int(v))
}

// --- Construction -----------------------------------------------------------

#[test]
fn nested_enum_variant_without_fields_can_be_constructed() {
    // Arrange
    let input = r#"
        enum Outer { Struct { id: Int }, enum Inner { Variant, OtherVariant } }
        Outer::Inner::Variant
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(
        result,
        Value::Enum(Enum {
            // The enclosing enum of a nested variant is the nested enum, not
            // the outermost one.
            type_name: "Outer::Inner".to_owned(),
            enum_member: Struct {
                type_name: "Outer::Inner::Variant".to_owned(),
                fields: vec![],
            },
        })
    );
}

#[test]
fn nested_enum_variant_with_fields_can_be_constructed() {
    // Arrange
    let input = r#"
        enum Outer { Struct { id: Int }, enum Inner { Variant { value: Int } } }
        Outer::Inner::Variant { value: 7 }
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(
        result,
        Value::Enum(Enum {
            type_name: "Outer::Inner".to_owned(),
            enum_member: Struct {
                type_name: "Outer::Inner::Variant".to_owned(),
                fields: vec![StructField {
                    identifier: "value".to_owned(),
                    value: int(7),
                }],
            },
        })
    );
}

#[test]
fn a_struct_variant_may_be_written_with_the_struct_keyword() {
    // Arrange
    let input = r#"
        enum Outer { struct S { id: Int }, enum Inner { Variant } }
        Outer::S { id: 1 }
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(
        result,
        Value::Enum(Enum {
            type_name: "Outer".to_owned(),
            enum_member: Struct {
                type_name: "Outer::S".to_owned(),
                fields: vec![StructField {
                    identifier: "id".to_owned(),
                    value: int(1),
                }],
            },
        })
    );
}

#[test]
fn enums_can_be_nested_more_than_one_level_deep() {
    // Arrange
    let input = r#"
        enum A { enum B { enum C { Leaf { n: Int } } } }
        A::B::C::Leaf { n: 42 }
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(
        result,
        Value::Enum(Enum {
            type_name: "A::B::C".to_owned(),
            enum_member: Struct {
                type_name: "A::B::C::Leaf".to_owned(),
                fields: vec![StructField {
                    identifier: "n".to_owned(),
                    value: int(42),
                }],
            },
        })
    );
}

// --- Typing -----------------------------------------------------------------

#[test]
fn a_nested_variant_is_assignable_to_the_outermost_enum() {
    // Arrange
    let input = r#"
        enum Outer { Struct { id: Int }, enum Inner { Variant } }
        let x: Outer = Outer::Inner::Variant;
        x
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert
    assert!(result.is_ok(), "{}", result.unwrap_err());
}

#[test]
fn a_nested_variant_is_assignable_to_its_own_enum() {
    // Arrange
    let input = r#"
        enum Outer { Struct { id: Int }, enum Inner { Variant } }
        let x: Outer::Inner = Outer::Inner::Variant;
        x
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert
    assert!(result.is_ok(), "{}", result.unwrap_err());
}

#[test]
fn a_sibling_variant_is_not_assignable_to_a_nested_enum() {
    // Arrange
    let input = r#"
        enum Outer { Struct { id: Int }, enum Inner { Variant } }
        let x: Outer::Inner = Outer::Struct { id: 1 };
        x
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert
    assert!(result.is_err());
}

#[test]
fn a_nested_enum_may_have_shared_fields_when_its_variants_are_all_structs() {
    // Arrange
    let input = r#"
        enum Outer { Struct { id: Int }, enum Inner { tag: Int, Variant } }
        let x: Outer::Inner = Outer::Inner::Variant { tag: 3 };
        x.tag
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(3));
}

#[test]
fn a_nested_enum_of_a_generic_enum_gets_its_type_arguments() {
    // Arrange
    let input = r#"
        enum Outer<T> { Struct { id: T }, enum Inner { Variant { value: T } } }
        let x: Outer<Int> = Outer::Inner::Variant { value: 9 };
        x
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(
        result,
        Value::Enum(Enum {
            type_name: "Outer::Inner".to_owned(),
            enum_member: Struct {
                type_name: "Outer::Inner::Variant".to_owned(),
                fields: vec![StructField {
                    identifier: "value".to_owned(),
                    value: int(9),
                }],
            },
        })
    );
}

#[test]
fn a_generic_nested_variant_rejects_a_field_of_the_wrong_type() {
    // Arrange
    let input = r#"
        enum Outer<T> { Struct { id: T }, enum Inner { Variant { value: T } } }
        let x: Outer<Int> = Outer::Inner::Variant { value: "nine" };
        x
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert
    assert!(result.is_err());
}

// --- Shape rules ------------------------------------------------------------

#[test]
fn an_enum_with_a_nested_enum_cannot_have_shared_fields() {
    // Arrange
    let input = r#"
        enum Outer { id: Int, Struct, enum Inner { Variant } }
        0
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert
    let error = result.unwrap_err();
    assert!(error.contains("Outer"), "{}", error);
    assert!(error.contains("Inner"), "{}", error);
    assert!(error.contains("shared fields"), "{}", error);
}

#[test]
fn the_shared_field_rule_applies_to_nested_enums_too() {
    // Arrange
    let input = r#"
        enum Outer { enum Inner { tag: Int, enum Deep { Leaf } } }
        0
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert
    let error = result.unwrap_err();
    assert!(error.contains("Outer::Inner"), "{}", error);
    assert!(error.contains("Deep"), "{}", error);
}

#[test]
fn sibling_variants_cannot_share_a_name() {
    // Arrange
    let input = r#"
        enum Outer { Dup, Dup }
        0
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert
    assert!(result.unwrap_err().contains("Dup"));
}

#[test]
fn a_nested_enum_and_its_parent_may_each_have_a_variant_of_the_same_name() {
    // Arrange: the qualified names differ, so there is nothing to collide.
    let input = r#"
        enum Outer { Same { id: Int }, enum Inner { Same } }
        Outer::Inner::Same
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(
        result,
        Value::Enum(Enum {
            type_name: "Outer::Inner".to_owned(),
            enum_member: Struct {
                type_name: "Outer::Inner::Same".to_owned(),
                fields: vec![],
            },
        })
    );
}

#[test]
fn a_nested_enum_cannot_declare_its_own_type_parameters() {
    // Arrange
    let input = r#"
        enum Outer { enum Inner<T> { Variant { value: T } } }
        0
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert
    assert!(result.unwrap_err().contains("type parameters"));
}

// --- Matching is not implemented yet ----------------------------------------

#[test]
fn matching_a_nested_variant_is_rejected_for_now() {
    // Arrange
    let input = r#"
        enum Outer { Struct { id: Int }, enum Inner { Variant, Other } }
        let x: Outer = Outer::Inner::Variant;
        x match
        | Outer::Struct { id } => id,
        | Outer::Inner::Variant => 1,
        | Outer::Inner::Other => 2
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert: rejected, not miscompiled.
    assert!(result.is_err());
}

#[test]
fn matching_a_whole_nested_enum_is_rejected_for_now() {
    // Arrange
    let input = r#"
        enum Outer { Struct { id: Int }, enum Inner { Variant, Other } }
        let x: Outer = Outer::Inner::Variant;
        x match
        | Outer::Struct { id } => id,
        | Outer::Inner => 1
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert: rejected, not miscompiled.
    assert!(result.is_err());
}

#[test]
fn matching_a_flat_enum_is_unaffected() {
    // Arrange
    let input = r#"
        enum MyEnum { First, Second }
        let e: MyEnum = MyEnum::First;
        e match
        | MyEnum::First => 1,
        | MyEnum::Second => 2
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(1));
}
