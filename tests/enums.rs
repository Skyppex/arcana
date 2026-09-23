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
fn enum_variant_without_fields_can_be_constructed() {
    // Arrange
    let input = r#"
        enum MyEnum { First, Second }
        MyEnum::First
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(
        result,
        Value::Enum(Enum {
            type_name: "MyEnum".to_owned(),
            enum_member: Struct {
                type_name: "MyEnum::First".to_owned(),
                fields: vec![],
            },
        })
    );
}

#[test]
fn enum_variant_with_fields_can_be_constructed() {
    // Arrange
    let input = r#"
        enum MyEnum { First { value: Int }, Second }
        MyEnum::First { value: 2 }
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(
        result,
        Value::Enum(Enum {
            type_name: "MyEnum".to_owned(),
            enum_member: Struct {
                type_name: "MyEnum::First".to_owned(),
                fields: vec![StructField {
                    identifier: "value".to_owned(),
                    value: int(2),
                }],
            },
        })
    );
}

#[test]
fn enum_variant_carries_shared_fields() {
    // Arrange
    // A shared field is declared once on the enum and exists on every variant.
    let input = r#"
        enum MyEnum { id: Int, First, Second }
        MyEnum::First { id: 2 }
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(
        result,
        Value::Enum(Enum {
            type_name: "MyEnum".to_owned(),
            enum_member: Struct {
                type_name: "MyEnum::First".to_owned(),
                fields: vec![StructField {
                    identifier: "id".to_owned(),
                    value: int(2),
                }],
            },
        })
    );
}

#[test]
fn enum_variant_missing_a_field_is_error() {
    // Arrange
    let input = r#"
        enum MyEnum { First { value: Int }, Second }
        MyEnum::First { }
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

#[test]
fn enum_unknown_variant_is_error() {
    // Arrange
    let input = r#"
        enum MyEnum { First, Second }
        MyEnum::Third
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

// --- Let bindings -----------------------------------------------------------

#[test]
fn enum_variant_can_be_bound_with_the_enum_type() {
    // Arrange
    // The annotation widens the variant to the enum, so `e` could hold either.
    let input = r#"
        enum MyEnum { id: Int, First, Second }
        let e: MyEnum = MyEnum::First { id: 2 };
        e.id
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(2));
}

#[test]
fn enum_variant_can_be_bound_with_the_variant_type() {
    // Arrange
    let input = r#"
        enum MyEnum { First { value: Int }, Second }
        let e: MyEnum::First = MyEnum::First { value: 3 };
        e.value
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(3));
}

#[test]
fn enum_variant_bound_without_annotation_keeps_the_variant_type() {
    // Arrange
    // With no annotation `e` is `MyEnum::First`, so the variant's own fields
    // are reachable without discriminating first.
    let input = r#"
        enum MyEnum { First { value: Int }, Second }
        let e = MyEnum::First { value: 3 };
        e.value
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(3));
}

// --- Field access -----------------------------------------------------------

#[test]
fn shared_fields_are_reachable_from_an_enum_typed_value() {
    // Arrange
    // `id` is guaranteed to exist whichever variant is held.
    let input = r#"
        enum MyEnum { id: Int, First, Second }
        let e: MyEnum = MyEnum::Second { id: 7 };
        e.id
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(7));
}

#[test]
fn variant_fields_are_not_reachable_from_an_enum_typed_value() {
    // Arrange
    // `value` is only on First, so it cannot be read without knowing the
    // variant.
    let input = r#"
        enum MyEnum { First { value: Int }, Second }
        let e: MyEnum = MyEnum::First { value: 1 };
        e.value
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

// --- Matching ---------------------------------------------------------------

#[test]
fn enum_can_be_matched_on_its_variants() {
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
fn matching_a_variant_binds_its_fields() {
    // Arrange
    let input = r#"
        enum MyEnum { id: Int, First, Second }
        let e: MyEnum = MyEnum::Second { id: 5 };
        e match
        | ::First => 0,
        | ::Second { id } => id
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(5));
}

#[test]
fn enum_can_be_matched_as_a_struct_over_shared_fields() {
    // Arrange
    // No variant is named, so this matches whichever variant is held.
    let input = r#"
        enum MyEnum { id: Int, First, Second }
        let e: MyEnum = MyEnum::Second { id: 5 };
        e match
        | { id } => id
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(5));
}
