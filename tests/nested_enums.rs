//! An enum variant may itself be an enum, written `enum Name { .. }` in place
//! of a struct variant.
//!
//! A variant of a nested enum is named by a longer path — `::E2::S3` — and a
//! path may stop at any level and bind what it matched.

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
    assert!(result.is_ok(), "{}", result.unwrap_err().to_string());
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
    assert!(result.is_ok(), "{}", result.unwrap_err().to_string());
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
    let error = result.unwrap_err().to_string();
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
    let error = result.unwrap_err().to_string();
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
    assert!(result.unwrap_err().to_string().contains("Dup"));
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
    assert!(result.unwrap_err().to_string().contains("type parameters"));
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

// --- Empty enums ------------------------------------------------------------

#[test]
fn an_enum_with_no_variants_is_rejected() {
    // Arrange
    let input = r#"
        enum Empty { }
        0
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert
    assert!(result.unwrap_err().to_string().contains("no variants"));
}

#[test]
fn a_forward_declared_enum_is_rejected_as_having_no_variants() {
    // Arrange: `enum Foo;` parses, so that the error names the real problem.
    let input = r#"
        enum Empty;
        0
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert
    assert!(result.unwrap_err().to_string().contains("no variants"));
}

#[test]
fn a_nested_enum_with_no_variants_is_rejected() {
    // Arrange
    let input = r#"
        enum Outer { S1, enum Inner { } }
        0
    "#;

    // Act
    let result = try_create_typed_ast(input);

    // Assert
    let error = result.unwrap_err().to_string();
    assert!(error.contains("Outer::Inner"), "{}", error);
    assert!(error.contains("no variants"), "{}", error);
}

// --- Matching ---------------------------------------------------------------

/// The shape from the design discussion: struct variants, a nested enum, and a
/// nested enum whose only variant is itself a nested enum.
const E1: &str = r#"
    enum E1 {
        S1 { f1: Int },
        enum E2 {
            S2,
            S3 { f2: Int }
        },
        enum E3 {
            enum E4 {
                S4,
                S5 { f3: Int }
            }
        }
    }
"#;

#[test]
fn every_leaf_path_can_be_matched_exhaustively() {
    // Arrange
    let input = format!(
        r#"{E1}
        let e: E1 = E1::E3::E4::S5 {{ f3: 7 }};
        e match
        | ::S1 {{ f1: f }} => f,
        | ::E2::S2 => 2,
        | ::E2::S3 {{ f2: f }} => f,
        | ::E3::E4::S4 => 4,
        | ::E3::E4::S5 {{ f3: f }} => f
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(7));
}

#[test]
fn an_arm_for_a_whole_nested_enum_covers_all_of_its_variants() {
    // Arrange
    let input = format!(
        r#"{E1}
        let e: E1 = E1::E2::S3 {{ f2: 5 }};
        e match
        | ::S1 s1 => 1,
        | ::E2 e2 => 2,
        | ::E3 e3 => 3
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(2));
}

#[test]
fn a_path_may_stop_at_an_intermediate_nested_enum() {
    // Arrange
    let input = format!(
        r#"{E1}
        let e: E1 = E1::E3::E4::S4;
        e match
        | ::S1 s1 => 1,
        | ::E2 e2 => 2,
        | ::E3::E4 e4 => 4
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(4));
}

#[test]
fn a_bound_struct_variant_has_the_variants_own_type() {
    // Arrange
    let input = format!(
        r#"{E1}
        let e: E1 = E1::S1 {{ f1: 9 }};
        e match
        | ::S1 s1 => s1.f1,
        | ::E2 e2 => 2,
        | ::E3 e3 => 3
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(9));
}

#[test]
fn a_bound_nested_enum_can_be_passed_where_that_enum_is_expected() {
    // Arrange
    let input = format!(
        r#"{E1}
        fun take_e2(x: E1::E2): Int {{ 42 }}
        let e: E1 = E1::E2::S2;
        e match
        | ::S1 s1 => 1,
        | ::E2 e2 => take_e2(e2),
        | ::E3 e3 => 3
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(42));
}

#[test]
fn a_bound_deep_nested_enum_has_its_own_type() {
    // Arrange
    let input = format!(
        r#"{E1}
        fun take_e4(x: E1::E3::E4): Int {{ 44 }}
        let e: E1 = E1::E3::E4::S4;
        e match
        | ::S1 s1 => 1,
        | ::E2 e2 => 2,
        | ::E3::E4 e4 => take_e4(e4)
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(44));
}

#[test]
fn a_bound_leaf_widens_to_an_enclosing_enum() {
    // Arrange: binding the deepest level loses nothing, because a variant is
    // assignable to every enum it is declared inside.
    let input = format!(
        r#"{E1}
        fun take_e2(x: E1::E2): Int {{ 42 }}
        let e: E1 = E1::E2::S3 {{ f2: 1 }};
        e match
        | ::S1 s1 => 1,
        | ::E2::S2 => 2,
        | ::E2::S3 s3 => take_e2(s3),
        | ::E3 e3 => 3
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(42));
}

#[test]
fn a_binder_of_the_wrong_variant_type_is_rejected() {
    // Arrange
    let input = format!(
        r#"{E1}
        fun take_e2(x: E1::E2): Int {{ 42 }}
        let e: E1 = E1::S1 {{ f1: 1 }};
        e match
        | ::S1 s1 => take_e2(s1),
        | ::E2 e2 => 2,
        | ::E3 e3 => 3
    "#
    );

    // Act
    let result = try_create_typed_ast(&input);

    // Assert
    assert!(result.is_err());
}

#[test]
fn a_variant_path_may_be_written_fully_qualified() {
    // Arrange
    let input = format!(
        r#"{E1}
        let e: E1 = E1::E2::S2;
        e match
        | E1::S1 {{ f1: f }} => f,
        | E1::E2::S2 => 2,
        | E1::E2::S3 {{ f2: f }} => f,
        | E1::E3 x => 3
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(2));
}

#[test]
fn a_nested_enums_shared_fields_are_readable_once_narrowed_to_it() {
    // Arrange
    let input = r#"
        enum Outer {
            S1,
            enum Inner { tag: Int, A, B }
        }
        let e: Outer = Outer::Inner::B { tag: 6 };
        e match
        | ::S1 => 0,
        | ::Inner { tag } => tag
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, int(6));
}

// --- Matching: rejections ---------------------------------------------------

#[test]
fn leaving_a_nested_leaf_uncovered_is_rejected() {
    // Arrange
    let input = format!(
        r#"{E1}
        let e: E1 = E1::E2::S2;
        e match
        | ::S1 s1 => 1,
        | ::E2::S2 => 2,
        | ::E3 e3 => 3
    "#
    );

    // Act
    let result = try_create_typed_ast(&input);

    // Assert
    let error = result.unwrap_err().to_string();
    assert!(error.contains("not exhaustive"), "{}", error);
    assert!(error.contains("E1::E2::S3"), "{}", error);
}

#[test]
fn leaving_a_whole_nested_enum_uncovered_is_rejected() {
    // Arrange
    let input = format!(
        r#"{E1}
        let e: E1 = E1::E2::S2;
        e match
        | ::S1 s1 => 1,
        | ::E2 e2 => 2
    "#
    );

    // Act
    let result = try_create_typed_ast(&input);

    // Assert
    let error = result.unwrap_err().to_string();
    assert!(error.contains("not exhaustive"), "{}", error);
    assert!(error.contains("E1::E3"), "{}", error);
}

#[test]
fn an_arm_subsumed_by_a_broader_path_is_unreachable() {
    // Arrange
    let input = format!(
        r#"{E1}
        let e: E1 = E1::E2::S2;
        e match
        | ::S1 s1 => 1,
        | ::E2 e2 => 2,
        | ::E2::S2 => 22,
        | ::E3 e3 => 3
    "#
    );

    // Act
    let result = try_create_typed_ast(&input);

    // Assert
    assert!(result.unwrap_err().to_string().contains("unreachable"));
}

#[test]
fn binding_a_variant_and_destructuring_it_is_rejected() {
    // Arrange
    let input = format!(
        r#"{E1}
        let e: E1 = E1::E2::S2;
        e match
        | ::S1 s1 {{ f1: f }} => 1,
        | _ => 0
    "#
    );

    // Act
    let result = try_create_typed_ast(&input);

    // Assert
    assert!(result.unwrap_err().to_string().contains("one or the other"));
}

#[test]
fn the_first_segment_is_rooted_and_never_searched_for() {
    // Arrange: `S2` exists, but only inside `E2`, so `::S2` does not find it.
    let input = format!(
        r#"{E1}
        let e: E1 = E1::E2::S2;
        e match
        | ::S2 => 2,
        | _ => 0
    "#
    );

    // Act
    let result = try_create_typed_ast(&input);

    // Assert
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("has no variant named `S2`"));
}

#[test]
fn a_path_through_a_struct_variant_is_rejected() {
    // Arrange
    let input = format!(
        r#"{E1}
        let e: E1 = E1::E2::S2;
        e match
        | ::S1::Nope => 1,
        | _ => 0
    "#
    );

    // Act
    let result = try_create_typed_ast(&input);

    // Assert
    assert!(result.unwrap_err().to_string().contains("is not an enum"));
}

// --- `@` on variant patterns ------------------------------------------------

#[test]
fn a_variant_can_be_bound_and_destructured_with_at() {
    // Arrange: the binder and the field pattern are exclusive on their own,
    // so `@` is how to ask for both.
    let input = format!(
        r#"{E1}
        fun take_s3(x: E1::E2::S3): Int {{ x.f2 }}
        let e: E1 = E1::E2::S3 {{ f2: 5 }};
        e match
        | ::S1 s1 => 1,
        | ::E2::S3 s3 @ {{ f2: 5 }} => take_s3(s3),
        | ::E2::S3 other => 33,
        | ::E2::S2 => 2,
        | ::E3 e3 => 3
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(5));
}

#[test]
fn the_right_of_at_is_rooted_at_the_narrowed_type() {
    // Arrange: `::E4::S4` resolves against `E1::E3`, not against `E1`, and
    // `e3` is bound at the level the path had reached.
    let input = format!(
        r#"{E1}
        fun take_e3(x: E1::E3): Int {{ 33 }}
        let e: E1 = E1::E3::E4::S4;
        e match
        | ::S1 s1 => 1,
        | ::E2 e2 => 2,
        | ::E3 e3 @ ::E4::S4 => take_e3(e3),
        | ::E3 e3 => 3
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(33));
}

#[test]
fn a_bare_binder_before_at_binds_at_the_matched_type() {
    // Arrange: nothing narrows before the binder, so `whole` is an `E1`.
    let input = format!(
        r#"{E1}
        fun take_e1(x: E1): Int {{ 11 }}
        let e: E1 = E1::E2::S2;
        e match
        | whole @ ::E2 => take_e1(whole),
        | ::S1 s1 => 1,
        | ::E3 e3 => 3
    "#
    );

    // Act
    let result = evaluate_expression(&input, create_env(), false);

    // Assert
    assert_eq!(result, int(11));
}

#[test]
fn a_constrained_variant_binding_does_not_cover_the_whole_variant() {
    // Arrange: `@ { f2: 5 }` is refutable, so `S3` still needs covering.
    let input = format!(
        r#"{E1}
        let e: E1 = E1::E2::S3 {{ f2: 5 }};
        e match
        | ::S1 s1 => 1,
        | ::E2::S3 s3 @ {{ f2: 5 }} => 55,
        | ::E2::S2 => 2,
        | ::E3 e3 => 3
    "#
    );

    // Act
    let result = try_create_typed_ast(&input);

    // Assert
    assert!(result.unwrap_err().to_string().contains("not exhaustive"));
}
