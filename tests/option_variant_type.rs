mod common;

use std::collections::HashMap;

use common::{create_env, create_typed_ast, evaluate_expression, StatementExt, VecStatementExt};

use interpreter::{value::EnumFields, Value};
use shared::{
    type_checker::{ast::Typed, EnumMember, StructField, Type},
    types::TypeIdentifier,
};

#[test]
fn enum_variant_can_be_used_as_a_type() {
    // Arrange
    let input = r#"
        enum O {
            S { x: Int },
            N
        }

        let x: O::S;
        "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(1)
        .unwrap_semi()
        .unwrap_expression();

    assert_eq!(
        expression.get_type(),
        Type::EnumMember(EnumMember {
            enum_name: TypeIdentifier::Type("O".to_owned()),
            discriminant_name: "S".to_owned(),
            embedded_structs: vec![],
            fields: vec![StructField {
                struct_name: TypeIdentifier::MemberType(
                    Box::new(TypeIdentifier::Type("O".to_owned())),
                    "S".to_owned()
                ),
                field_name: "x".to_owned(),
                field_type: Type::Int
            }]
        })
    )
}

#[test]
#[ignore]
fn enum_variant_can_be_assigned_to_variable_with_variant_type() {
    // Arrange
    let input = r#"
        enum O {
            S(x: Int),
            N
        }

        let x: O::S = O::S(x: 1);
        "#;

    let environment = create_env();

    // Act
    let value = evaluate_expression(input, environment, true);

    // Assert
    assert_eq!(
        value,
        Value::Enum {
            enum_member: interpreter::value::EnumMember {
                enum_name: shared::types::TypeAnnotation::Type("O".to_string()),
                member_name: "S".to_string(),
            },
            fields: EnumFields::Named(HashMap::from([(
                "x".to_string(),
                Value::Number(interpreter::value::Number::Int(1))
            )]))
        }
    );
}
