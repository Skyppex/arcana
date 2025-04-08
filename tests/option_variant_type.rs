mod common;

use common::{create_env, create_typed_ast, evaluate_expression, StatementExt, VecStatementExt};

use interpreter::{value::Enum, Value};
use shared::{
    type_checker::{ast::Typed, Struct, StructField, Type},
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
        Type::Struct(Struct {
            type_identifier: TypeIdentifier::MemberType(
                Box::new(TypeIdentifier::Type("O".to_owned())),
                "S".to_owned()
            ),
            embedded_structs: vec![],
            fields: vec![StructField {
                struct_name: TypeIdentifier::MemberType(
                    Box::new(TypeIdentifier::Type("O".to_owned())),
                    "S".to_owned()
                ),
                field_name: "x".to_owned(),
                default_value: None,
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
        Value::Enum(Enum {
            type_name: "O".to_string(),
            enum_member: interpreter::value::Struct {
                type_name: "O::S".to_string(),
                fields: vec![interpreter::value::StructField {
                    identifier: "x".to_string(),
                    value: Value::Number(interpreter::value::Number::Int(1))
                }]
            },
        })
    );
}
