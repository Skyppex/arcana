mod common;

use std::collections::HashMap;

use common::{create_typed_ast, StatementExt, VecStatementExt};

use shared::{
    type_checker::{ast::Typed, EnumMember, EnumMemberField, Type},
    types::TypeIdentifier,
};

#[test]
fn enum_variant_can_be_used_as_a_type() {
    // Arrange
    let input = r#"
        enum Option {
            Some(x: int),
            None
        }

        let x: Option::Some; 
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
            enum_name: TypeIdentifier::Type("Option".to_owned()),
            discriminant_name: "Some".to_owned(),
            fields: HashMap::from([(
                "x".to_owned(),
                Type::EnumMemberField(EnumMemberField {
                    enum_name: TypeIdentifier::Type("Option".to_owned()),
                    discriminant_name: "Some".to_owned(),
                    field_name: "x".to_owned(),
                    field_type: Box::new(Type::Int)
                })
            )])
        })
    )
}
