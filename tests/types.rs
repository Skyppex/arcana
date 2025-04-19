mod common;
use common::{create_typed_ast, StatementExt, VecStatementExt};
use shared::type_checker::{ast::Typed, LiteralType, Type};

#[test]
fn literal_type_annotation_works() {
    // Arrange
    let input = r#"
        let x: #Int;
        x
        "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(1)
        .unwrap_expression();

    assert_eq!(
        expression.get_type(),
        Type::Literal {
            name: "Int".to_string(),
            type_: Box::new(LiteralType::Int)
        }
    );
}

#[test]
fn value_literal_type_annotation_works() {
    // Arrange
    let input = r#"
        let x: #100;
        x
        "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(1)
        .unwrap_expression();

    assert_eq!(
        expression.get_type(),
        Type::Literal {
            name: "100".to_string(),
            type_: Box::new(LiteralType::IntValue(100))
        }
    );
}

#[test]
fn variable_with_literal_type_can_be_assigned_a_literal() {
    // Arrange
    let input = r#"
        let x: #Int;
        x = 100;
        "#;

    // Act
    create_typed_ast(input); // should not panic
}

#[test]
#[should_panic(expected = "Member type #Int does not match initializer type Int")]
fn variable_with_literal_type_cannot_be_assigned_a_value_with_normal_type() {
    // Arrange
    let input = r#"
        let x: Int = 100;
        let y: #Int;
        y = x;
        "#;

    // Act
    create_typed_ast(input); // should panic due to an unwrap made on a Result::Err
}

#[test]
fn variable_with_value_literal_type_can_be_assigned_a_value() {
    // Arrange
    let input = r#"
        let x: #3;
        x = 3;
        "#;

    // Act
    create_typed_ast(input); // should not panic
}

#[test]
#[should_panic(expected = "Member type #3 does not match initializer type Int")]
fn variable_with_value_literal_type_cannot_be_assigned_a_normal_type() {
    // Arrange
    let input = r#"
        let x: #3;
        let y: Int = 2 + 3;
        x = y;
        "#;

    // Act
    create_typed_ast(input); // should not panic
}

#[test]
#[should_panic(expected = "Member type #3 does not match initializer type #Int")]
fn variable_with_value_literal_type_cannot_be_assigned_a_literal_type() {
    // Arrange
    let input = r#"
        let x: #3;
        let y: #Int = 100;
        x = y;
        "#;

    // Act
    create_typed_ast(input); // should not panic
}

#[test]
#[should_panic(expected = "Member type #3 does not match initializer type #100")]
fn variable_with_value_literal_type_cannot_be_assigned_a_different_value_literal_type() {
    // Arrange
    let input = r#"
        let x: #3;
        let y: #100 = 100;
        x = y;
        "#;

    // Act
    create_typed_ast(input); // should not panic
}
