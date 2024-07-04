mod common;

use common::{
    create_env, create_type_env, create_typed_ast, evaluate_expression, StatementExt,
    VecStatementExt,
};

use interpreter::Value;
use shared::type_checker::{
    ast::{Literal, Typed, TypedExpression},
    Type,
};

#[test]
fn variable_declaration_is_immutable() {
    // Arrange
    let input = "let x: bool;";

    // Act
    let typed_ast = create_typed_ast(input, create_type_env());

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_semi()
        .unwrap_expression();

    match expression {
        TypedExpression::VariableDeclaration { mutable, .. } => {
            assert!(!mutable);
        }
        _ => panic!(
            "Expected a variable declaration, but found {:?}",
            expression
        ),
    }
}

#[test]
fn variable_declaration_is_mutable() {
    // Arrange
    let input = "let mut x: bool;";

    // Act
    let typed_ast = create_typed_ast(input, create_type_env());

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_semi()
        .unwrap_expression();

    match expression {
        TypedExpression::VariableDeclaration { mutable, .. } => {
            assert!(mutable);
        }
        _ => panic!(
            "Expected a variable declaration, but found {:?}",
            expression
        ),
    }
}

#[test]
fn variable_declaration_has_correct_identifier() {
    // Arrange
    let input = "let x: bool;";

    // Act
    let typed_ast = create_typed_ast(input, create_type_env());

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_semi()
        .unwrap_expression();

    match expression {
        TypedExpression::VariableDeclaration { identifier, .. } => {
            assert_eq!(identifier, "x");
        }
        _ => panic!(
            "Expected a variable declaration, but found {:?}",
            expression
        ),
    }
}

#[test]
fn variable_declaration_has_correct_type() {
    // Arrange
    let input = "let x: bool;";

    // Act
    let typed_ast = create_typed_ast(input, create_type_env());

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_semi()
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Bool);
}

#[test]
fn variable_declaration_has_no_initializer() {
    // Arrange
    let input = "let x: bool;";

    // Act
    let typed_ast = create_typed_ast(input, create_type_env());

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_semi()
        .unwrap_expression();

    match expression {
        TypedExpression::VariableDeclaration { initializer, .. } => {
            assert_eq!(initializer, None);
        }
        _ => panic!(
            "Expected a variable declaration, but found {:?}",
            expression
        ),
    }
}

#[test]
fn variable_declaration_has_value() {
    // Arrange
    let input = "let x: bool = true;";

    // Act
    let typed_ast = create_typed_ast(input, create_type_env());

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_semi()
        .unwrap_expression();

    match expression {
        TypedExpression::VariableDeclaration { initializer, .. } => {
            assert!(initializer.is_some());

            let initializer = *initializer.unwrap();
            assert_eq!(initializer, TypedExpression::Literal(Literal::Bool(true)));
        }
        _ => panic!(
            "Expected a variable declaration, but found {:?}",
            expression
        ),
    }
}

#[test]
fn variable_declaration_adds_variable_to_environment() {
    // Arrange
    let input = "let x: bool;";
    let environment = create_env();

    // Act
    let value = evaluate_expression(input, environment.clone(), false);

    // Assert
    assert!(environment.borrow().get_variable("x").is_some());
    assert_eq!(value, Value::Void);
}

#[test]
fn variable_declaration_adds_variable_to_environment_with_value() {
    // Arrange
    let input = "let x: bool = true";
    let environment = create_env();

    // Act
    let value = evaluate_expression(input, environment.clone(), false);

    // Assert
    assert!(environment.borrow().get_variable("x").is_some());
    assert_eq!(value, Value::Bool(true));
}

#[test]
fn variable_type_is_inferred() {
    // Arrange
    let input = "let x = true;";

    // Act
    let type_environment = create_type_env();
    create_typed_ast(input, type_environment.clone());

    // Assert
    let variable_type = type_environment.borrow().get_variable("x").unwrap();
    assert_eq!(variable_type, Type::Bool);
}

#[test]
fn variable_type_inference_is_defered() {
    // Arrange
    let input = r#"
        let x;
        x = true;
    "#;

    // Act
    let type_environment = create_type_env();
    create_typed_ast(input, type_environment.clone());

    // Assert
    let variable_type = type_environment.borrow().get_variable("x").unwrap();
    assert_eq!(variable_type, Type::Bool);
}
