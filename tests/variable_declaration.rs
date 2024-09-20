mod common;

use common::{create_env, create_typed_ast, evaluate_expression, StatementExt, VecStatementExt};

use interpreter::Value;
use shared::type_checker::{
    ast::{Literal, Typed, TypedExpression},
    decision_tree::Pattern,
    Type,
};

#[test]
fn variable_declaration_is_immutable() {
    // Arrange
    let input = "let x: Bool;";

    // Act
    let typed_ast = create_typed_ast(input);

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
    let input = "let mut x: Bool;";

    // Act
    let typed_ast = create_typed_ast(input);

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
    let input = "let x: Bool;";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_semi()
        .unwrap_expression();

    match expression {
        TypedExpression::VariableDeclaration { pattern, .. } => {
            assert_eq!(pattern, Pattern::Variable("x".to_owned()));
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
    let input = "let x: Bool;";

    // Act
    let typed_ast = create_typed_ast(input);

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
    let input = "let x: Bool;";

    // Act
    let typed_ast = create_typed_ast(input);

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
    let input = "let x: Bool = true;";

    // Act
    let typed_ast = create_typed_ast(input);

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
    let input = "let x: Bool;";
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
fn variable_declaration_type_is_inferred() {
    // Arrange
    let input = "let x = true";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(
        expression.get_type(),
        Type::Literal {
            name: "true".to_owned(),
            type_: Box::new(Type::Bool)
        }
    );
}

#[test]
fn variable_declaration_type_is_deferred() {
    // Arrange
    let input = r#"
        let x;
        x = true
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
            name: "true".to_owned(),
            type_: Box::new(Type::Bool)
        }
    );
}

#[test]
fn variable_declaration_function_type_is_used_to_infer_closure_parameter_types() {
    // Arrange
    let input = r#"
        let f: fun(Int, Float, String, UInt): Int = |i, f, s, u| i
    "#;

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(
        expression.get_type(),
        Type::Function(shared::type_checker::Function {
            identifier: None,
            param: Some(shared::type_checker::Parameter {
                identifier: "Int".to_string(),
                type_: Box::new(Type::Int)
            }),
            return_type: Box::new(Type::Function(shared::type_checker::Function {
                identifier: None,
                param: Some(shared::type_checker::Parameter {
                    identifier: "Float".to_string(),
                    type_: Box::new(Type::Float)
                }),
                return_type: Box::new(Type::Function(shared::type_checker::Function {
                    identifier: None,
                    param: Some(shared::type_checker::Parameter {
                        identifier: "String".to_string(),
                        type_: Box::new(Type::String)
                    }),
                    return_type: Box::new(Type::Function(shared::type_checker::Function {
                        identifier: None,
                        param: Some(shared::type_checker::Parameter {
                            identifier: "UInt".to_string(),
                            type_: Box::new(Type::UInt)
                        }),
                        return_type: Box::new(Type::Int)
                    }))
                }))
            }))
        })
    );
}
