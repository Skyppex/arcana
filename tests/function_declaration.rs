use common::{create_type_env, create_typed_ast, StatementExt, VecStatementExt};
use shared::{
    type_checker::{
        ast::{Typed, TypedStatement},
        Function, Parameter, Type,
    },
    types::TypeIdentifier,
};

mod common;

#[test]
pub fn function_declaration_is_function_declaration() {
    // Arrange

    let input = "fun foo() => {}";

    // Act
    let typed_ast = create_typed_ast(input, create_type_env());

    // Assert
    let statement = typed_ast.unwrap_program().nth_statement(0);

    assert!(matches!(
        statement,
        TypedStatement::FunctionDeclaration { .. }
    ));
}

#[test]
pub fn function_declaration_has_correct_type() {
    // Arrange
    let input = "fun foo() => {}";

    // Act
    let typed_ast = create_typed_ast(input, create_type_env());

    // Assert
    let statement = typed_ast.unwrap_program().nth_statement(0);

    assert_eq!(
        statement.get_type(),
        Type::Function(Function {
            identifier: Some(TypeIdentifier::Type("foo".to_string())),
            param: None,
            return_type: Box::new(Type::Void),
        })
    );
}

#[test]
pub fn function_declaration_has_correct_type_with_param_and_return() {
    // Arrange
    let input = "fun foo(x: int): int => x";

    // Act
    let typed_ast = create_typed_ast(input, create_type_env());

    // Assert
    let statement = typed_ast.unwrap_program().nth_statement(0);

    assert_eq!(
        statement.get_type(),
        Type::Function(Function {
            identifier: Some(TypeIdentifier::Type("foo".to_string())),
            param: Some(Parameter {
                identifier: "x".to_string(),
                type_: Box::new(Type::Int),
            }),
            return_type: Box::new(Type::Int),
        })
    );
}

#[test]
pub fn function_declaration_with_two_params_has_correct_type() {
    // Arrange
    let input = "fun foo(x: int, y: int): int => x";

    // Act
    let typed_ast = create_typed_ast(input, create_type_env());

    // Assert
    let statement = typed_ast.unwrap_program().nth_statement(0);

    assert_eq!(
        statement.get_type(),
        Type::Function(Function {
            identifier: Some(TypeIdentifier::Type("foo".to_string())),
            param: Some(Parameter {
                identifier: "x".to_string(),
                type_: Box::new(Type::Int),
            }),
            return_type: Box::new(Type::Function(Function {
                identifier: None,
                param: Some(Parameter {
                    identifier: "int".to_string(),
                    type_: Box::new(Type::Int),
                }),
                return_type: Box::new(Type::Int),
            })),
        })
    );
}

// This one is added after going from only supporting two params to supporting any number of params
#[test]
pub fn function_declaration_with_multiple_params_has_correct_type() {
    // Arrange
    let input = "fun foo(x: int, y: int, z: int): int => x + y + z";

    // Act
    let typed_ast = create_typed_ast(input, create_type_env());

    // Assert
    let statement = typed_ast.unwrap_program().nth_statement(0);

    assert_eq!(
        statement.get_type(),
        Type::Function(Function {
            identifier: Some(TypeIdentifier::Type("foo".to_string())),
            param: Some(Parameter {
                identifier: "x".to_string(),
                type_: Box::new(Type::Int),
            }),
            return_type: Box::new(Type::Function(Function {
                identifier: None,
                param: Some(Parameter {
                    identifier: "int".to_string(),
                    type_: Box::new(Type::Int),
                }),
                return_type: Box::new(Type::Function(Function {
                    identifier: None,
                    param: Some(Parameter {
                        identifier: "int".to_string(),
                        type_: Box::new(Type::Int),
                    }),
                    return_type: Box::new(Type::Int),
                }))
            })),
        })
    );
}
