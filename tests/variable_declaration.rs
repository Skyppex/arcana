mod common;

use shared::ast::pattern::Pattern;

use common::{
    create_env, create_typed_ast, evaluate_expression, try_create_typed_ast, StatementExt,
    VecStatementExt,
};

use interpreter::Value;
use shared::type_checker::{
    model::{Typed, TypedExpression, ValueLiteral},
    LiteralType, Type,
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
        _ => panic!("Expected a variable declaration, but found {expression:?}"),
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
        _ => panic!("Expected a variable declaration, but found {expression:?}"),
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
            assert_eq!(pattern, Pattern::Binding("x".to_owned()));
        }
        _ => panic!("Expected a variable declaration, but found {expression:?}"),
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
        _ => panic!("Expected a variable declaration, but found {expression:?}"),
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
            assert_eq!(
                initializer,
                TypedExpression::Literal {
                    literal: ValueLiteral::Bool(true),
                    type_: Type::Literal {
                        name: "true".to_string(),
                        type_: Box::new(LiteralType::BoolValue(true))
                    }
                }
            );
        }
        _ => panic!("Expected a variable declaration, but found {expression:?}"),
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
    let input = "let x: Bool = true";
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
    // The declaration itself evaluates to whether the name could be bound, so
    // the inferred type is observed on the variable rather than on the
    // declaration.
    let input = r#"
        let x = true;
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
            name: "true".to_owned(),
            type_: Box::new(LiteralType::BoolValue(true))
        }
    );
}

#[test]
fn variable_declaration_evaluates_to_whether_it_bound() {
    // Arrange
    let input = "let x = true";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Bool);
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
            type_: Box::new(LiteralType::BoolValue(true))
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

    // The parameter types land on the closure, so the assertion is on the
    // initializer rather than on the declaration.
    let TypedExpression::VariableDeclaration {
        initializer: Some(initializer),
        ..
    } = expression
    else {
        panic!("Expected a variable declaration with an initializer");
    };

    assert_eq!(
        initializer.get_type(),
        Type::Function(shared::type_checker::Function {
            identifier: None,
            param: Some(shared::type_checker::Parameter {
                // The closure names this one; the nested ones fall back to the
                // annotation, which has only type names to go on.
                identifier: "i".to_string(),
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

// ---------------------------------------------------------------------------
// Destructuring declarations. A declaration binds unconditionally, so only
// irrefutable patterns are allowed here.
// ---------------------------------------------------------------------------

#[test]
fn variable_declaration_destructures_a_struct() {
    // Arrange
    let input = r#"
        struct Point { x: Int, y: Int }
        let p: Point = Point { x: 1, y: 2 };
        let { x, y } = p;
        x + y
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(interpreter::value::Number::Int(3)));
}

#[test]
fn variable_declaration_destructures_a_subset_of_fields() {
    // Arrange
    let input = r#"
        struct Point { x: Int, y: Int }
        let p: Point = Point { x: 1, y: 2 };
        let { y } = p;
        y
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(interpreter::value::Number::Int(2)));
}

#[test]
fn variable_declaration_destructures_a_named_struct() {
    // Arrange
    let input = r#"
        struct Point { x: Int }
        let p: Point = Point { x: 1 };
        let Point { x } = p;
        x
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(interpreter::value::Number::Int(1)));
}

#[test]
fn variable_declaration_destructures_a_tuple() {
    // Arrange
    let input = r#"
        let t: (Int, Int) = (1, 2);
        let (x, y) = t;
        x + y
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(interpreter::value::Number::Int(3)));
}

#[test]
fn variable_declaration_tuple_elements_may_be_wildcards() {
    // Arrange
    let input = r#"
        let t: (Int, Int) = (1, 2);
        let (x, _) = t;
        x
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(interpreter::value::Number::Int(1)));
}

#[test]
fn variable_declaration_destructures_nested_patterns() {
    // Arrange
    let input = r#"
        struct Inner { v: Int }
        struct Outer { inner: Inner }
        let o: Outer = Outer { inner: Inner { v: 5 } };
        let { inner: { v } } = o;
        v
    "#;

    // Act
    let result = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(result, Value::Number(interpreter::value::Number::Int(5)));
}

#[test]
fn variable_declaration_rejects_a_refutable_field_pattern() {
    // Arrange
    let input = r#"
        struct Point { x: Int }
        let p: Point = Point { x: 1 };
        let { x: 1 } = p;
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

#[test]
fn variable_declaration_rejects_a_refutable_tuple_pattern() {
    // Arrange
    let input = r#"
        let t: (Int, Int) = (1, 2);
        let (1, y) = t;
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}

#[test]
fn variable_declaration_rejects_a_mismatched_struct_name() {
    // Arrange
    let input = r#"
        struct Point { x: Int }
        struct Other { x: Int }
        let p: Point = Point { x: 1 };
        let Other { x } = p;
    "#;

    // Act & Assert
    assert!(try_create_typed_ast(input).is_err());
}
