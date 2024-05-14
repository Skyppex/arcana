use std::{cell::RefCell, rc::Rc};

use shared::{lexer, parser, type_checker::{self, ast::{Literal, TypedExpression, TypedStatement}, Type}};
use interpreter::{Environment, Value};

#[test]
fn variable_declaration_is_immutable() {
    // Arrange
    let input = "let x: bool;";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast.unwrap_program()
        .nth_statement(0)
        .unwrap_semi()
        .unwrap_expression();
    
    match expression {
        TypedExpression::VariableDeclaration { mutable, .. } => {
            assert_eq!(mutable, false);
        },
        _ => panic!("Expected a variable declaration, but found {:?}", expression),
    }
}

#[test]
fn variable_declaration_is_mutable() {
    // Arrange
    let input = "let mut x: bool;";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast.unwrap_program()
        .nth_statement(0)
        .unwrap_semi()
        .unwrap_expression();
        
    match expression {
        TypedExpression::VariableDeclaration { mutable, .. } => {
            assert_eq!(mutable, true);
        },
        _ => panic!("Expected a variable declaration, but found {:?}", expression),
    }
}

#[test]
fn variable_declaration_has_correct_identifier() {
    // Arrange
    let input = "let x: bool;";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast.unwrap_program()
        .nth_statement(0)
        .unwrap_semi()
        .unwrap_expression();
        
    match expression {
        TypedExpression::VariableDeclaration { identifier, .. } => {
            assert_eq!(identifier, "x");
        },
        _ => panic!("Expected a variable declaration, but found {:?}", expression),
    }
}

#[test]
fn variable_declaration_has_correct_type() {
    // Arrange
    let input = "let x: bool;";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast.unwrap_program()
        .nth_statement(0)
        .unwrap_semi()
        .unwrap_expression();
        
    match expression {
        TypedExpression::VariableDeclaration { type_, .. } => {
            assert_eq!(type_, Type::Bool);
        },
        _ => panic!("Expected a variable declaration, but found {:?}", expression),
    }
}

#[test]
fn variable_declaration_has_no_initializer() {
    // Arrange
    let input = "let x: bool;";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast.unwrap_program()
        .nth_statement(0)
        .unwrap_semi()
        .unwrap_expression();
        
    match expression {
        TypedExpression::VariableDeclaration { initializer, .. } => {
            assert_eq!(initializer, None);
        },
        _ => panic!("Expected a variable declaration, but found {:?}", expression),
    }
}

#[test]
fn variable_declaration_has_value() {
    // Arrange
    let input = "let x: bool = true;";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast.unwrap_program()
        .nth_statement(0)
        .unwrap_semi()
        .unwrap_expression();
    
    match expression {
        TypedExpression::VariableDeclaration { initializer, .. } => {
            assert!(matches!(initializer, Some(_)));

            let initializer = *initializer.unwrap();
            assert_eq!(initializer, TypedExpression::Literal(Literal::Bool(true)));
        },
        _ => panic!("Expected a variable declaration, but found {:?}", expression),
    }
}

#[test]
fn variable_declaration_adds_variable_to_environment() {
    // Arrange
    let input = "let x: bool;";
    let environment = create_rcrc(Environment::new());

    // Act
    let value = evaluate_expression(input, environment.clone());

    // Assert
    assert!(environment.borrow().get_variable("x").is_some());
    assert_eq!(value, Value::Uninitialized);
}

#[test]
fn variable_declaration_adds_variable_to_environment_with_value() {
    // Arrange
    let input = "let x: bool = true;";
    let environment = create_rcrc(Environment::new());

    // Act
    let value = evaluate_expression(input, environment.clone());

    // Assert
    assert!(environment.borrow().get_variable("x").is_some());
    assert_eq!(value, Value::Bool(true));
}

fn create_typed_ast(input: &str) -> TypedStatement {
    let tokens = lexer::tokenize(input).unwrap();
    let ast = parser::create_ast(tokens).unwrap();
    let type_environment = Rc::new(RefCell::new(type_checker::TypeEnvironment::new()));
    let typed_ast = type_checker::create_typed_ast(ast, type_environment).unwrap();
    typed_ast
}

fn evaluate_expression(input: &str, environment: Rcrc<Environment>) -> Value {
    let tokens = lexer::tokenize(input).unwrap();
    let ast = parser::create_ast(tokens).unwrap();
    let type_environment = Rc::new(RefCell::new(type_checker::TypeEnvironment::new()));
    let typed_ast = type_checker::create_typed_ast(ast, type_environment).unwrap();

    let expression = typed_ast.unwrap_program().nth_statement(0).unwrap_semi();

    interpreter::evaluate(expression, environment).unwrap()
}

trait StatementExt {
    fn unwrap_program(self) -> Vec<TypedStatement>;
    fn unwrap_semi(self) -> TypedStatement;
    fn unwrap_expression(self) -> TypedExpression;
}

trait VecStatementExt {
    fn nth_statement(self, n: usize) -> TypedStatement;
}

impl StatementExt for TypedStatement {
    fn unwrap_program(self) -> Vec<TypedStatement> {
        match self {
            TypedStatement::Program { statements } => statements,
            _ => panic!("Expected a program"),
        }
    }
    
    fn unwrap_semi(self) -> TypedStatement {
        match self {
            TypedStatement::Semi(expression) => *expression,
            _ => panic!("Expected a semi"),
        }
    }

    fn unwrap_expression(self) -> TypedExpression {
        match self {
            TypedStatement::Expression(expression) => expression,
            _ => panic!("Expected an expression"),
        }
    }
}

impl VecStatementExt for Vec<TypedStatement> {
    fn nth_statement(self, n: usize) -> TypedStatement {
        self.get(n).unwrap().clone()
    }
}

type Rcrc<T> = Rc<RefCell<T>>;

fn create_rcrc<T>(value: T) -> Rcrc<T> {
    Rc::new(RefCell::new(value))
}
