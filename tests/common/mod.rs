#![allow(dead_code, unused)]
use std::{cell::RefCell, rc::Rc};

use interpreter::{Environment, Value};
use shared::{
    lexer, parser,
    type_checker::{
        self,
        ast::{TypedExpression, TypedStatement},
    },
};

pub fn tokenize(input: &str) -> Vec<lexer::token::Token> {
    lexer::tokenize(input).unwrap()
}

pub fn create_typed_ast(input: &str) -> TypedStatement {
    let tokens = lexer::tokenize(input).unwrap();
    let ast = parser::create_ast(tokens).unwrap();
    let type_environment = Rc::new(RefCell::new(type_checker::TypeEnvironment::new()));

    type_checker::create_typed_ast(ast, type_environment).unwrap()
}

pub fn evaluate_expression(
    input: &str,
    environment: Rcrc<Environment>,
    unwrap_semi: bool,
) -> Value {
    let tokens = lexer::tokenize(input).unwrap();
    let ast = parser::create_ast(tokens).unwrap();
    let type_environment = Rc::new(RefCell::new(type_checker::TypeEnvironment::new()));
    let typed_ast = type_checker::create_typed_ast(ast, type_environment).unwrap();

    if unwrap_semi {
        interpreter::evaluate(typed_ast.unwrap_semi(), environment).unwrap()
    } else {
        interpreter::evaluate(typed_ast, environment).unwrap()
    }
}

pub trait TokenExt {
    fn nth_token(&self, n: usize) -> lexer::token::Token;
}

pub trait StatementExt {
    fn unwrap_program(self) -> Vec<TypedStatement>;
    fn unwrap_semi(self) -> TypedStatement;
    fn unwrap_expression(self) -> TypedExpression;
}

pub trait VecStatementExt {
    fn nth_statement(self, n: usize) -> TypedStatement;
}

impl TokenExt for Vec<lexer::token::Token> {
    fn nth_token(&self, n: usize) -> lexer::token::Token {
        self.get(n).unwrap().clone()
    }
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

pub type Rcrc<T> = Rc<RefCell<T>>;

pub fn create_env() -> Rcrc<Environment> {
    Rc::new(RefCell::new(Environment::new()))
}
