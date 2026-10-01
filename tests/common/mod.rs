#![allow(dead_code, unused)]
use std::{cell::RefCell, rc::Rc};

use interpreter::{Environment, Value};
use shared::{
    ast,
    diagnostic::{Diagnostic, SourceFile},
    lexer,
    type_checker::{
        self,
        model::{TypedExpression, TypedStatement},
    },
};

pub fn tokenize(input: &str) -> Vec<lexer::token::Token> {
    lexer::tokenize(input).unwrap()
}

/// A type environment with the core library loaded, as the compiler builds one.
///
/// The checker resolves `Option` from the library rather than synthesising it,
/// so anything that type checks an `if` without an else needs the library
/// present — tests included.
pub fn core_type_environment() -> Rcrc<type_checker::TypeEnvironment> {
    let type_environment = Rc::new(RefCell::new(type_checker::TypeEnvironment::new(false)));

    let core = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("core/lib.ar"),
    )
    .expect("failed to read core/lib.ar");

    let tokens = lexer::tokenize(&core).expect("failed to tokenize the core library");

    type_checker::register_core(tokens, type_environment.clone())
        .expect("failed to register the core library");

    type_environment
}

pub fn create_typed_ast(input: &str) -> TypedStatement {
    let tokens = lexer::tokenize(input).unwrap();
    let ast = ast::create_ast(tokens, false).unwrap();
    let type_environment = core_type_environment();

    type_checker::create_typed_ast(ast, type_environment).unwrap()
}

/// Like [`create_typed_ast`], but with the simplification pass applied, for
/// asserting what that pass did to the tree.
pub fn create_simplified_ast(input: &str) -> TypedStatement {
    type_checker::simplify(create_typed_ast(input))
}

/// Like [`create_typed_ast`], but surfaces lex/parse/type errors instead of panicking.
/// Use this to assert that a program is *rejected*.
pub fn try_create_typed_ast(input: &str) -> Result<TypedStatement, Diagnostic> {
    let tokens = lexer::tokenize(input)?;
    let ast = ast::create_ast(tokens, false)?;
    let type_environment = core_type_environment();

    type_checker::create_typed_ast(ast, type_environment)
}

/// The name every diagnostic test compiles under, so expected output can name a
/// file without each test repeating it.
pub const TEST_FILE: &str = "t.ar";

/// Compiles `input` and renders the error it produces, exactly as the driver
/// would print it.
///
/// Panics if the program compiles: a test that expects a diagnostic and gets a
/// working program has not found what it was looking for.
pub fn render_error(input: &str) -> String {
    let Err(error) = try_create_typed_ast(input) else {
        panic!("expected the program to be rejected, but it compiled");
    };

    error.render(&SourceFile::new(TEST_FILE, input))
}

pub fn evaluate_expression(
    input: &str,
    environment: Rcrc<Environment>,
    unwrap_semi: bool,
) -> Value {
    let tokens = lexer::tokenize(input).unwrap();
    let ast = ast::create_ast(tokens, false).unwrap();
    let type_environment = core_type_environment();
    let typed_ast = type_checker::simplify(type_checker::create_typed_ast(ast, type_environment).unwrap());

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
