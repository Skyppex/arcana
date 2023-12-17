mod ast;
mod cursor;
mod statements;
mod expressions;

use std::vec;

pub use ast::*;

use crate::lexer::token::Token;

use self::cursor::Cursor;

pub fn create_ast(tokens: Vec<Token>) -> Statement {
    let mut cursor = Cursor::new(tokens);
    let mut statements = vec![];

    while !cursor.is_end_of_file() {
        statements.push(statements::parse_statement(&mut cursor));
    }

    Statement::Program { statements }
}