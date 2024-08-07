pub(crate) mod ast;
pub mod cursor;
mod expressions;
mod statements;

pub use ast::*;

use crate::lexer::token::Token;

use self::cursor::Cursor;

pub fn create_ast(tokens: Vec<Token>, verbose: bool) -> Result<Statement, String> {
    let mut cursor = Cursor::new(tokens, verbose);

    let statements = statements::parse_file(&mut cursor)?;

    Ok(Statement::Program { statements })
}

pub fn discover_module(
    tokens: Vec<Token>,
) -> Result<Option<(Option<AccessModifier>, Vec<String>)>, String> {
    let mut cursor = Cursor::new(tokens, false);
    let module = statements::parse_module_only(&mut cursor)?;
    Ok(module)
}
