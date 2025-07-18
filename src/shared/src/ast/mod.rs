pub mod cursor;
mod expressions;
pub(crate) mod model;
mod statements;

use expressions::{parse_block, parse_expression};
pub use model::*;

use crate::{
    ast::statements::ParseContext,
    lexer::token::{Token, TokenKind},
};

use self::cursor::Cursor;

pub fn create_ast(tokens: Vec<Token>, verbose: bool) -> Result<Statement, String> {
    let mut cursor = Cursor::new(tokens, verbose);

    let context = ParseContext::default();
    let statements = statements::parse_file(&mut cursor, &context)?;

    Ok(Statement::Program { statements })
}

pub fn discover_module(
    tokens: Vec<Token>,
) -> Result<Option<(Option<AccessModifier>, ModPath, Statement)>, String> {
    let mut cursor = Cursor::new(tokens, false);
    let module = statements::parse_module(&mut cursor)?;
    Ok(module)
}

pub fn fat_arrow_expr_or_block_expr(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Expression, String> {
    match cursor.first().kind {
        TokenKind::FatArrow => {
            cursor.bump()?; // Consume the =>
            parse_expression(cursor, context)
        }
        TokenKind::OpenBrace => parse_block(cursor, context),
        _ => Err(format!(
            "Expected => or {{ but found {:?}",
            cursor.first().kind
        )),
    }
}
