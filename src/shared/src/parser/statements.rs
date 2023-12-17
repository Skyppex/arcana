use crate::lexer::token::{TokenKind, Keyword, Token};

use super::{Statement, cursor::Cursor, expressions::parse_expression};



pub fn parse_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    let statement = parse_function_declaration_statement(cursor);

    if let TokenKind::Semicolon = cursor.first().kind {
        cursor.bump();
    }

    statement
}

fn parse_function_declaration_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    let mut access = None;

    if cursor.first().kind == TokenKind::Keyword(Keyword::Public) || cursor.first().kind == TokenKind::Keyword(Keyword::Internal) {
        if cursor.second().kind != TokenKind::Keyword(Keyword::Fn) {
            return parse_variable_declaration_statement(cursor);
        }

        cursor.bump();
        access = Some(cursor.first().kind.clone());
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Fn) {
        return parse_variable_declaration_statement(cursor);
    }

    cursor.bump();
    todo!()
}

fn parse_variable_declaration_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    todo!()
}

fn expect(kind: TokenKind, cursor: &mut Cursor) -> Result<Token, String> {
    if cursor.first().kind == kind {
        Ok(cursor.bump().expect(format!("Expected {:?} but found {:?}", kind, cursor.first().kind).as_str()))
    } else {
        Err(format!("Expected {:?} but found {:?}", kind, cursor.first().kind))
    }
}