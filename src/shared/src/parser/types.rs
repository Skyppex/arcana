use crate::lexer::token::{self, TokenKind};

use super::cursor::Cursor;


pub(super) fn can_be_type(cursor: &Cursor) -> bool {
    let mut cloned_cursor = cursor.clone();

    match cloned_cursor.first().kind {
        TokenKind::Literal(token::Literal::Unit) => true,
        TokenKind::Identifier(_) => true,
        TokenKind::OpenBracket => {
            let _ = cloned_cursor.bump();
            can_be_type(&cloned_cursor)
        },
        _ => false,
    }
}

pub(super) fn parse_type(cursor: &mut Cursor) -> Result<String, String> {
    match cursor.first().kind {
        TokenKind::Literal(token::Literal::Unit) => {
            cursor.bump()?; // Consume the unit
            Ok("unit".to_string())
        },
        TokenKind::Identifier(type_name) => {
            cursor.bump()?; // Consume the type identifier
            Ok(type_name)
        },
        TokenKind::OpenBracket => {
            cursor.bump()?; // Consume the [

            let type_name = parse_type(cursor)?;

            if cursor.first().kind != TokenKind::CloseBracket {
                return Err(format!("Expected ] but found {:?}", cursor.first().kind));
            }

            cursor.bump()?; // Consume the ]
            Ok(format!("[{}]", type_name))
        },
        _ => Err(format!("Expected type identifier but found {:?}", cursor.first().kind)),
    }
}
