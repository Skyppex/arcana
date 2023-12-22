use token::{Token, TokenKind, Literal};
use cursor::Cursor;

use crate::parser::AccessModifier;

use self::{num_lit::parse_numeric_literal, token::Keyword};

pub mod token;
mod cursor;
mod num_lit;

pub fn tokenize(source_code: String) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut cursor = cursor::Cursor::new(source_code.as_str());

    while !cursor.is_end_of_file() {
        let token = tokenize_next(&mut cursor);
        tokens.push(token);
    }

    transpose(tokens)
}

fn tokenize_next(cursor: &mut Cursor) -> Result<Token, String> {
    cursor.reset_position_within_token();

    let token = match cursor.first() {
        ' ' | '\r' | '\t' | '\n' => {
            cursor.bump();
            cursor.eat_while(|c| is_white_space(c));

            Ok(Token {
                kind: TokenKind::WhiteSpace,
                length: cursor.position_within_token(),
            })
        }
        '(' => Ok(create_token(TokenKind::OpenParen, cursor)),
        ')' => Ok(create_token(TokenKind::CloseParen, cursor)),
        '{' => Ok(create_token(TokenKind::OpenBrace, cursor)),
        '}' => Ok(create_token(TokenKind::CloseBrace, cursor)),
        '[' => Ok(create_token(TokenKind::OpenBracket, cursor)),
        ']' => Ok(create_token(TokenKind::CloseBracket, cursor)),
        ',' => Ok(create_token(TokenKind::Comma, cursor)),
        ':' => Ok(create_token(TokenKind::Colon, cursor)),
        ';' => Ok(create_token(TokenKind::Semicolon, cursor)),
        '.' => Ok(create_token(TokenKind::Dot, cursor)),
        '?' => Ok(create_token(TokenKind::QuestionMark, cursor)),
        '+' => {
            if cursor.second() == '=' {
                cursor.bump();
                Ok(create_token(TokenKind::PlusEqual, cursor))
            } else {
                Ok(create_token(TokenKind::Plus, cursor))
            }
        }
        '-' => {
            if cursor.second() == '=' {
                cursor.bump();
                Ok(create_token(TokenKind::MinusEqual, cursor))
            } else {
                Ok(create_token(TokenKind::Minus, cursor))
            }
        }
        '*' => {
            if cursor.second() == '=' {
                cursor.bump();
                Ok(create_token(TokenKind::StarEqual, cursor))
            } else {
                Ok(create_token(TokenKind::Star, cursor))
            }
        }
        '/' => {
            if cursor.second() == '/' {
                cursor.eat_while(|c| !is_end_of_line_comment(c));
                Ok(Token {
                    kind: TokenKind::LineComment,
                    length: cursor.position_within_token(),
                })
            } else if cursor.second() == '=' {
                cursor.bump();
                Ok(create_token(TokenKind::SlashEqual, cursor))
            } else {
                Ok(create_token(TokenKind::Slash, cursor))
            }
        }
        '%' => {
            if cursor.second() == '=' {
                cursor.bump();
                Ok(create_token(TokenKind::PercentEqual, cursor))
            } else {
                Ok(create_token(TokenKind::Percent, cursor))
            }
        }
        '&' => {
            if cursor.second() == '&' {
                cursor.bump();
                Ok(create_token(TokenKind::DoubleAmpersand, cursor))
            } else if cursor.second() == '=' {
                cursor.bump();
                Ok(create_token(TokenKind::AmpersandEqual, cursor))
            } else {
                Ok(create_token(TokenKind::Ampersand, cursor))
            }
        }
        '|' => {
            if cursor.second() == '|' {
                cursor.bump();
                Ok(create_token(TokenKind::DoublePipe, cursor))
            } else if cursor.second() == '=' {
                cursor.bump();
                Ok(create_token(TokenKind::PipeEqual, cursor))
            
            } else {
                Ok(create_token(TokenKind::Pipe, cursor))
            }
        }
        '^' => {
            if cursor.second() == '=' {
                cursor.bump();
                Ok(create_token(TokenKind::CaretEqual, cursor))
            } else {
                Ok(create_token(TokenKind::Caret, cursor))
            }
        }
        '~' => {
            Ok(create_token(TokenKind::Tilde, cursor))
        }
        '=' => {
            if cursor.second() == '=' {
                cursor.bump();
                Ok(create_token(TokenKind::DoubleEqual, cursor))
            } else {
                Ok(create_token(TokenKind::Equal, cursor))
            }
        }
        '!' => {
            if cursor.second() == '=' {
                cursor.bump();
                Ok(create_token(TokenKind::BangEqual, cursor))
            } else {
                Ok(create_token(TokenKind::Bang, cursor))
            }
        }
        '<' => {
            if cursor.second() == '=' {
                cursor.bump();
                Ok(create_token(TokenKind::LessEqual, cursor))
            } else if cursor.second() == '<' {
                cursor.bump();
                Ok(create_token(TokenKind::DoubleLess, cursor))
            } else {
                Ok(create_token(TokenKind::Less, cursor))
            }
        }
        '>' => {
            if cursor.second() == '=' {
                cursor.bump();
                Ok(create_token(TokenKind::GreaterEqual, cursor))
            } else if cursor.second() == '>' {
                cursor.bump();
                Ok(create_token(TokenKind::DoubleGreater, cursor))
            } else {
                Ok(create_token(TokenKind::Greater, cursor))
            }
        }
        '0'..='9' => {
            Ok(parse_numeric_literal(cursor))
        }
        '"' => {
            cursor.bump();
            let mut string = String::new();

            while cursor.first() != '"' {
                if cursor.first() == '\\' {
                    match escapable_is_string(cursor.second()) {
                        Some(c) => {
                            string.push(c);
                            cursor.bump();
                            cursor.bump();
                        }
                        None => {
                            string.push(cursor.bump().unwrap());
                        }
                    }

                    continue;
                }

                string.push(cursor.bump().unwrap());
            }

            cursor.bump();
            Ok(Token {
                kind: TokenKind::Literal(Literal::String(string)),
                length: cursor.position_within_token(),
            })
        }
        '\'' => {
            cursor.bump();
            let mut string = String::new();

            if cursor.first() == '\\' {
                match escapable_is_char(cursor.second()) {
                    Some(c) => {
                        string.push(c);
                        cursor.bump();
                        cursor.bump();
                    }
                    None => {
                        string.push(cursor.bump().unwrap());
                    }
                }
            } else {
                string.push(cursor.bump().unwrap());
            }

            cursor.bump();
            Ok(Token {
                kind: TokenKind::Literal(Literal::Char(string)),
                length: cursor.position_within_token(),
            })
        }
        '\0' => Ok(Token {
            kind: TokenKind::EndOfFile,
            length: cursor.position_within_token(),
        }),
        c => {
            if is_identifier_start(c) {
                let mut string = String::new();

                while is_identifier_continue(cursor.first()) {
                    string.push(cursor.bump().unwrap());
                }

                if let Some(keyword) = get_reserved_keyword(&string) {
                    return Ok(Token {
                        kind: keyword,
                        length: cursor.position_within_token(),
                    })
                }

                return Ok(Token {
                    kind: TokenKind::Identifier(string),
                    length: cursor.position_within_token(),
                })
            }

            Err(format!("Unrecognized character: {0}", cursor.first()))
        }
    };

    token
}

fn is_identifier_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_identifier_continue(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn is_end_of_line_comment(c: char) -> bool {
    c == '\n' || c == '\r' || c == '\0'
}

fn is_white_space(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\r' || c == '\n'
}

fn always_escapable(c: char) -> Option<char> {
    match c {
        'a' => Some('a'),
        'b' => Some('b'),
        'f' => Some('f'),
        'n' => Some('n'),
        'r' => Some('r'),
        't' => Some('t'),
        'v' => Some('v'),
        '0' => Some('0'),
        '\\' => Some('\\'),
        _ => None,
    }
}

fn escapable_is_string(c: char) -> Option<char> {
    always_escapable(c).or(match c {
        '"' => Some('"'),
        _ => None,
    })
}

fn escapable_is_char(c: char) -> Option<char> {
    always_escapable(c).or(match c {
        '\'' => Some('\''),
        _ => None,
    })
}

fn get_reserved_keyword(string: &str) -> Option<TokenKind> {
    match string {
        "mutable" => Some(TokenKind::Keyword(Keyword::Mutable)),
        "if" => Some(TokenKind::Keyword(Keyword::If)),
        "else" => Some(TokenKind::Keyword(Keyword::Else)),
        // "while" => Some(TokenKind::Keyword(Keyword::While)),
        // "for" => Some(TokenKind::Keyword(Keyword::For)),
        // "match" => Some(TokenKind::Keyword(Keyword::Match)),
        // "return" => Some(TokenKind::Keyword(Keyword::Return)),
        // "break" => Some(TokenKind::Keyword(Keyword::Break)),
        // "continue" => Some(TokenKind::Keyword(Keyword::Continue)),
        "fn" => Some(TokenKind::Keyword(Keyword::Fn)),
        "struct" => Some(TokenKind::Keyword(Keyword::Struct)),
        "union" => Some(TokenKind::Keyword(Keyword::Union)),
        "public" => Some(TokenKind::Keyword(Keyword::AccessModifier(AccessModifier::Public))),
        "internal" => Some(TokenKind::Keyword(Keyword::AccessModifier(AccessModifier::Internal))),
        "true" => Some(TokenKind::Literal(Literal::Bool(true))),
        "false" => Some(TokenKind::Literal(Literal::Bool(false))),
        _ => None,
    }
}

fn create_token(kind: TokenKind, cursor: &mut Cursor<'_>) -> Token {
    cursor.bump();
    Token {
        kind,
        length: cursor.position_within_token(),
    }
}

fn transpose(tokens: Vec<Result<Token, String>>) -> Result<Vec<Token>, String> {
    let mut tks = Vec::new();

    for token in tokens {
        match token {
            Ok(token) => tks.push(token),
            Err(err) => return Err(err),
        }
    }

    Ok(tks)
}