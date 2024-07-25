use cursor::Cursor;
use token::{Literal, Token, TokenKind};

use crate::parser::AccessModifier;

use self::{num_lit::parse_numeric_literal, token::Keyword};

pub mod cursor;
mod num_lit;
pub mod token;

pub fn tokenize(source_code: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut cursor = cursor::Cursor::new(source_code);

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

            tokenize_next(cursor)
            // Ok(Token {
            //     kind: TokenKind::WhiteSpace,
            //     length: cursor.position_within_token(),
            // })
        }
        '(' => Ok(create_token(TokenKind::OpenParen, cursor)),
        ')' => Ok(create_token(TokenKind::CloseParen, cursor)),
        '{' => Ok(create_token(TokenKind::OpenBrace, cursor)),
        '}' => Ok(create_token(TokenKind::CloseBrace, cursor)),
        '[' => Ok(create_token(TokenKind::OpenBracket, cursor)),
        ']' => Ok(create_token(TokenKind::CloseBracket, cursor)),
        ',' => Ok(create_token(TokenKind::Comma, cursor)),
        '_' => Ok(create_token(TokenKind::Underscore, cursor)),
        ':' => match cursor.second() {
            ':' => {
                cursor.bump();
                Ok(create_token(TokenKind::DoubleColon, cursor))
            }
            _ => Ok(create_token(TokenKind::Colon, cursor)),
        },
        ';' => Ok(create_token(TokenKind::Semicolon, cursor)),
        '.' => match cursor.second() {
            '.' => {
                cursor.bump();
                Ok(create_token(TokenKind::DoubleDot, cursor))
            }
            _ => Ok(create_token(TokenKind::Dot, cursor)),
        },
        '?' => Ok(create_token(TokenKind::QuestionMark, cursor)),
        '+' => match cursor.second() {
            '=' => {
                cursor.bump();
                Ok(create_token(TokenKind::PlusEqual, cursor))
            }
            _ => Ok(create_token(TokenKind::Plus, cursor)),
        },
        '-' => match cursor.second() {
            '=' => {
                cursor.bump();
                Ok(create_token(TokenKind::MinusEqual, cursor))
            }
            _ => Ok(create_token(TokenKind::Minus, cursor)),
        },
        '*' => match cursor.second() {
            '=' => {
                cursor.bump();
                Ok(create_token(TokenKind::StarEqual, cursor))
            }
            _ => Ok(create_token(TokenKind::Star, cursor)),
        },
        '/' => match cursor.second() {
            '/' => {
                cursor.eat_while(|c| !is_end_of_line_comment(c));
                Ok(Token {
                    kind: TokenKind::LineComment,
                    length: cursor.position_within_token(),
                })
            }
            '-' => {
                cursor.bump();
                cursor.bump();

                let mut length = 2;
                // let mut nested = 1;

                while !cursor.is_end_of_file() {
                    // if cursor.first() == '/' && cursor.second() == '-' {
                    //     cursor.bump();
                    //     cursor.bump();
                    //     nested += 1;
                    //     length += 2;
                    // } else if cursor.first() == '-' && cursor.second() == '/' {
                    //     cursor.bump();
                    //     cursor.bump();
                    //     nested -= 1;
                    //     length += 2;

                    if cursor.first() == '-' && cursor.second() == '/' {
                        cursor.bump();
                        cursor.bump();
                        length += 2;
                        break;
                    } else {
                        cursor.bump();
                        length += 1;
                    }

                    // if nested == 0 {
                    //     break;
                    // }
                }

                Ok(Token {
                    kind: TokenKind::BlockComment,
                    length,
                })
            }
            '=' => {
                cursor.bump();
                Ok(create_token(TokenKind::SlashEqual, cursor))
            }
            _ => Ok(create_token(TokenKind::Slash, cursor)),
        },
        '%' => match cursor.second() {
            '=' => {
                cursor.bump();
                Ok(create_token(TokenKind::PercentEqual, cursor))
            }
            _ => Ok(create_token(TokenKind::Percent, cursor)),
        },
        '&' => match cursor.second() {
            '&' => {
                cursor.bump();
                Ok(create_token(TokenKind::DoubleAmpersand, cursor))
            }
            '=' => {
                cursor.bump();
                Ok(create_token(TokenKind::AmpersandEqual, cursor))
            }
            _ => Ok(create_token(TokenKind::Ampersand, cursor)),
        },
        '|' => match cursor.second() {
            '=' => {
                cursor.bump();
                Ok(create_token(TokenKind::PipeEqual, cursor))
            }
            _ => Ok(create_token(TokenKind::Pipe, cursor)),
        },
        '^' => match cursor.second() {
            '=' => {
                cursor.bump();
                Ok(create_token(TokenKind::CaretEqual, cursor))
            }
            _ => Ok(create_token(TokenKind::Caret, cursor)),
        },
        '~' => Ok(create_token(TokenKind::Tilde, cursor)),
        '=' => match cursor.second() {
            '>' => {
                cursor.bump();
                Ok(create_token(TokenKind::FatArrow, cursor))
            }
            '=' => {
                cursor.bump();
                Ok(create_token(TokenKind::DoubleEqual, cursor))
            }
            _ => Ok(create_token(TokenKind::Equal, cursor)),
        },
        '!' => match cursor.second() {
            '=' => {
                cursor.bump();
                Ok(create_token(TokenKind::BangEqual, cursor))
            }
            _ => Ok(create_token(TokenKind::Bang, cursor)),
        },
        '<' => match cursor.second() {
            '=' => {
                cursor.bump();
                Ok(create_token(TokenKind::LessEqual, cursor))
            }
            _ => Ok(create_token(TokenKind::Less, cursor)),
        },
        '>' => match cursor.second() {
            '=' => {
                cursor.bump();
                Ok(create_token(TokenKind::GreaterEqual, cursor))
            }
            _ => Ok(create_token(TokenKind::Greater, cursor)),
        },
        '0'..='9' => Ok(parse_numeric_literal(cursor)),
        '"' => {
            cursor.bump(); // Consume the "
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
                    });
                }

                return Ok(Token {
                    kind: TokenKind::Identifier(string),
                    length: cursor.position_within_token(),
                });
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
        'n' => Some('\n'),
        'r' => Some('\r'),
        't' => Some('\t'),
        '0' => Some('\0'),
        '\\' => Some('\\'),
        _ => None,
    }
}

fn escapable_is_string(c: char) -> Option<char> {
    always_escapable(c).or_else(|| match c {
        '"' => Some('"'),
        _ => None,
    })
}

fn escapable_is_char(c: char) -> Option<char> {
    always_escapable(c).or_else(|| match c {
        '\'' => Some('\''),
        _ => None,
    })
}

fn get_reserved_keyword(string: &str) -> Option<TokenKind> {
    match string {
        // Access modifiers
        "pub" => Some(TokenKind::Keyword(Keyword::AccessModifier(
            AccessModifier::Public,
        ))),
        "mod" => Some(TokenKind::Keyword(Keyword::AccessModifier(
            AccessModifier::Module,
        ))),
        "sup" => Some(TokenKind::Keyword(Keyword::AccessModifier(
            AccessModifier::Super,
        ))),

        // Variable declarations
        "let" => Some(TokenKind::Keyword(Keyword::Let)),

        // Types
        "mut" => Some(TokenKind::Keyword(Keyword::Mut)),
        "fun" => Some(TokenKind::Keyword(Keyword::Fun)),
        "struct" => Some(TokenKind::Keyword(Keyword::Struct)),
        "enum" => Some(TokenKind::Keyword(Keyword::Enum)),
        "union" => Some(TokenKind::Keyword(Keyword::Union)),
        // "flags" => Some(TokenKind::Keyword(Keyword::Flags)),
        // "impl" => Some(TokenKind::Keyword(Keyword::Impl)),
        // "trait" => Some(TokenKind::Keyword(Keyword::Trait)),

        // Generics
        "where" => Some(TokenKind::Keyword(Keyword::Where)),
        "is" => Some(TokenKind::Keyword(Keyword::Is)),
        "and" => Some(TokenKind::Keyword(Keyword::And)),

        // Control flow
        "if" => Some(TokenKind::Keyword(Keyword::If)),
        "else" => Some(TokenKind::Keyword(Keyword::Else)),
        "match" => Some(TokenKind::Keyword(Keyword::Match)),
        "arm" => Some(TokenKind::Keyword(Keyword::Arm)),
        "loop" => Some(TokenKind::Keyword(Keyword::Loop)),
        "while" => Some(TokenKind::Keyword(Keyword::While)),
        "for" => Some(TokenKind::Keyword(Keyword::For)),
        "in" => Some(TokenKind::Keyword(Keyword::In)),
        "return" => Some(TokenKind::Keyword(Keyword::Return)),
        "break" => Some(TokenKind::Keyword(Keyword::Break)),
        "continue" => Some(TokenKind::Keyword(Keyword::Continue)),

        // Literals
        "void" => Some(TokenKind::Literal(Literal::Void)),
        "unit" => Some(TokenKind::Literal(Literal::Unit)),
        "true" => Some(TokenKind::Literal(Literal::Bool(true))),
        "false" => Some(TokenKind::Literal(Literal::Bool(false))),

        #[cfg(feature = "interpreter")]
        "drop" => Some(TokenKind::Keyword(Keyword::Drop)),
        #[cfg(feature = "interpreter")]
        "print" => Some(TokenKind::Keyword(Keyword::Print)),

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
