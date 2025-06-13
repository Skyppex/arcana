use super::{
    cursor::Cursor,
    token::{IntLiteral, IntLiteralBase, Literal, NumericLiteralType, Token, TokenKind},
};

pub fn parse_float_literal_starting_with_dot(cursor: &mut Cursor) -> Result<Token, String> {
    let mut value = String::from(".");
    cursor.bump(); // consume the dot

    while cursor.first().is_ascii_digit() {
        value.push(cursor.first());
        cursor.bump(); // consume the digit
    }

    let kind = TokenKind::Literal(Literal::Float(value.parse::<f64>().unwrap()));

    if let Some(suffix) = parse_suffix(cursor) {
        match suffix {
            NumericLiteralType::Float => {}
            _ => return Err("Invalid suffix for float literal".to_string()),
        }
    }

    Ok(Token {
        kind,
        length: cursor.position_within_token(),
    })
}

pub fn parse_numeric_literal(cursor: &mut Cursor) -> Token {
    let base: IntLiteralBase = parse_base_prefix(cursor).unwrap_or(IntLiteralBase::None);
    let value = parse_numeric_literal_value(cursor, base.clone());
    let suffix = parse_suffix(cursor).unwrap_or_else(|| {
        if base == IntLiteralBase::None {
            if value.contains('.') {
                NumericLiteralType::Float
            } else {
                NumericLiteralType::Int
            }
        } else {
            NumericLiteralType::Int
        }
    });

    let kind = match suffix {
        NumericLiteralType::Int => TokenKind::Literal(Literal::Int(IntLiteral::<i64> {
            value: i64::from_str_radix(&value, base.radix()).unwrap(),
            base,
        })),
        NumericLiteralType::UInt => TokenKind::Literal(Literal::UInt(IntLiteral::<u64> {
            value: u64::from_str_radix(&value, base.radix()).unwrap(),
            base,
        })),
        NumericLiteralType::Float => TokenKind::Literal(Literal::Float(
            value.parse::<f64>().expect("Failed to parse float literal"),
        )),
    };

    Token {
        kind,
        length: cursor.position_within_token(),
    }
}

fn parse_base_prefix(cursor: &mut Cursor) -> Option<IntLiteralBase> {
    if cursor.first() == '0' {
        match cursor.second() {
            'b' => {
                cursor.bump();
                cursor.bump();
                Some(IntLiteralBase::Binary)
            }
            'o' => {
                cursor.bump();
                cursor.bump();
                Some(IntLiteralBase::Octal)
            }
            'd' => {
                cursor.bump();
                cursor.bump();
                Some(IntLiteralBase::Decimal)
            }
            'x' => {
                cursor.bump();
                cursor.bump();
                Some(IntLiteralBase::Hexadecimal)
            }
            _ => None,
        }
    } else {
        None
    }
}

fn parse_numeric_literal_value(cursor: &mut Cursor, base: IntLiteralBase) -> String {
    let mut value = String::new();

    while is_numeric_literal_continue(cursor.first(), cursor.second(), base.clone()) {
        value.push(cursor.first());
        cursor.bump();
    }

    value
}

fn parse_suffix(cursor: &mut Cursor) -> Option<NumericLiteralType> {
    if is_numeric_literal_suffix_start(cursor.first()) {
        let suffix = String::from(cursor.bump().unwrap());
        into_numeric_literal_suffix(&suffix)
    } else {
        None
    }
}

fn is_numeric_literal_continue(first: char, second: char, base: IntLiteralBase) -> bool {
    let base = match base {
        IntLiteralBase::None => 10,
        other => other as u32,
    };

    first.is_digit(base) || (first == '.' && second != '.')
}

fn is_numeric_literal_suffix_start(c: char) -> bool {
    c == 'i' || c == 'u' || c == 'f'
}

fn into_numeric_literal_suffix(suffix: &str) -> Option<NumericLiteralType> {
    match suffix {
        "i" => Some(NumericLiteralType::Int),
        "u" => Some(NumericLiteralType::UInt),
        "f" => Some(NumericLiteralType::Float),
        _ => None,
    }
}
