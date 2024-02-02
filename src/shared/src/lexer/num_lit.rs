use super::{cursor::Cursor, token::{Token, NumericLiteralType, IntLiteralBase, TokenKind, IntLiteral, Literal}};


pub fn parse_numeric_literal(cursor: &mut Cursor) -> Token {
    let base: IntLiteralBase = parse_base_prefix(cursor).unwrap_or(IntLiteralBase::None);
    let value = parse_numeric_literal_value(cursor, base.clone());
    let suffix = parse_suffix(cursor).unwrap_or_else(|| {
        if base == IntLiteralBase::None {
            if value.contains('.') {
                NumericLiteralType::F64
            } else {
                NumericLiteralType::I64
            }
        } else {
            NumericLiteralType::I64
        }
    });

    let kind = match suffix {
        NumericLiteralType::I8 => TokenKind::Literal(Literal::I8(IntLiteral::<i8> {
            value: value.parse::<i8>().expect("Failed to parse i8 literal"),
            base,
        })),
        NumericLiteralType::I16 => TokenKind::Literal(Literal::I16(IntLiteral::<i16> {
            value: value.parse::<i16>().unwrap(),
            base,
        })),
        NumericLiteralType::I32 => TokenKind::Literal(Literal::I32(IntLiteral::<i32> {
            value: value.parse::<i32>().unwrap(),
            base,
        })),
        NumericLiteralType::I64 => TokenKind::Literal(Literal::I64(IntLiteral::<i64> {
            value: value.parse::<i64>().unwrap(),
            base,
        })),
        NumericLiteralType::I128 => TokenKind::Literal(Literal::I128(IntLiteral::<i128> {
            value: value.parse::<i128>().unwrap(),
            base,
        })),
        NumericLiteralType::U8 => TokenKind::Literal(Literal::U8(IntLiteral::<u8> {
            value: value.parse::<u8>().unwrap(),
            base,
        })),
        NumericLiteralType::U16 => TokenKind::Literal(Literal::U16(IntLiteral::<u16> {
            value: value.parse::<u16>().unwrap(),
            base,
        })),
        NumericLiteralType::U32 => TokenKind::Literal(Literal::U32(IntLiteral::<u32> {
            value: value.parse::<u32>().unwrap(),
            base,
        })),
        NumericLiteralType::U64 => TokenKind::Literal(Literal::U64(IntLiteral::<u64> {
            value: value.parse::<u64>().unwrap(),
            base,
        })),
        NumericLiteralType::U128 => TokenKind::Literal(Literal::U128(IntLiteral::<u128> {
            value: value.parse::<u128>().unwrap(),
            base,
        })),
        NumericLiteralType::F32 => TokenKind::Literal(Literal::F32(value.parse::<f32>().expect("Failed to parse f32 literal"))),
        NumericLiteralType::F64 => TokenKind::Literal(Literal::F64(value.parse::<f64>().expect("Failed to parse f64 literal"))),
    };

    Token {
        kind,
        length: cursor.position_within_token(),
    }
}

fn parse_base_prefix(cursor: &mut Cursor) -> Option<IntLiteralBase> {
    if cursor.first() == '0' {
        match cursor.second() {
            'b' => {cursor.bump(); cursor.bump(); Some(IntLiteralBase::Binary)},
            'o' => {cursor.bump(); cursor.bump(); Some(IntLiteralBase::Octal)},
            'd' => {cursor.bump(); cursor.bump(); Some(IntLiteralBase::Decimal)},
            'x' => {cursor.bump(); cursor.bump(); Some(IntLiteralBase::Hexadecimal)},
            _ => None,
        }
    } else {
        None
    }
}

fn parse_numeric_literal_value(cursor: &mut Cursor, base: IntLiteralBase) -> String {
    let mut value = String::new();

    while is_numeric_literal_continue(cursor.first(), base.clone()) {
        value.push(cursor.first());
        cursor.bump();
    }
    
    value
}

fn parse_suffix(cursor: &mut Cursor) -> Option<NumericLiteralType> {
    if is_numeric_literal_suffix_start(cursor.first()) {
        let mut suffix = String::from(cursor.bump().unwrap());

        while cursor.first().is_digit(10) {
            suffix.push(cursor.first());
            cursor.bump();
        }

        into_numeric_literal_suffix(&suffix)
    } else {
        None
    }
}

fn is_numeric_literal_continue(c: char, base: IntLiteralBase) -> bool {
    let base = match base {
        IntLiteralBase::None => 10,
        other => other as u32,
    };

    c.is_digit(base) || c == '.'
}

fn is_numeric_literal_suffix_start(c: char) -> bool {
    c == 'i' || c == 'u' || c == 'f'
}

fn into_numeric_literal_suffix(suffix: &str) -> Option<NumericLiteralType> {
    match suffix {
        "i8" => Some(NumericLiteralType::I8),
        "i16" => Some(NumericLiteralType::I16),
        "i32" => Some(NumericLiteralType::I32),
        "i64" => Some(NumericLiteralType::I64),
        "i128" => Some(NumericLiteralType::I128),
        "u8" => Some(NumericLiteralType::U8),
        "u16" => Some(NumericLiteralType::U16),
        "u32" => Some(NumericLiteralType::U32),
        "u64" => Some(NumericLiteralType::U64),
        "u128" => Some(NumericLiteralType::U128),
        "f32" => Some(NumericLiteralType::F32),
        "f64" => Some(NumericLiteralType::F64),
        _ => None,
    }
}
