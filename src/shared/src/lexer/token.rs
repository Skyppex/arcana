use std::fmt::Display;

use num_traits::int::PrimInt;

use crate::parser::AccessModifier;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub length: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Comments
    LineComment,
    BlockComment,

    // Whitespace
    WhiteSpace,

    // Operators
    Plus,
    PlusEqual,
    Minus,
    MinusEqual,
    Star,
    StarEqual,
    Slash,
    SlashEqual,
    Percent,
    PercentEqual,
    Ampersand,
    AmpersandEqual,
    DoubleAmpersand,
    Pipe,
    PipeEqual,
    DoublePipe,
    Caret,
    CaretEqual,
    Tilde,
    Equal,
    DoubleEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Keywords
    Keyword(Keyword),

    // Literals
    Literal(Literal),

    // Identifiers
    Identifier(String),

    // Punctuation
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Comma,
    Colon,
    DoubleColon,
    Semicolon,
    Dot,
    QuestionMark,

    EndOfFile,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Unit,
    I8(IntLiteral<i8>),
    I16(IntLiteral<i16>),
    I32(IntLiteral<i32>),
    I64(IntLiteral<i64>),
    I128(IntLiteral<i128>),
    U8(IntLiteral<u8>),
    U16(IntLiteral<u16>),
    U32(IntLiteral<u32>),
    U64(IntLiteral<u64>),
    U128(IntLiteral<u128>),
    F32(f32),
    F64(f64),
    String(String),
    Char(String),
    Bool(bool),
}

impl ToString for Literal {
    fn to_string(&self) -> String {
        match self {
            Literal::Unit => "unit".to_string(),
            Literal::I8(v) => v.to_string(),
            Literal::I16(v) => v.to_string(),
            Literal::I32(v) => v.to_string(),
            Literal::I64(v) => v.to_string(),
            Literal::I128(v) => v.to_string(),
            Literal::U8(v) => v.to_string(),
            Literal::U16(v) => v.to_string(),
            Literal::U32(v) => v.to_string(),
            Literal::U64(v) => v.to_string(),
            Literal::U128(v) => v.to_string(),
            Literal::F32(v) => v.to_string(),
            Literal::F64(v) => v.to_string(),
            Literal::String(v) => v.to_string(),
            Literal::Char(v) => v.to_string(),
            Literal::Bool(v) => v.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntLiteral<T: PrimInt> {
    pub value: T,
    pub base: IntLiteralBase,
}

impl<T: PrimInt + Display> ToString for IntLiteral<T> {
    fn to_string(&self) -> String {
        format!("{}{}", self.base.to_string(), self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntLiteralBase {
    None = 0,
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16,
}

impl ToString for IntLiteralBase {
    fn to_string(&self) -> String {
        match self {
            IntLiteralBase::None => "".to_string(),
            IntLiteralBase::Binary => "0b".to_string(),
            IntLiteralBase::Octal => "0o".to_string(),
            IntLiteralBase::Decimal => "".to_string(),
            IntLiteralBase::Hexadecimal => "0x".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumericLiteralType {
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    // Access modifiers
    AccessModifier(AccessModifier),

    // Variable Declarations
    Let,

    // Types
    Mut,
    Func,
    Struct,
    Union,
    // Flags,
    Impl,

    // Control flow
    If,
    Else,
    // Match,
    Loop,
    While,
    // For,
    Break,
    Continue,
    Return,

    // Literals
    True,
    False,

    // Interpreter specific
    #[cfg(feature = "interpreter")]
    Drop,
    
    #[cfg(feature = "interpreter")]
    Print,
}