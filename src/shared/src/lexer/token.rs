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
    DoubleLess,
    LessEqual,
    Greater,
    DoubleGreater,
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
    Semicolon,
    Dot,
    QuestionMark,

    EndOfFile,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
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

#[derive(Debug, Clone, PartialEq)]
pub struct IntLiteral<T: PrimInt> {
    pub value: T,
    pub base: IntLiteralBase,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntLiteralBase {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumericLiteralType {
    Int8,
    Int16,
    Int32,
    Int64,
    Int128,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    UInt128,
    Float32,
    Float64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    // Access modifiers
    AccessModifier(AccessModifier),

    // Types
    Mutable,
    Fn,
    Struct,
    Union,
    // Flags,

    // Control flow
    If,
    Else,
    // Match,
    // While,
    // For,
    // Loop,
    // Break,
    // Continue,
    // Return,

    // Literals
    True,
    False,

    // Interpreter specific
    #[cfg(feature = "interpreter")]
    Drop,
    
    #[cfg(feature = "interpreter")]
    Print,
}