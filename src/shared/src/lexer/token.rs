use num_traits::int::PrimInt;

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
    Int8(IntLiteral<i8>),
    Int16(IntLiteral<i16>),
    Int32(IntLiteral<i32>),
    Int64(IntLiteral<i64>),
    Int128(IntLiteral<i128>),
    UInt8(IntLiteral<u8>),
    UInt16(IntLiteral<u16>),
    UInt32(IntLiteral<u32>),
    UInt64(IntLiteral<u64>),
    UInt128(IntLiteral<u128>),
    Float32(f32),
    Float64(f64),
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
    Public,
    Internal,

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
}