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
    Int(IntLiteral<i64>),
    UInt(IntLiteral<u64>),
    Float(f64),
    String(String),
    Char(String),
    Bool(bool),
}

impl ToString for Literal {
    fn to_string(&self) -> String {
        match self {
            Literal::Unit => "unit".to_string(),
            Literal::Int(v) => v.to_string(),
            Literal::UInt(v) => v.to_string(),
            Literal::Float(v) => v.to_string(),
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
    Int,
    UInt,
    Float,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    // Access modifiers
    AccessModifier(AccessModifier),

    // Variable declarations
    Let,

    // Types
    Mut,
    Func,
    Struct,
    Enum,
    Union,
    // Flags,
    Impl,
    Trait,
    Type,

    // Generics
    Where,
    Is,
    And,

    // Control flow
    If,
    Else,
    // Match,
    Loop,
    While,
    // For,
    Return,
    Break,
    Continue,

    // Interpreter specific
    #[cfg(feature = "interpreter")]
    Drop,
    
    #[cfg(feature = "interpreter")]
    Print,
}