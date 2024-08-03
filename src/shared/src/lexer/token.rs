use std::fmt::Display;

use num_traits::int::PrimInt;

use crate::{parser::AccessModifier, pretty_print::PrettyPrint};

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub length: u32,
}

impl<T: IntoIterator<Item = Token> + Clone> PrettyPrint for T {
    fn prettify(&self) -> String {
        self.clone()
            .into_iter()
            .map(|token| format!("{:?} -> {}", token.kind, token.length))
            .collect::<Vec<String>>()
            .join("\n")
    }
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
    Underscore,

    // Keywords
    Keyword(Keyword),

    // Literals
    Literal(Literal),
    Hash,

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
    DoubleDot,
    QuestionMark,
    Arrow,
    FatArrow,

    EndOfFile,
}

impl TokenKind {
    pub fn is_access_modifier(&self) -> Option<AccessModifier> {
        match self {
            TokenKind::Keyword(Keyword::Pub) => Some(AccessModifier::Public),
            TokenKind::Keyword(Keyword::Sup) => Some(AccessModifier::Super),
            TokenKind::Keyword(Keyword::Mod) => Some(AccessModifier::Module),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Void,
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
            Literal::Void => "void".to_string(),
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
    // Modules & Access modifiers
    Mod,
    Pub,
    Sup,

    // Variable declarations
    Let,

    // Types
    Mut,
    Fun,
    Struct,
    Enum,
    Union,
    // Flags,
    // Impl,
    // Trait,
    Type,

    // Generics
    Where,
    Is,
    And,

    // Control flow
    If,
    Else,
    Match,
    Loop,
    While,
    For,
    In,
    Return,
    Break,
    Continue,

    // Interpreter specific
    #[cfg(feature = "interpreter")]
    Drop,

    #[cfg(feature = "interpreter")]
    Print,
}
