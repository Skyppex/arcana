use std::fmt::Display;

use regex;

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

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Void => write!(f, "void"),
            Literal::Unit => write!(f, "unit"),
            Literal::Int(v) => write!(f, "{}", v),
            Literal::UInt(v) => write!(f, "{}", v),
            Literal::Float(v) => write!(f, "{}", v),
            Literal::String(v) => write!(f, "{}", v),
            Literal::Char(v) => write!(f, "{}", v),
            Literal::Bool(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntLiteral<T: PrimInt> {
    pub value: T,
    pub base: IntLiteralBase,
}

impl<T: PrimInt + Display> Display for IntLiteral<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.base, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IntLiteralBase {
    None = 0,
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16,
}

impl Display for IntLiteralBase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntLiteralBase::None => write!(f, ""),
            IntLiteralBase::Binary => write!(f, "0b"),
            IntLiteralBase::Octal => write!(f, "0o"),
            IntLiteralBase::Decimal => write!(f, "0d"),
            IntLiteralBase::Hexadecimal => write!(f, "0x"),
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
    Use,
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
    Imp,
    Proto,
    Type,

    // Generics
    Where,
    Is,
    And,
    Or,

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

    #[cfg(feature = "interpreter")]
    Input,
}

pub trait IdentifierType {
    fn is_type_identifier_name(&self) -> bool;
    fn is_generic_type_identifier_name(&self) -> bool;
    fn is_function_identifier_name(&self) -> bool;
    fn is_variable_identifier_name(&self) -> bool;
    fn is_module_identifier_name(&self) -> bool;
}

impl IdentifierType for str {
    fn is_type_identifier_name(&self) -> bool {
        let regex = regex::Regex::new(r"^[A-Z]\w*$");

        match regex {
            Ok(re) => re.is_match(self),
            Err(_) => false,
        }
    }

    fn is_generic_type_identifier_name(&self) -> bool {
        let regex = regex::Regex::new(r"^T\w*$");

        match regex {
            Ok(re) => re.is_match(self),
            Err(_) => false,
        }
    }

    fn is_function_identifier_name(&self) -> bool {
        let regex = regex::Regex::new(r"^[_a-z][_a-z\d]*$");

        match regex {
            Ok(re) => re.is_match(self),
            Err(_) => false,
        }
    }

    fn is_variable_identifier_name(&self) -> bool {
        let regex = regex::Regex::new(r"^[_a-z][_a-z\d]*$");

        match regex {
            Ok(re) => re.is_match(self),
            Err(_) => false,
        }
    }

    fn is_module_identifier_name(&self) -> bool {
        let regex = regex::Regex::new(r"^[_a-z][_a-z\d]*$");

        match regex {
            Ok(re) => re.is_match(self),
            Err(_) => false,
        }
    }
}

impl IdentifierType for String {
    fn is_type_identifier_name(&self) -> bool {
        self.as_str().is_type_identifier_name()
    }

    fn is_generic_type_identifier_name(&self) -> bool {
        self.as_str().is_generic_type_identifier_name()
    }

    fn is_function_identifier_name(&self) -> bool {
        self.as_str().is_function_identifier_name()
    }

    fn is_variable_identifier_name(&self) -> bool {
        self.as_str().is_variable_identifier_name()
    }

    fn is_module_identifier_name(&self) -> bool {
        self.as_str().is_module_identifier_name()
    }
}
