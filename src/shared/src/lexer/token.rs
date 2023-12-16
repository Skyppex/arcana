pub enum Token {
    UnaryOp(UnaryOp),
    BinOp(BinOp),

    // Keywords
    Mutable,
    Fn,
    Public,
    Internal,
    Struct,
    Union,

    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    CharLiteral(char),
    BoolLiteral(bool),
    StructLiteral,
    UnionLiteral,

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

    Eof,
}

pub enum UnaryOp {
    BitwiseNot,
    Not,
    Negate,
    Plus,
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
}