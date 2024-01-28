use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone)]
pub enum Statement {
    Program { statements: Vec<Statement> },
    StructDeclaration(StructDeclaration),
    UnionDeclaration(UnionDeclaration),
    // FlagsDeclaration(FlagsDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    Semi(Box<Statement>),
    Break(Option<Expression>),
    Continue,
    Return(Option<Expression>),
    Expression(Expression),

    #[cfg(feature = "interpreter")]
    Print(Expression),
}

type Block = Vec<Statement>;

#[derive(Debug, Clone)]
pub enum Expression {
    None, // For testing purposes

    VariableDeclaration(VariableDeclaration),
    If(If),
    Assignment(Assignment),
    Member(Member),
    Literal(Literal),
    Call(Call),
    Unary(Unary),
    Binary(Binary),
    Ternary(Ternary),
    Block(Block),
    Loop(Block),
    While(While),
    #[cfg(feature = "interpreter")]
    Drop(String),
}

#[derive(Debug, Clone)]
pub struct StructDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub type_name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct UnionDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub type_name: String,
    pub members: Vec<UnionMember>,
}

#[derive(Debug, Clone)]
pub struct FlagsDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub type_name: String,
    pub members: Vec<FlagsMember>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub identifier: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<String>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Unit,
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    F32(f32),
    F64(f64),
    String(String),
    Char(char),
    Bool(bool),
    Struct {
        type_name: String,
        field_initializers: Vec<FieldInitializer>,
    },
    Union {
        type_name: String,
        member: String,
        field_initializers: UnionMemberFieldInitializers,
    },
}

#[derive(Debug, Clone)]
pub struct ConditionBlock {
    pub condition: Box<Expression>,
    pub block: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Box<Expression>,
    pub statements: Vec<Statement>,
    pub else_statements: Option<Vec<Statement>>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub access_modifier: Option<AccessModifier>,
    pub mutable: bool,
    pub identifier: String,
    pub type_name: String,
}

#[derive(Debug, Clone)]
pub struct FieldInitializer {
    pub identifier: Option<String>,
    pub initializer: Expression,
}

#[derive(Debug, Clone)]
pub struct UnionMember {
    pub identifier: String,
    pub fields: Vec<UnionMemberField>,
}

#[derive(Debug, Clone)]
pub struct FlagsMember {
    pub identifier: String,
    pub value: FlagsValue,
}

#[derive(Debug, Clone)]
pub enum FlagsValue {
    Default,
    // Int(String),
    // Compiled(String)
}

impl Display for FlagsValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlagsValue::Default => write!(f, "default"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnionMemberField {
    pub identifier: String,
    pub type_name: String,
}

#[derive(Debug, Clone)]
pub enum UnionMemberFieldInitializers {
    None,
    Named(HashMap<String, Expression>),
    Unnamed(Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AccessModifier {
    Public,
    Internal,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub identifier: String,
    pub type_name: String,
}

#[derive(Debug, Clone)]
pub enum Member {
    Identifier {
        symbol: String,
    },
    MemberAccess {
        object: Box<Expression>,
        member: Box<Member>,
        symbol: String,
    },
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub mutable: bool,
    pub type_name: String,
    pub identifier: String,
    pub initializer: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub r#if: ConditionBlock,
    pub else_ifs: Option<Vec<ConditionBlock>>,
    pub r#else: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub member: Box<Member>,
    pub initializer: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub caller: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: UnaryOperator,
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Ternary {
    pub condition: Box<Expression>,
    pub true_expression: Box<Expression>,
    pub false_expression: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Identity,
    Negate,
    LogicalNot,
    BitwiseNot,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseLeftShift,
    BitwiseRightShift,
    BooleanLogicalAnd,
    BooleanLogicalOr,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}
