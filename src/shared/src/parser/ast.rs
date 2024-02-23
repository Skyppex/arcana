use std::{collections::HashMap, fmt::Display};

use crate::types::{TypeAnnotation, TypeIdentifier};

#[derive(Debug, Clone)]
pub struct Impl{
    pub type_annotation: TypeAnnotation,
    pub functions: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Program { statements: Vec<Statement> },
    StructDeclaration(StructDeclaration),
    EnumDeclaration(EnumDeclaration),
    Impl(Impl),
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
    Index(Index),
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
    pub type_identifier: TypeIdentifier,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct EnumDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub type_identifier: TypeIdentifier,
    pub members: Vec<EnumMember>,
}

#[derive(Debug, Clone)]
pub struct FlagsDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub type_identifier: TypeIdentifier,
    pub members: Vec<FlagsMember>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub identifier: TypeIdentifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<TypeAnnotation>,
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
    Array(Vec<Expression>),
    Struct {
        type_annotation: TypeAnnotation,
        field_initializers: Vec<FieldInitializer>,
    },
    Enum {
        type_annotation: TypeAnnotation,
        member: String,
        field_initializers: EnumMemberFieldInitializers,
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
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct FieldInitializer {
    pub identifier: Option<String>,
    pub initializer: Expression,
}

#[derive(Debug, Clone)]
pub struct EnumMember {
    pub identifier: String,
    pub fields: Vec<EnumMemberField>,
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
pub struct EnumMemberField {
    pub identifier: String,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub enum EnumMemberFieldInitializers {
    None,
    Named(HashMap<String, Expression>),
    Unnamed(Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AccessModifier {
    Public,
    Internal,
    Super,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub identifier: String,
    pub type_annotation: TypeAnnotation,
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
    pub type_annotation: TypeAnnotation,
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
pub struct Index {
    pub caller: Box<Expression>,
    pub index: Box<Expression>,
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
