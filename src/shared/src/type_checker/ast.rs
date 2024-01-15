use crate::parser;

use super::Type;

pub trait Typed {
    fn get_type(&self) -> Type;
}

#[derive(Debug, Clone)]
pub enum TypedStatement {
    None,
    Program {
        statements: Vec<TypedStatement>,
    },
    StructDeclaration {
        type_name: String,
        fields: Vec<StructField>,
        type_: Type,
    },
    UnionDeclaration {
        type_name: String,
        members: Vec<UnionMember>,
        type_: Type,
    },
    FunctionDeclaration {
        identifier: String,
        parameters: Vec<Parameter>,
        return_type: Type,
        body: Box<TypedExpression>,
        type_: Type,
    },
    Expression(TypedExpression),
}

impl TypedStatement {
    pub fn as_expression(&self) -> Option<&TypedExpression> {
        match self {
            TypedStatement::Expression(e) => Some(e),
            _ => None,
        }
    }
}

impl Typed for TypedStatement {
    fn get_type(&self) -> Type {
        match self {
            TypedStatement::None => Type::Void,
            TypedStatement::Program { .. } => Type::Void,
            TypedStatement::StructDeclaration { type_, .. } => type_.clone(),
            TypedStatement::UnionDeclaration { type_, .. } => type_.clone(),
            TypedStatement::FunctionDeclaration { type_, .. } => type_.clone(),
            TypedStatement::Expression(e) => e.get_type(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypedExpression {
    None, // For testing purposes

    VariableDeclaration {
        mutable: bool,
        identifier: String,
        initializer: Option<Box<TypedExpression>>,
        type_: Type
    },
    If {
        r#if: ConditionBlock,
        else_ifs: Option<Vec<ConditionBlock>>,
        r#else: Option<Box<TypedExpression>>,
        type_: Type,
    },
    Assignment {
        member: Box<TypedExpression>,
        initializer: Box<TypedExpression>,
        type_: Type,
    },
    Member(Member),
    Literal(Literal),
    Call {
        caller: Box<TypedExpression>,
        arguments: Vec<TypedExpression>,
        type_: Type,
    },
    Unary {
        operator: UnaryOperator,
        expression: Box<TypedExpression>,
        type_: Type,
    },
    Binary {
        left: Box<TypedExpression>,
        operator: BinaryOperator,
        right: Box<TypedExpression>,
        type_: Type,
    },
    Ternary {
        condition: Box<TypedExpression>,
        true_expression: Box<TypedExpression>,
        false_expression: Box<TypedExpression>,
        type_: Type,
    },
    Block {
        statements: Vec<TypedStatement>,
        type_: Type,
    },
    Drop {
        identifier: String,
        type_: Type
    },
}

impl Typed for TypedExpression {
    fn get_type(&self) -> Type {
        match self {
            TypedExpression::None => Type::Void,
            TypedExpression::VariableDeclaration { mutable, identifier, initializer, type_ } => type_.clone(),
            TypedExpression::If { r#if, else_ifs, r#else, type_ } => type_.clone(),
            TypedExpression::Assignment { member, initializer, type_ } => type_.clone(),
            TypedExpression::Member(member) => member.get_type(),
            TypedExpression::Literal(literal) => literal.get_type(),
            TypedExpression::Call { caller, arguments, type_ } => type_.clone(),
            TypedExpression::Unary { operator, expression, type_ } => type_.clone(),
            TypedExpression::Binary { left, operator, right, type_ } => type_.clone(),
            TypedExpression::Ternary { condition, true_expression, false_expression, type_ } => type_.clone(),
            TypedExpression::Block { statements, type_ } => type_.clone(),
            TypedExpression::Drop { identifier, type_ } => type_.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub mutable: bool,
    pub identifier: String,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AccessModifier {
    Public,
    Internal,
}

impl Into<AccessModifier> for parser::AccessModifier {
    fn into(self) -> AccessModifier {
        match self {
            parser::AccessModifier::Public => AccessModifier::Public,
            parser::AccessModifier::Internal => AccessModifier::Internal,
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnionMember {
    pub union_name: String,
    pub discriminant_name: String,
    pub fields: Vec<UnionMemberField>,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub struct UnionMemberField {
    pub union_name: String,
    pub discriminant_name: String,
    pub field_position: usize,
    pub identifier: Option<String>,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub enum Literal {
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
        field_initializers: Option<Vec<FieldInitializer>>,
        type_: Type,
    },
    Union {
        type_name: String,
        member: String,
        field_initializers: Option<Vec<FieldInitializer>>,
        type_: Type,
    },
}

impl Typed for Literal {
    fn get_type(&self) -> Type {
        match self {
            Literal::I8(_) => Type::I8,
            Literal::I16(_) => Type::I16,
            Literal::I32(_) => Type::I32,
            Literal::I64(_) => Type::I64,
            Literal::I128(_) => Type::I128,
            Literal::U8(_) => Type::U8,
            Literal::U16(_) => Type::U16,
            Literal::U32(_) => Type::U32,
            Literal::U64(_) => Type::U64,
            Literal::U128(_) => Type::U128,
            Literal::F32(_) => Type::F32,
            Literal::F64(_) => Type::F64,
            Literal::String(_) => Type::String,
            Literal::Char(_) => Type::Char,
            Literal::Bool(_) => Type::Bool,
            Literal::Struct { type_, .. } => type_.clone(),
            Literal::Union { type_, .. } => type_.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConditionBlock {
    pub condition: Box<TypedExpression>,
    pub block: Box<TypedExpression>,
}

#[derive(Debug, Clone)]
pub struct FieldInitializer {
    pub identifier: Option<String>,
    pub initializer: TypedExpression,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub identifier: String,
    pub type_name: String,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub enum Member {
    Identifier {
        symbol: String,
        type_: Type,
    },
    MemberAccess {
        object: Box<TypedExpression>,
        member: Box<Member>,
        symbol: String,
        type_: Type,
    },
}

impl Typed for Member {
    fn get_type(&self) -> Type {
        match self {
            Member::Identifier { type_, .. } => type_.clone(),
            Member::MemberAccess { type_, .. } => type_.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Negation,
    LogicalNot,
    BitwiseNot,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
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
