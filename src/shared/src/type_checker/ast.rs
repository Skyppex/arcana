use crate::parser;

use super::Type;

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
        return_type: String,
        body: Box<TypedStatement>,
        type_: Type,
    },
    Expression(TypedExpression),
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
