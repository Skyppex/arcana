use std::collections::HashMap;

use crate::{parser, types::TypeName};

use super::Type;

pub trait Typed {
    fn get_type(&self) -> Type;
    fn get_deep_type(&self) -> Type;
}

#[derive(Debug, Clone, PartialEq)]
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
        body: Vec<TypedStatement>,
        type_: Type,
    },
    Impl {
        type_name: String,
        functions: Vec<TypedStatement>,
    },
    Semi(Box<TypedStatement>),
    Break(Option<TypedExpression>),
    Continue,
    Return(Option<TypedExpression>),
    Expression(TypedExpression),

    #[cfg(feature = "interpreter")]
    Print(TypedExpression),
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
            TypedStatement::Impl { .. } => Type::Void,
            TypedStatement::Semi { .. } => Type::Void,
            TypedStatement::Break(_) => Type::Void,
            TypedStatement::Continue => Type::Void,
            TypedStatement::Return(_) => Type::Void,
            TypedStatement::Expression(e) => e.get_type(),
            TypedStatement::Print(_) => Type::Void,
        }
    }

    fn get_deep_type(&self) -> Type {
        match self {
            TypedStatement::None => Type::Void,
            TypedStatement::Program { .. } => Type::Void,
            TypedStatement::StructDeclaration { type_, .. } => type_.clone(),
            TypedStatement::UnionDeclaration { type_, .. } => type_.clone(),
            TypedStatement::FunctionDeclaration { type_, .. } => type_.clone(),
            TypedStatement::Impl { .. } => Type::Void,
            TypedStatement::Semi(e) => e.get_deep_type(),
            TypedStatement::Break(e) => e.as_ref().map_or(Type::Void, |e| e.get_deep_type()),
            TypedStatement::Continue => Type::Void,
            TypedStatement::Return(e) => e.as_ref().map_or(Type::Void, |e| e.get_deep_type()),
            TypedStatement::Expression(e) => e.get_deep_type(),
            TypedStatement::Print(e) => e.get_deep_type(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block{
    pub statements: Vec<TypedStatement>,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
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
        else_ifs: Vec<ConditionBlock>,
        r#else: Option<Box<TypedExpression>>,
        type_: Type,
    },
    Assignment {
        member: Box<Member>,
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
    Index {
        caller: Box<TypedExpression>,
        argument: Box<TypedExpression>,
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
    Block(Block),
    Drop {
        identifier: String,
        type_: Type
    },
    Loop(Block),
    While {
        condition: Box<TypedExpression>,
        block: Vec<TypedStatement>,
        else_block: Option<Vec<TypedStatement>>,
        type_: Type,
    },
}

impl Typed for TypedExpression {
    fn get_type(&self) -> Type {
        match self {
            TypedExpression::None => Type::Void,
            TypedExpression::VariableDeclaration { type_, .. } => type_.clone(),
            TypedExpression::If { type_, .. } => type_.clone(),
            TypedExpression::Assignment { type_, .. } => type_.clone(),
            TypedExpression::Member(member) => member.get_type(),
            TypedExpression::Literal(literal) => literal.get_type(),
            TypedExpression::Call { type_, .. } => type_.clone(),
            TypedExpression::Index { type_, .. } => type_.clone(),
            TypedExpression::Unary { type_, .. } => type_.clone(),
            TypedExpression::Binary { type_, .. } => type_.clone(),
            TypedExpression::Ternary { type_, .. } => type_.clone(),
            TypedExpression::Block(Block { type_, .. }) => type_.clone(),
            TypedExpression::Drop { type_, .. } => type_.clone(),
            TypedExpression::Loop(Block { type_, .. }) => type_.clone(),
            TypedExpression::While { type_, .. } => type_.clone(),
        }
    }

    fn get_deep_type(&self) -> Type {
        match self {
            TypedExpression::None => Type::Void,
            TypedExpression::VariableDeclaration { type_, .. } => type_.clone(),
            TypedExpression::If { type_, .. } => type_.clone(),
            TypedExpression::Assignment { type_, .. } => type_.clone(),
            TypedExpression::Member(member) => member.get_deep_type(),
            TypedExpression::Literal(literal) => literal.get_deep_type(),
            TypedExpression::Call { type_, .. } => type_.clone(),
            TypedExpression::Index { type_, .. } => type_.clone(),
            TypedExpression::Unary { type_, .. } => type_.clone(),
            TypedExpression::Binary { type_, .. } => type_.clone(),
            TypedExpression::Ternary { type_, .. } => type_.clone(),
            TypedExpression::Block(Block { type_, .. }) => type_.clone(),
            TypedExpression::Drop { type_, .. } => type_.clone(),
            TypedExpression::Loop(Block { type_, .. }) => type_.clone(),
            TypedExpression::While { type_, .. } => type_.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub mutable: bool,
    pub identifier: String,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AccessModifier {
    Public,
    Internal,
    Super,
}

impl Into<AccessModifier> for parser::AccessModifier {
    fn into(self) -> AccessModifier {
        match self {
            parser::AccessModifier::Public => AccessModifier::Public,
            parser::AccessModifier::Internal => AccessModifier::Internal,
            parser::AccessModifier::Super => AccessModifier::Super,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionMember {
    pub union_name: String,
    pub discriminant_name: String,
    pub fields: Vec<UnionMemberField>,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionMemberField {
    pub union_name: String,
    pub discriminant_name: String,
    pub identifier: String,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
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
    Array { values: Vec<TypedExpression>, type_: Type },
    Struct {
        type_name: TypeName,
        field_initializers: Vec<FieldInitializer>,
        type_: Type,
    },
    Union {
        type_name: TypeName,
        member: String,
        field_initializers: UnionMemberFieldInitializers,
        type_: Type,
    },
}

impl Typed for Literal {
    fn get_type(&self) -> Type {
        match self {
            Literal::Unit => Type::Unit,
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
            Literal::Array { type_, .. } => Type::Array(Box::new(type_.clone())),
            Literal::Struct { type_, .. } => type_.clone(),
            Literal::Union { type_, .. } => type_.clone(),
        }
    }

    fn get_deep_type(&self) -> Type {
        self.get_type()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionBlock {
    pub condition: Box<TypedExpression>,
    pub block: Box<TypedExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldInitializer {
    pub identifier: Option<String>,
    pub initializer: TypedExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnionMemberFieldInitializers {
    None,
    Named(HashMap<String, TypedExpression>),
    Unnamed(Vec<TypedExpression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub identifier: String,
    pub type_name: String,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
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

impl Member {
    pub fn get_symbol(&self) -> &str {
        match self {
            Member::Identifier { symbol, .. } => symbol,
            Member::MemberAccess { symbol, .. } => symbol,
        }
    }
}

impl Typed for Member {
    fn get_type(&self) -> Type {
        match self {
            Member::Identifier { type_, .. } => type_.clone(),
            Member::MemberAccess { type_, .. } => type_.clone(),
        }
    }

    fn get_deep_type(&self) -> Type {
        match self {
            Member::Identifier { type_, .. } => type_.clone(),
            Member::MemberAccess { member, .. } => {
                member.get_deep_type()
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Identity,
    Negate,
    LogicalNot,
    BitwiseNot,
}

#[derive(Debug, Clone, PartialEq)]
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
