use std::{collections::HashMap, fmt::Display};
use std::hash::Hash;

use crate::{parser, types::{GenericConstraint, TypeAnnotation, TypeIdentifier}};

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
        type_identifier: TypeIdentifier,
        where_clause: Option<Vec<GenericConstraint>>,
        fields: Vec<StructField>,
        type_: Type,
    },
    EnumDeclaration {
        type_identifier: TypeIdentifier,
        members: Vec<EnumMember>,
        type_: Type,
    },
    UnionDeclaration {
        type_identifier: TypeIdentifier,
        literals: Vec<TypeAnnotation>,
        type_: Type,
    },
    FunctionDeclaration {
        identifier: TypeIdentifier,
        parameters: Vec<Parameter>,
        return_type: Type,
        body: Vec<TypedStatement>,
        type_: Type,
    },
    Impl {
        type_annotation: TypeAnnotation,
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
            TypedStatement::EnumDeclaration { type_, .. } => type_.clone(),
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
            TypedStatement::EnumDeclaration { type_, .. } => type_.clone(),
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

impl Display for TypedStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedStatement::None => write!(f, "None"),
            TypedStatement::Program { statements } => write!(f, "{}", statements.iter().map(|s| s.to_string()).collect::<Vec<String>>().join(", ")),
            TypedStatement::StructDeclaration { type_identifier, fields, .. } => write!(f, "struct {} {{{}}}", type_identifier, fields.iter().map(|f| f.to_string()).collect::<Vec<String>>().join(", ")),
            TypedStatement::EnumDeclaration { type_identifier, members, .. } => write!(f, "enum {} {{{}}}", type_identifier, members.iter().map(|m| m.to_string()).collect::<Vec<String>>().join(", ")),
            TypedStatement::UnionDeclaration { type_identifier, literals, .. } => write!(f, "union {} {{{}}}", type_identifier, literals.iter().map(|l| l.to_string()).collect::<Vec<String>>().join(", ")),
            TypedStatement::FunctionDeclaration { identifier, parameters, return_type, body, .. } => write!(f, "fn {}({}) -> {} {{{}}}", identifier, parameters.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(", "), return_type, body.iter().map(|s| s.to_string()).collect::<Vec<String>>().join(", ")),
            TypedStatement::Impl { type_annotation, functions } => write!(f, "impl {} {{{}}}", type_annotation, functions.iter().map(|s| s.to_string()).collect::<Vec<String>>().join(", ")),
            TypedStatement::Semi(s) => write!(f, "{};", s),
            TypedStatement::Break(e) => write!(f, "break{}", if let Some(e) = e { format!(" {}", e) } else { "".to_string() }),
            TypedStatement::Continue => write!(f, "continue"),
            TypedStatement::Return(e) => write!(f, "return{}", if let Some(e) = e { format!(" {}", e) } else { "".to_string() }),
            TypedStatement::Expression(e) => write!(f, "{}", e),
            TypedStatement::Print(e) => write!(f, "print {}", e),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block{
    pub statements: Vec<TypedStatement>,
    pub type_: Type,
}

impl Typed for Block {
    fn get_type(&self) -> Type {
        self.type_.clone()
    }

    fn get_deep_type(&self) -> Type {
        self.type_.clone()
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{{}}}", self.statements.iter().map(|s| s.to_string()).collect::<Vec<String>>().join(", "))
    }
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

impl Display for TypedExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedExpression::None => write!(f, "None"),
            TypedExpression::VariableDeclaration { mutable, identifier, initializer, .. } => {
                if let Some(initializer) = initializer {
                    write!(f, "{}{} = {}", if *mutable { "mut " } else { "" }, identifier, initializer)
                } else {
                    write!(f, "{}{}", if *mutable { "mut " } else { "" }, identifier)
                }
            },
            TypedExpression::If { r#if, else_ifs, r#else, .. } => {
                write!(f, "if {} {{ {} }}", r#if.condition, r#if.block)?;
                for else_if in else_ifs {
                    write!(f, " else if {} {{ {} }}", else_if.condition, else_if.block)?;
                }
                if let Some(r#else) = r#else {
                    write!(f, " else {{ {} }}", r#else)?;
                }
                Ok(())
            },
            TypedExpression::Assignment { member, initializer, .. } => write!(f, "{} = {}", member, initializer),
            TypedExpression::Member(member) => write!(f, "{}", member),
            TypedExpression::Literal(literal) => write!(f, "{}", literal),
            TypedExpression::Call { caller, arguments, .. } => write!(f, "{}({})", caller, arguments.iter().map(|a| a.to_string()).collect::<Vec<String>>().join(", ")),
            TypedExpression::Index { caller, argument, .. } => write!(f, "{}[{}]", caller, argument),
            TypedExpression::Unary { operator, expression, .. } => write!(f, "{}{}", operator, expression),
            TypedExpression::Binary { left, operator, right, .. } => write!(f, "{} {} {}", left, operator, right),
            TypedExpression::Ternary { condition, true_expression, false_expression, .. } => write!(f, "{} ? {} : {}", condition, true_expression, false_expression),
            TypedExpression::Block(block) => write!(f, "{}", block),
            TypedExpression::Drop { identifier, .. } => write!(f, "drop {}", identifier),
            TypedExpression::Loop(block) => write!(f, "loop {}", block),
            TypedExpression::While { condition, block, else_block, .. } => {
                write!(f, "while {} {{ {} }}", condition, block.iter().map(|s| s.to_string()).collect::<Vec<String>>().join(", "))?;
                if let Some(else_block) = else_block {
                    write!(f, " else {{ {} }}", else_block.iter().map(|s| s.to_string()).collect::<Vec<String>>().join(", "))?;
                }
                Ok(())
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub mutable: bool,
    pub identifier: String,
    pub type_: Type,
}

impl Display for StructField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}: {}", if self.mutable { "mut " } else { "" }, self.identifier, self.type_)
    }
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
pub struct EnumMember {
    pub enum_name: TypeIdentifier,
    pub discriminant_name: String,
    pub fields: Vec<EnumMemberField>,
    pub type_: Type,
}

impl Display for EnumMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{{}}}", self.enum_name, self.fields.iter().map(|f| f.to_string()).collect::<Vec<String>>().join(", "))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumMemberField {
    pub enum_name: TypeIdentifier,
    pub discriminant_name: String,
    pub identifier: String,
    pub type_: Type,
}

impl Display for EnumMemberField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.identifier, self.type_)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Unit,
    Int(i64),
    UInt(u64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Array { values: Vec<TypedExpression>, type_: Type },
    Struct {
        type_annotation: TypeAnnotation,
        field_initializers: Vec<FieldInitializer>,
        type_: Type,
    },
    Enum {
        type_annotation: TypeAnnotation,
        member: String,
        field_initializers: EnumMemberFieldInitializers,
        type_: Type,
    },
}

impl Eq for Literal {}
impl Hash for Literal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

impl Typed for Literal {
    fn get_type(&self) -> Type {
        match self {
            Literal::Unit => Type::Literal { name: "unit".to_string(), type_: Box::new(Type::Unit) },
            Literal::Int(v) => Type::Literal { name: v.to_string(), type_: Box::new(Type::Int) },
            Literal::UInt(v) => Type::Literal { name: v.to_string(), type_: Box::new(Type::UInt) },
            Literal::Float(v) => Type::Literal { name: v.to_string(), type_: Box::new(Type::Float) },
            Literal::String(v) => Type::Literal { name: format!("\"{}\"", v.to_string()), type_: Box::new(Type::String) },
            Literal::Char(v) => Type::Literal { name: format!("'{}'", v.to_string()), type_: Box::new(Type::Char) },
            Literal::Bool(v) => Type::Literal { name: v.to_string(), type_: Box::new(Type::Bool) },
            Literal::Array { type_, .. } => Type::Array(Box::new(type_.clone())),
            Literal::Struct { type_, .. } => type_.clone(),
            Literal::Enum { type_, .. } => type_.clone(),
        }
    }

    fn get_deep_type(&self) -> Type {
        match self {
            Literal::Unit => Type::Unit,
            Literal::Int(_) => Type::Int,
            Literal::UInt(_) => Type::UInt,
            Literal::Float(_) => Type::Float,
            Literal::String(_) => Type::String,
            Literal::Char(_) => Type::Char,
            Literal::Bool(_) => Type::Bool,
            Literal::Array { type_, .. } => type_.clone(),
            Literal::Struct { type_, .. } => type_.clone(),
            Literal::Enum { type_, .. } => type_.clone(),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Unit => write!(f, "unit"),
            Literal::Int(v) => write!(f, "{}", v),
            Literal::UInt(v) => write!(f, "{}", v),
            Literal::Float(v) => write!(f, "{}", v),
            Literal::String(v) => write!(f, "\"{}\"", v),
            Literal::Char(v) => write!(f, "'{}'", v),
            Literal::Bool(v) => write!(f, "{}", v),
            Literal::Array { values, .. } => write!(f, "[{}]", values.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(", ")),
            Literal::Struct { field_initializers, .. } => write!(f, "{{{}}}", field_initializers.iter().map(|fi| {
                if let Some(identifier) = &fi.identifier {
                    format!("{}: {}", identifier, fi.initializer.to_string())
                } else {
                    fi.initializer.to_string()
                }
            }).collect::<Vec<String>>().join(", ")),
            Literal::Enum { field_initializers, .. } => write!(f, "{}", field_initializers.to_string()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionBlock {
    pub condition: Box<TypedExpression>,
    pub block: Box<TypedExpression>,
}

impl Display for ConditionBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{ {} }}", self.condition, self.block)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldInitializer {
    pub identifier: Option<String>,
    pub initializer: TypedExpression,
}

impl Display for FieldInitializer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(identifier) = &self.identifier {
            write!(f, "{}: {}", identifier, self.initializer)
        } else {
            write!(f, "{}", self.initializer)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumMemberFieldInitializers {
    None,
    Named(HashMap<String, TypedExpression>),
    Unnamed(Vec<TypedExpression>),
}

impl Display for EnumMemberFieldInitializers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EnumMemberFieldInitializers::None => write!(f, ""),
            EnumMemberFieldInitializers::Named(field_initializers) => write!(f, "{{{}}}", field_initializers.iter().map(|(k, v)| format!("{}: {}", k, v)).collect::<Vec<String>>().join(", ")),
            EnumMemberFieldInitializers::Unnamed(field_initializers) => write!(f, "({})", field_initializers.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(", ")),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub identifier: String,
    pub type_annotation: TypeAnnotation,
    pub type_: Type,
}

impl Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.identifier, self.type_annotation)
    }
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

impl Display for Member {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Member::Identifier { symbol, .. } => write!(f, "{}", symbol),
            Member::MemberAccess { object, member, symbol, .. } => write!(f, "{}.{}", object, member),
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

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Identity => write!(f, "+"),
            UnaryOperator::Negate => write!(f, "-"),
            UnaryOperator::LogicalNot => write!(f, "!"),
            UnaryOperator::BitwiseNot => write!(f, "~"),
        }
    }
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

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Subtract => write!(f, "-"),
            BinaryOperator::Multiply => write!(f, "*"),
            BinaryOperator::Divide => write!(f, "/"),
            BinaryOperator::Modulo => write!(f, "%"),
            BinaryOperator::BitwiseAnd => write!(f, "&"),
            BinaryOperator::BitwiseOr => write!(f, "|"),
            BinaryOperator::BitwiseXor => write!(f, "^"),
            BinaryOperator::BitwiseLeftShift => write!(f, "<<"),
            BinaryOperator::BitwiseRightShift => write!(f, ">>"),
            BinaryOperator::BooleanLogicalAnd => write!(f, "&&"),
            BinaryOperator::BooleanLogicalOr => write!(f, "||"),
            BinaryOperator::Equal => write!(f, "=="),
            BinaryOperator::NotEqual => write!(f, "!="),
            BinaryOperator::LessThan => write!(f, "<"),
            BinaryOperator::LessThanOrEqual => write!(f, "<="),
            BinaryOperator::GreaterThan => write!(f, ">"),
            BinaryOperator::GreaterThanOrEqual => write!(f, ">="),
        }
    }
}