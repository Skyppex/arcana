use std::hash::Hash;
use std::{collections::HashMap, fmt::Display};

use crate::display::{Indent, IndentDisplay};
use crate::pretty_print::PrettyPrint;
use crate::types::{GenericConstraint, TypeAnnotation, TypeIdentifier};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Program {
        statements: Vec<Statement>,
    },
    StructDeclaration(StructDeclaration),
    EnumDeclaration(EnumDeclaration),
    UnionDeclaration(UnionDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    Semi(Box<Statement>),
    Break(Option<Expression>),
    Continue,
    Return(Option<Expression>),
    Expression(Expression),

    #[cfg(feature = "interpreter")]
    Print(Expression),
}

impl PrettyPrint for Statement {
    fn prettify(&self) -> String {
        let mut indent = Indent::new();
        self.indent_display(&mut indent)
    }
}

type Block = Vec<Statement>;

#[derive(Debug, Clone, PartialEq)]
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

impl PrettyPrint for Expression {
    fn prettify(&self) -> String {
        let mut indent = Indent::new();
        self.indent_display(&mut indent)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub type_identifier: TypeIdentifier,
    pub where_clause: Option<Vec<GenericConstraint>>,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub type_identifier: TypeIdentifier,
    pub members: Vec<EnumMember>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub type_identifier: TypeIdentifier,
    pub literals: Vec<Literal>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub identifier: TypeIdentifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<TypeAnnotation>,
    pub body: Block,
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

impl Eq for Literal {}
impl Hash for Literal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
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
            Literal::Array(array) => format!(
                "[{}]",
                array
                    .iter()
                    .map(|e| {
                        if let Expression::Literal(literal) = e {
                            literal.to_string()
                        } else {
                            panic!("Array element is not a literal")
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Literal::Struct {
                type_annotation,
                field_initializers,
            } => todo!(),
            Literal::Enum {
                type_annotation,
                member,
                field_initializers,
            } => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub condition: Box<Expression>,
    pub statements: Vec<Statement>,
    pub else_statements: Option<Vec<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub access_modifier: Option<AccessModifier>,
    pub mutable: bool,
    pub identifier: String,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldInitializer {
    pub identifier: Option<String>,
    pub initializer: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumMember {
    pub identifier: String,
    pub fields: Vec<EnumMemberField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FlagsMember {
    pub identifier: String,
    pub value: FlagsValue,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct EnumMemberField {
    pub identifier: String,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumMemberFieldInitializers {
    None,
    Named(HashMap<String, Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AccessModifier {
    Public,
    Internal,
    Super,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub identifier: String,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub mutable: bool,
    pub type_annotation: TypeAnnotation,
    pub identifier: String,
    pub initializer: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Box<Expression>,
    pub true_expression: Box<Expression>,
    pub false_expression: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub member: Box<Member>,
    pub initializer: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub caller: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub operator: UnaryOperator,
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ternary {
    pub condition: Box<Expression>,
    pub true_expression: Box<Expression>,
    pub false_expression: Box<Expression>,
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
    LogicalAnd,
    LogicalOr,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}
