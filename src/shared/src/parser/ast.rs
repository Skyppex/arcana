use std::hash::Hash;
use std::{collections::HashMap, fmt::Display};

use crate::display::{Indent, IndentDisplay};
use crate::pretty_print::PrettyPrint;
use crate::type_checker::decision_tree::Pattern;
use crate::types::{GenericConstraint, TypeAnnotation, TypeIdentifier};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Program { statements: Vec<Statement> },
    ModuleDeclaration(ModuleDeclaration),
    Use(Use),
    StructDeclaration(StructDeclaration),
    EnumDeclaration(EnumDeclaration),
    UnionDeclaration(UnionDeclaration),
    TypeAliasDeclaration(TypeAliasDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    Semi(Box<Statement>),
    Expression(Expression),
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
    // None, // For testing purposes
    VariableDeclaration(VariableDeclaration),
    If(If),
    Match(Match),
    Assignment(Assignment),
    Member(Member),
    Literal(Literal),
    Closure(Closure),
    Call(Call),
    Unary(Unary),
    Binary(Binary),
    Block(Block),
    Loop(Box<Expression>),
    While(While),
    For(For),

    Break(Option<Box<Expression>>),
    Continue,
    Return(Option<Box<Expression>>),

    #[cfg(feature = "interpreter")]
    Print(Box<Expression>),
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
pub struct ModuleDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub module_path: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Use {
    pub use_item: UseItem,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UseItem {
    Item(String),
    Navigation(String, Box<UseItem>),
    List(Vec<UseItem>),
}

impl Display for UseItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UseItem::Item(item) => write!(f, "{}", item),
            UseItem::Navigation(item, next) => write!(f, "{}::{}", item, next),
            UseItem::List(items) => write!(
                f,
                "{{{}}}",
                items
                    .iter()
                    .map(|item| item.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
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
    pub shared_fields: Vec<StructField>,
    pub members: Vec<EnumMember>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub type_identifier: TypeIdentifier,
    pub literals: Vec<Literal>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAliasDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub type_identifier: TypeIdentifier,
    pub type_annotations: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub identifier: TypeIdentifier,
    pub param: Option<Parameter>,
    pub return_type_annotation: Option<TypeAnnotation>,
    pub body: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub identifier: String,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureParameter {
    pub identifier: String,
    pub type_annotation: Option<TypeAnnotation>,
}

impl From<Parameter> for ClosureParameter {
    fn from(param: Parameter) -> Self {
        Self {
            identifier: param.identifier,
            type_annotation: Some(param.type_annotation),
        }
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
            Literal::Struct { .. } => todo!(),
            Literal::Enum { .. } => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub condition: Box<Expression>,
    pub body: Box<Expression>,
    pub else_body: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct For {
    pub identifier: String,
    pub iterable: Box<Expression>,
    pub body: Box<Expression>,
    pub else_body: Option<Box<Expression>>,
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
    Super,
    Module,
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
    ParamPropagation {
        object: Box<Expression>,
        member: Box<Member>,
        symbol: String,
    },
}

impl Member {
    pub fn get_symbol(&self) -> String {
        match self {
            Member::Identifier { symbol } => symbol.clone(),
            Member::MemberAccess { symbol, .. } => symbol.clone(),
            Member::ParamPropagation { symbol, .. } => symbol.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub mutable: bool,
    pub type_annotation: Option<TypeAnnotation>,
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
pub struct Match {
    pub expression: Box<Expression>,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub member: Box<Member>,
    pub initializer: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub param: Option<ClosureParameter>,
    pub return_type_annotation: Option<TypeAnnotation>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Box<Expression>,
    pub argument: Option<Box<Expression>>,
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
    Range,
    RangeInclusive,
}
