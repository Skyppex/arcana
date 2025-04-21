use std::hash::Hash;
use std::{collections::HashMap, fmt::Display};

use crate::display::{Indent, IndentDisplay};
use crate::pretty_print::PrettyPrint;
use crate::type_checker::decision_tree::Pattern;
use crate::types::{GenericType, ToKey, TypeAnnotation, TypeIdentifier};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Program { statements: Vec<Statement> },
    ModuleDeclaration(ModuleDeclaration),
    Use(Use),
    StructDeclaration(StructDeclaration),
    EnumDeclaration(EnumDeclaration),
    UnionDeclaration(UnionDeclaration),
    TypeAliasDeclaration(TypeAliasDeclaration),
    ProtocolDeclaration(ProtocolDeclaration),
    ImplementationDeclaration(ImplementationDeclaration),
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
    Literal(ValueLiteral),
    Tuple(Vec<Expression>),
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
    #[cfg(feature = "interpreter")]
    Input(Box<Expression>),
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
    pub module_path: ModPath,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModPath {
    // NOTE: This MUST be the only field in the struct
    path: String,
}

impl ModPath {
    pub fn new(path: Vec<String>) -> Self {
        Self {
            path: path.join("::"),
        }
    }

    fn new_from_string(path: &String) -> &Self {
        unsafe { &*(path as *const String as *const ModPath) }
    }

    pub fn root() -> Self {
        Self {
            path: String::new(),
        }
    }

    pub fn join<M: AsRef<ModPath>>(&self, other: M) -> Self {
        let mut new_path = self.path.clone();

        if !new_path.is_empty() {
            new_path.push_str("::");
        }

        new_path.push_str(&other.as_ref().path);
        Self { path: new_path }
    }

    pub fn base(&self) -> String {
        self.path
            .split("::")
            .last()
            .unwrap_or(&self.path)
            .to_string()
    }

    pub fn components(&self) -> Vec<String> {
        self.path.split("::").map(|s| s.to_string()).collect()
    }
}

impl AsRef<ModPath> for ModPath {
    fn as_ref(&self) -> &ModPath {
        self
    }
}

impl AsRef<ModPath> for String {
    fn as_ref(&self) -> &ModPath {
        ModPath::new_from_string(self)
    }
}

impl Display for ModPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.path)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub body: StructData,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructData {
    pub type_identifier: TypeIdentifier,
    pub embedded_structs: Vec<EmbeddedStruct>,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EmbeddedStruct {
    pub type_annotation: TypeAnnotation,
    pub field_initializers: Vec<FieldInitializer>,
}

impl ToKey for EmbeddedStruct {
    fn to_key(&self) -> String {
        self.type_annotation.to_key()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub type_identifier: TypeIdentifier,
    pub shared_fields: Vec<StructField>,
    pub members: Vec<StructData>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub type_identifier: TypeIdentifier,
    pub literals: Vec<ValueLiteral>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAliasDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub type_identifier: TypeIdentifier,
    pub type_annotations: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProtocolDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub type_identifier: TypeIdentifier,
    pub associated_types: Vec<AssociatedType>,
    pub functions: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplementationDeclaration {
    pub scoped_generics: Vec<GenericType>,
    pub protocol_annotation: TypeAnnotation,
    pub type_annotation: TypeAnnotation,
    pub associated_types: Vec<AssociatedType>,
    pub functions: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssociatedType {
    pub type_identifier: TypeIdentifier,
    pub default_type_annotation: Option<TypeAnnotation>,
}

impl Display for AssociatedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_identifier)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub type_identifier: TypeIdentifier,
    pub param: Option<Parameter>,
    pub return_type_annotation: Option<TypeAnnotation>,
    pub body: Option<Expression>,
    pub signature_only: bool,
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
pub enum ValueLiteral {
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
        field_initializers: Vec<FieldInitializer>,
    },
}

impl Eq for ValueLiteral {}
impl Hash for ValueLiteral {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

impl Display for ValueLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueLiteral::Unit => write!(f, "#Unit"),
            ValueLiteral::Int(v) => write!(f, "#{}", v),
            ValueLiteral::UInt(v) => write!(f, "#{}", v),
            ValueLiteral::Float(v) => write!(f, "#{}", v),
            ValueLiteral::String(v) => write!(f, "#\"{}\"", v),
            ValueLiteral::Char(v) => write!(f, "#'{}'", v),
            ValueLiteral::Bool(v) => write!(f, "#{}", v),
            ValueLiteral::Array(array) => write!(
                f,
                "#[{}]",
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
            ValueLiteral::Struct { .. } => todo!(),
            ValueLiteral::Enum { .. } => todo!(),
        }
    }
}

impl ToKey for ValueLiteral {
    fn to_key(&self) -> String {
        match self {
            ValueLiteral::Unit => "#Unit".to_string(),
            ValueLiteral::Int(v) => format!("#Int:{}", v),
            ValueLiteral::UInt(v) => format!("#Uint:{}", v),
            ValueLiteral::Float(v) => format!("#Float:{}", v),
            ValueLiteral::String(v) => format!("#String:{}", v),
            ValueLiteral::Char(v) => format!("#Char:{}", v),
            ValueLiteral::Bool(v) => format!("#Bool:{}", v),
            ValueLiteral::Array(array) => format!(
                "#[{}]",
                array
                    .iter()
                    .map(|e| {
                        if let Expression::Literal(literal) = e {
                            literal.to_key()
                        } else {
                            panic!("Array element is not a literal")
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            ValueLiteral::Struct {
                type_annotation, ..
            } => type_annotation.to_key(),
            ValueLiteral::Enum {
                type_annotation,
                member,
                ..
            } => format!("{}::{}", type_annotation.to_key(), member),
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
    pub identifier: String,
    pub initializer: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumMember {
    pub identifier: String,
    pub embedded_structs: Vec<TypeAnnotation>,
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
        generics: Option<Vec<GenericType>>,
    },
    StaticMemberAccess {
        type_annotation: TypeAnnotation,
        member: Box<Member>,
        symbol: String,
        generics: Option<Vec<GenericType>>,
    },
    MemberAccess {
        object: Box<Expression>,
        member: Box<Member>,
        symbol: String,
        generics: Option<Vec<GenericType>>,
    },
    ParamPropagation {
        object: Box<Expression>,
        member: Box<Member>,
        symbol: String,
        generics: Option<Vec<GenericType>>,
    },
}

impl Member {
    pub fn get_symbol(&self) -> String {
        match self {
            Member::Identifier { symbol, .. } => symbol.clone(),
            Member::StaticMemberAccess { symbol, .. } => symbol.clone(),
            Member::MemberAccess { symbol, .. } => symbol.clone(),
            Member::ParamPropagation { symbol, .. } => symbol.clone(),
        }
    }

    pub fn with_generics(self, generics: Vec<GenericType>) -> Self {
        match self {
            Member::Identifier { symbol, .. } => Member::Identifier {
                symbol,
                generics: Some(generics),
            },
            Member::StaticMemberAccess {
                type_annotation,
                member,
                symbol,
                ..
            } => Member::StaticMemberAccess {
                type_annotation,
                member: Box::new(member.with_generics(generics.clone())),
                symbol,
                generics: Some(generics),
            },
            Member::MemberAccess {
                object,
                member,
                symbol,
                ..
            } => Member::MemberAccess {
                object,
                member: Box::new(member.with_generics(generics.clone())),
                symbol,
                generics: Some(generics),
            },
            Member::ParamPropagation {
                object,
                member,
                symbol,
                ..
            } => Member::ParamPropagation {
                object,
                member: Box::new(member.with_generics(generics.clone())),
                symbol,
                generics: Some(generics),
            },
        }
    }
}

impl Display for Member {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Member::Identifier { symbol, generics } => {
                if let Some(generics) = generics {
                    write!(
                        f,
                        "{}<{}>",
                        symbol,
                        generics
                            .iter()
                            .map(|g| g.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                } else {
                    write!(f, "{}", symbol)
                }
            }
            Member::StaticMemberAccess {
                type_annotation,
                member,
                ..
            } => write!(f, "{}::{}", type_annotation, member),
            Member::MemberAccess { member, .. } => {
                write!(f, ".{}", member)
            }
            Member::ParamPropagation { member, .. } => {
                write!(f, ":{}", member)
            }
        }
    }
}

impl ToKey for Member {
    fn to_key(&self) -> String {
        match self {
            Member::Identifier { symbol, generics } => {
                if let Some(generics) = generics {
                    format!("{}<{}>", symbol, generics.len())
                } else {
                    symbol.to_string()
                }
            }
            Member::StaticMemberAccess {
                type_annotation,
                member,
                ..
            } => format!("{}::{}", type_annotation.to_key(), member.to_key()),
            Member::MemberAccess { member, .. } => {
                format!(".{}", member.to_key())
            }
            Member::ParamPropagation { member, .. } => {
                format!(":{}", member.to_key())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub mutable: bool,
    pub type_annotation: Option<TypeAnnotation>,
    pub pattern: Pattern,
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
