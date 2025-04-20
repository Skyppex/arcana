use std::fmt::Display;
use std::hash::Hash;

use crate::display::{Indent, IndentDisplay};
use crate::parser::{AssociatedType, Expression, UseItem};
use crate::pretty_print::PrettyPrint;
use crate::types::{GenericType, ToKey};
use crate::{
    parser,
    types::{TypeAnnotation, TypeIdentifier},
};

use super::decision_tree::{Decision, Pattern};
use super::{LiteralType, Rcrc, Type, TypeEnvironment};

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
    ModuleDeclaration {
        access_modifier: Option<AccessModifier>,
        module_path: Vec<String>,
        type_: Type,
    },
    Use {
        use_item: UseItem,
        type_: Type,
    },
    StructDeclaration(StructData),
    EnumDeclaration {
        type_identifier: TypeIdentifier,
        shared_fields: Vec<StructField>,
        members: Vec<StructData>,
        type_: Type,
    },
    UnionDeclaration {
        type_identifier: TypeIdentifier,
        literals: Vec<TypeAnnotation>,
        type_: Type,
    },
    TypeAliasDeclaration {
        type_identifier: TypeIdentifier,
        type_annotations: Vec<TypeAnnotation>,
        type_: Type,
    },
    ProtocolDeclaration {
        type_identifier: TypeIdentifier,
        associated_types: Vec<AssociatedType>,
        functions: Vec<TypedStatement>,
        type_: Type,
    },
    ImplementationDeclaration {
        scoped_generics: Vec<GenericType>,
        protocol_annotation: TypeAnnotation,
        type_annotation: TypeAnnotation,
        associated_types: Vec<AssociatedType>,
        functions: Vec<(String, TypedStatement)>,
        type_: Type,
    },
    FunctionDeclaration {
        type_identifier: TypeIdentifier,
        param: Option<TypedParameter>,
        return_type: Type,
        body: Option<TypedExpression>,
        type_: Type,
    },
    Semi(Box<TypedStatement>),
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
            TypedStatement::ModuleDeclaration { type_, .. } => type_.clone(),
            TypedStatement::Use { type_, .. } => type_.clone(),
            TypedStatement::StructDeclaration(StructData { type_, .. }) => type_.clone(),
            TypedStatement::EnumDeclaration { type_, .. } => type_.clone(),
            TypedStatement::UnionDeclaration { type_, .. } => type_.clone(),
            TypedStatement::TypeAliasDeclaration { type_, .. } => type_.clone(),
            TypedStatement::ProtocolDeclaration { type_, .. } => type_.clone(),
            TypedStatement::ImplementationDeclaration { type_, .. } => type_.clone(),
            TypedStatement::FunctionDeclaration { type_, .. } => type_.clone(),
            TypedStatement::Semi { .. } => Type::Void,
            TypedStatement::Expression(e) => e.get_type(),
        }
    }

    fn get_deep_type(&self) -> Type {
        match self {
            TypedStatement::None => Type::Void,
            TypedStatement::Program { .. } => Type::Void,
            TypedStatement::ModuleDeclaration { type_, .. } => type_.clone(),
            TypedStatement::Use { type_, .. } => type_.clone(),
            TypedStatement::StructDeclaration(StructData { type_, .. }) => type_.clone(),
            TypedStatement::EnumDeclaration { type_, .. } => type_.clone(),
            TypedStatement::UnionDeclaration { type_, .. } => type_.clone(),
            TypedStatement::TypeAliasDeclaration { type_, .. } => type_.clone(),
            TypedStatement::ProtocolDeclaration { type_, .. } => type_.clone(),
            TypedStatement::ImplementationDeclaration { type_, .. } => type_.clone(),
            TypedStatement::FunctionDeclaration { type_, .. } => type_.clone(),
            TypedStatement::Semi(e) => e.get_deep_type(),
            TypedStatement::Expression(e) => e.get_deep_type(),
        }
    }
}

impl PrettyPrint for TypedStatement {
    fn prettify(&self) -> String {
        let mut indent = Indent::new();
        self.indent_display(&mut indent)
    }
}

impl Display for TypedStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedStatement::None => write!(f, "None"),
            TypedStatement::Program { statements } => write!(
                f,
                "{}",
                statements
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            TypedStatement::ModuleDeclaration { module_path, .. } => {
                write!(f, "mod {}", module_path.join("::"))
            }
            TypedStatement::Use { use_item, .. } => write!(f, "use {}", use_item),
            TypedStatement::StructDeclaration(StructData {
                type_identifier,
                fields,
                ..
            }) => write!(
                f,
                "struct {} {{{}}}",
                type_identifier,
                fields
                    .iter()
                    .map(|f| f.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            TypedStatement::EnumDeclaration {
                type_identifier,
                members,
                ..
            } => write!(
                f,
                "enum {} {{{}}}",
                type_identifier,
                members
                    .iter()
                    .map(|m| m.type_identifier.to_key())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            TypedStatement::UnionDeclaration {
                type_identifier,
                literals,
                ..
            } => write!(
                f,
                "union {} {{{}}}",
                type_identifier,
                literals
                    .iter()
                    .map(|l| l.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            TypedStatement::ProtocolDeclaration {
                type_identifier,
                associated_types,
                functions,
                type_: _,
            } => write!(
                f,
                "protocol {} {{ associated types: {}, functions: {} }}",
                type_identifier,
                associated_types
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                functions
                    .iter()
                    .map(|f| f.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            TypedStatement::ImplementationDeclaration {
                scoped_generics,
                protocol_annotation,
                type_annotation,
                associated_types,
                functions,
                type_: _,
                ..
            } => write!(
                f,
                "imp{} {} for {} {{ associated types: {}, functions: {} }}",
                format_args!(
                    "<{}>",
                    scoped_generics
                        .iter()
                        .map(|g| g.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                protocol_annotation,
                type_annotation,
                associated_types
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                functions
                    .iter()
                    .map(|f| f.clone().0)
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            TypedStatement::TypeAliasDeclaration {
                type_identifier,
                type_annotations,
                ..
            } => write!(
                f,
                "type {} = {}",
                type_identifier,
                type_annotations
                    .iter()
                    .map(|l| l.to_string())
                    .collect::<Vec<String>>()
                    .join(" | ")
            ),
            TypedStatement::FunctionDeclaration {
                type_identifier: identifier,
                param,
                return_type,
                body,
                ..
            } => match body {
                Some(body) => write!(
                    f,
                    "fn {}({}): {} => {}",
                    identifier,
                    if let Some(param) = param {
                        param.to_string()
                    } else {
                        "".to_string()
                    },
                    return_type,
                    body
                ),
                None => write!(
                    f,
                    "fn {}({}): {};",
                    identifier,
                    if let Some(param) = param {
                        param.to_string()
                    } else {
                        "".to_string()
                    },
                    return_type,
                ),
            },
            TypedStatement::Semi(s) => write!(f, "{};", s),
            TypedStatement::Expression(e) => write!(f, "{}", e),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedParameter {
    pub identifier: String,
    pub type_annotation: TypeAnnotation,
    pub type_: Box<Type>,
}

impl Typed for TypedParameter {
    fn get_type(&self) -> Type {
        *self.type_.clone()
    }

    fn get_deep_type(&self) -> Type {
        *self.type_.clone()
    }
}

impl Display for TypedParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.identifier, self.type_annotation)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedClosureParameter {
    pub identifier: String,
    pub type_annotation: Option<TypeAnnotation>,
    pub type_: Box<Type>,
}

impl Typed for TypedClosureParameter {
    fn get_type(&self) -> Type {
        *self.type_.clone()
    }

    fn get_deep_type(&self) -> Type {
        *self.type_.clone()
    }
}

impl Display for TypedClosureParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {}",
            self.identifier,
            self.type_annotation
                .clone()
                .unwrap_or(TypeAnnotation::Type("".to_string()))
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
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
        write!(
            f,
            "{{{}}}",
            self.statements
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedExpression {
    // None, // For testing purposes
    VariableDeclaration {
        mutable: bool,
        pattern: Pattern,
        initializer: Option<Box<TypedExpression>>,
        type_: Type,
    },
    If {
        condition: Box<TypedExpression>,
        true_expression: Box<TypedExpression>,
        false_expression: Option<Box<TypedExpression>>,
        type_: Type,
    },
    Match {
        expression: Box<TypedExpression>,
        arms: Vec<TypedMatchArm>,
        decision_tree: Decision,
        type_: Type,
    },
    Assignment {
        member: Box<Member>,
        initializer: Box<TypedExpression>,
        type_: Type,
    },
    Member(Member),
    Literal(ValueLiteral),
    Tuple {
        elements: Vec<TypedExpression>,
        type_: Type,
    },
    Closure {
        param: Option<TypedClosureParameter>,
        return_type: Type,
        body: Box<TypedExpression>,
        type_: Type,
    },
    Call {
        callee: Box<TypedExpression>,
        argument: Option<Box<TypedExpression>>,
        type_: Type,
    },
    Index {
        callee: Box<TypedExpression>,
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
    Block(Block),
    #[cfg(feature = "interpreter")]
    Print {
        value: Box<TypedExpression>,
    },
    #[cfg(feature = "interpreter")]
    Drop {
        identifier: String,
        type_: Type,
    },
    #[cfg(feature = "interpreter")]
    Input {
        value: Box<TypedExpression>,
    },
    Loop {
        body: Box<TypedExpression>,
        type_: Type,
    },
    While {
        condition: Box<TypedExpression>,
        body: Box<TypedExpression>,
        else_body: Option<Box<TypedExpression>>,
        type_: Type,
    },
    For {
        identifier: String,
        iterable: Box<TypedExpression>,
        body: Box<TypedExpression>,
        else_body: Option<Box<TypedExpression>>,
        type_: Type,
    },
    Break(Option<Box<TypedExpression>>),
    Continue,
    Return(Option<Box<TypedExpression>>),
}

impl Typed for TypedExpression {
    fn get_type(&self) -> Type {
        match self {
            // TypedExpression::None => Type::Void,
            TypedExpression::VariableDeclaration { type_, .. } => type_.clone(),
            TypedExpression::If { type_, .. } => type_.clone(),
            TypedExpression::Match { type_, .. } => type_.clone(),
            TypedExpression::Assignment { type_, .. } => type_.clone(),
            TypedExpression::Member(member) => member.get_type(),
            TypedExpression::Literal(literal) => literal.get_type(),
            TypedExpression::Tuple { type_, .. } => type_.clone(),
            TypedExpression::Closure { type_, .. } => type_.clone(),
            TypedExpression::Call { type_, .. } => type_.clone(),
            TypedExpression::Index { type_, .. } => type_.clone(),
            TypedExpression::Unary { type_, .. } => type_.clone(),
            TypedExpression::Binary { type_, .. } => type_.clone(),
            TypedExpression::Block(Block { type_, .. }) => type_.clone(),
            TypedExpression::Loop { type_, .. } => type_.clone(),
            TypedExpression::While { type_, .. } => type_.clone(),
            TypedExpression::For { type_, .. } => type_.clone(),
            TypedExpression::Print { .. } => Type::Void,
            TypedExpression::Drop { type_, .. } => type_.clone(),
            TypedExpression::Input { .. } => Type::String,
            TypedExpression::Break(_) => Type::Void,
            TypedExpression::Continue => Type::Void,
            TypedExpression::Return(_) => Type::Void,
        }
        .unsubstitute()
    }

    fn get_deep_type(&self) -> Type {
        match self {
            // TypedExpression::None => Type::Void,
            TypedExpression::VariableDeclaration { type_, .. } => type_.clone(),
            TypedExpression::If { type_, .. } => type_.clone(),
            TypedExpression::Match { type_, .. } => type_.clone(),
            TypedExpression::Assignment { type_, .. } => type_.clone(),
            TypedExpression::Member(member) => member.get_deep_type(),
            TypedExpression::Literal(literal) => literal.get_deep_type(),
            TypedExpression::Tuple { type_, .. } => type_.clone(),
            TypedExpression::Closure { type_, .. } => type_.clone(),
            TypedExpression::Call { type_, .. } => type_.clone(),
            TypedExpression::Index { type_, .. } => type_.clone(),
            TypedExpression::Unary { type_, .. } => type_.clone(),
            TypedExpression::Binary { type_, .. } => type_.clone(),
            TypedExpression::Block(Block { type_, .. }) => type_.clone(),
            TypedExpression::Loop { type_, .. } => type_.clone(),
            TypedExpression::While { type_, .. } => type_.clone(),
            TypedExpression::For { type_, .. } => type_.clone(),
            TypedExpression::Print { .. } => Type::Void,
            TypedExpression::Drop { type_, .. } => type_.clone(),
            TypedExpression::Input { .. } => Type::String,
            TypedExpression::Break(_) => Type::Void,
            TypedExpression::Continue => Type::Void,
            TypedExpression::Return(_) => Type::Void,
        }
        .unsubstitute()
    }
}

impl PrettyPrint for TypedExpression {
    fn prettify(&self) -> String {
        let mut indent = Indent::new();
        self.indent_display(&mut indent)
    }
}

impl Display for TypedExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // TypedExpression::None => write!(f, "None"),
            TypedExpression::VariableDeclaration {
                mutable,
                pattern,
                initializer,
                ..
            } => {
                if let Some(initializer) = initializer {
                    write!(
                        f,
                        "{}{} = {}",
                        if *mutable { "mut " } else { "" },
                        pattern,
                        initializer
                    )
                } else {
                    write!(f, "{}{}", if *mutable { "mut " } else { "" }, pattern)
                }
            }
            TypedExpression::If {
                condition,
                true_expression,
                false_expression,
                ..
            } => {
                write!(f, "if {} {{ {} }}", condition, true_expression)?;
                if let Some(false_expression) = false_expression {
                    write!(f, " else {{ {} }}", false_expression)?;
                }
                Ok(())
            }
            TypedExpression::Match {
                expression, arms, ..
            } => write!(
                f,
                "match {} {}",
                expression,
                arms.iter()
                    .map(|a| "|>".to_string() + &a.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            TypedExpression::Assignment {
                member,
                initializer,
                ..
            } => write!(f, "{} = {}", member, initializer),
            TypedExpression::Member(member) => write!(f, "{}", member),
            TypedExpression::Literal(literal) => write!(f, "{}", literal),
            TypedExpression::Tuple { elements, .. } => {
                write!(
                    f,
                    "({})",
                    elements
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            TypedExpression::Closure {
                param,
                return_type,
                body,
                type_,
            } => {
                write!(
                    f,
                    "fn {}({}) -> {} {{ {} }}",
                    if let Some(param) = param {
                        param.to_string()
                    } else {
                        "".to_string()
                    },
                    return_type,
                    body,
                    type_
                )
            }
            TypedExpression::Call {
                callee, argument, ..
            } => write!(
                f,
                "{}({})",
                callee,
                argument.clone().map_or("".to_string(), |a| a.to_string())
            ),
            TypedExpression::Index {
                callee, argument, ..
            } => write!(f, "{}[{}]", callee, argument),
            TypedExpression::Unary {
                operator,
                expression,
                ..
            } => write!(f, "{}{}", operator, expression),
            TypedExpression::Binary {
                left,
                operator,
                right,
                ..
            } => write!(f, "{} {} {}", left, operator, right),
            TypedExpression::Block(block) => write!(f, "{}", block),
            TypedExpression::Drop { identifier, .. } => write!(f, "drop {}", identifier),
            TypedExpression::Print { value, .. } => write!(f, "print {}", value),
            TypedExpression::Input { value, .. } => write!(f, "input {}", value),
            TypedExpression::Loop { body, .. } => write!(f, "loop {}", body),
            TypedExpression::While {
                condition,
                body,
                else_body,
                ..
            } => {
                write!(f, "while {} {}", condition, body)?;
                if let Some(else_body) = else_body {
                    write!(f, " else {}", else_body)?;
                }
                Ok(())
            }
            TypedExpression::For {
                identifier,
                iterable,
                body,
                else_body,
                ..
            } => {
                write!(f, "for {} in {} {}", identifier, iterable, body)?;
                if let Some(else_body) = else_body {
                    write!(f, " else {}", else_body)?;
                }
                Ok(())
            }
            TypedExpression::Break(e) => write!(
                f,
                "break{}",
                if let Some(e) = e {
                    format!(" {}", e)
                } else {
                    "".to_string()
                }
            ),
            TypedExpression::Continue => write!(f, "continue"),
            TypedExpression::Return(e) => write!(
                f,
                "return{}",
                if let Some(e) = e {
                    format!(" {}", e)
                } else {
                    "".to_string()
                }
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EmbeddedStruct {
    pub type_annotation: TypeAnnotation,
    pub field_initializers: Vec<FieldInitializer>,
    pub type_: Type,
}

impl ToKey for EmbeddedStruct {
    fn to_key(&self) -> String {
        self.type_annotation.to_key()
    }
}

impl Display for EmbeddedStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {{{}}}",
            self.type_annotation,
            self.field_initializers
                .iter()
                .map(|f| f.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub struct_identifier: TypeIdentifier,
    pub mutable: bool,
    pub identifier: String,
    pub default_value: Option<TypedExpression>,
    pub type_: Type,
}

impl Display for StructField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}: {}",
            if self.mutable { "mut " } else { "" },
            self.identifier,
            self.type_
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructData {
    pub type_identifier: TypeIdentifier,
    pub embedded_structs: Vec<EmbeddedStruct>,
    pub fields: Vec<StructField>,
    pub type_: Type,
}

impl Display for StructData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {{{}}}",
            self.type_identifier,
            self.fields
                .iter()
                .map(|f| f.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AccessModifier {
    Public,
    Module,
    Super,
}

impl From<parser::AccessModifier> for AccessModifier {
    fn from(val: parser::AccessModifier) -> Self {
        match val {
            parser::AccessModifier::Public => AccessModifier::Public,
            parser::AccessModifier::Super => AccessModifier::Super,
            parser::AccessModifier::Module => AccessModifier::Module,
        }
    }
}

// #[derive(Debug, Clone, PartialEq)]
// pub struct EnumMember {
//     pub enum_name: TypeIdentifier,
//     pub discriminant_name: String,
//     pub embedded_structs: Vec<Type>,
//     pub fields: Vec<EnumMemberField>,
//     pub type_: Type,
// }

// impl Display for EnumMember {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "{} {{{}}}",
//             self.enum_name,
//             self.fields
//                 .iter()
//                 .map(|f| f.to_string())
//                 .collect::<Vec<String>>()
//                 .join(", ")
//         )
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct EnumMemberField {
//     pub enum_name: TypeIdentifier,
//     pub discriminant_name: String,
//     pub identifier: String,
//     pub type_: Type,
// }

// impl Display for EnumMemberField {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}: {}", self.identifier, self.type_)
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub enum ValueLiteral {
    Void,
    Unit,
    Int(i64),
    UInt(u64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Array {
        values: Vec<TypedExpression>,
        type_: Type,
    },
    Struct {
        type_annotation: TypeAnnotation,
        field_initializers: Vec<FieldInitializer>,
        type_: Type,
    },
    Enum {
        type_annotation: TypeAnnotation,
        member: String,
        field_initializers: Vec<FieldInitializer>,
        type_: Type,
    },
}

impl Eq for ValueLiteral {}
impl Hash for ValueLiteral {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

impl Typed for ValueLiteral {
    fn get_type(&self) -> Type {
        match self {
            ValueLiteral::Void => Type::Void,
            ValueLiteral::Unit => Type::Unit,
            ValueLiteral::Int(v) => Type::Literal {
                name: v.to_string(),
                type_: Box::new(LiteralType::IntValue(*v)),
            },
            ValueLiteral::UInt(v) => Type::Literal {
                name: v.to_string(),
                type_: Box::new(LiteralType::UIntValue(*v)),
            },
            ValueLiteral::Float(v) => Type::Literal {
                name: v.to_string(),
                type_: Box::new(LiteralType::FloatValue(*v)),
            },
            ValueLiteral::String(v) => Type::Literal {
                name: format!("\"{}\"", v),
                type_: Box::new(LiteralType::StringValue(v.clone())),
            },
            ValueLiteral::Char(v) => Type::Literal {
                name: format!("'{}'", v),
                type_: Box::new(LiteralType::CharValue(v.to_string())),
            },
            ValueLiteral::Bool(v) => Type::Literal {
                name: v.to_string(),
                type_: Box::new(LiteralType::BoolValue(*v)),
            },
            ValueLiteral::Array { type_, .. } => Type::Array(Box::new(type_.clone())),
            ValueLiteral::Struct { type_, .. } => type_.clone(),
            ValueLiteral::Enum { type_, .. } => type_.clone(),
        }
    }

    fn get_deep_type(&self) -> Type {
        match self {
            ValueLiteral::Void => Type::Void,
            ValueLiteral::Unit => Type::Unit,
            ValueLiteral::Int(_) => Type::Int,
            ValueLiteral::UInt(_) => Type::UInt,
            ValueLiteral::Float(_) => Type::Float,
            ValueLiteral::String(_) => Type::String,
            ValueLiteral::Char(_) => Type::Char,
            ValueLiteral::Bool(_) => Type::Bool,
            ValueLiteral::Array { type_, .. } => type_.clone(),
            ValueLiteral::Struct { type_, .. } => type_.clone(),
            ValueLiteral::Enum { type_, .. } => type_.clone(),
        }
    }
}

impl TryFrom<Type> for ValueLiteral {
    type Error = String;

    fn try_from(type_: Type) -> Result<Self, Self::Error> {
        let Type::Literal { type_, .. } = type_ else {
            return Err("Invalid type for Literal".into());
        };

        match *type_ {
            LiteralType::IntValue(value) => Ok(ValueLiteral::Int(value)),
            LiteralType::UIntValue(value) => Ok(ValueLiteral::UInt(value)),
            LiteralType::FloatValue(value) => Ok(ValueLiteral::Float(value)),
            LiteralType::StringValue(value) => Ok(ValueLiteral::String(value)),
            LiteralType::CharValue(value) => Ok(ValueLiteral::Char(
                value.parse().map_err(|_| "Invalid char literal")?,
            )),
            LiteralType::BoolValue(value) => Ok(ValueLiteral::Bool(value)),
            _ => Err("Invalid type for Literal".into()),
        }
    }
}

impl Display for ValueLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueLiteral::Void => write!(f, "#Void"),
            ValueLiteral::Unit => write!(f, "#Unit"),
            ValueLiteral::Int(v) => write!(f, "#{}", v),
            ValueLiteral::UInt(v) => write!(f, "#{}", v),
            ValueLiteral::Float(v) => write!(f, "#{}", v),
            ValueLiteral::String(v) => write!(f, "#\"{}\"", v),
            ValueLiteral::Char(v) => write!(f, "#'{}'", v),
            ValueLiteral::Bool(v) => write!(f, "#{}", v),
            ValueLiteral::Array { values, .. } => write!(
                f,
                "#[{}]",
                values
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            ValueLiteral::Struct {
                field_initializers, ..
            } => write!(
                f,
                "{{{}}}",
                field_initializers
                    .iter()
                    .map(|fi| { format!("{}: {}", fi.identifier, fi.initializer) })
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            ValueLiteral::Enum {
                field_initializers, ..
            } => write!(
                f,
                "{}",
                field_initializers
                    .iter()
                    .map(|fi| fi.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldInitializer {
    pub identifier: String,
    pub initializer: TypedExpression,
}

impl Display for FieldInitializer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.identifier, self.initializer)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Member {
    Identifier {
        symbol: String,
        type_: Type,
    },
    StaticMemberAccess {
        type_annotation: TypeAnnotation,
        member: Box<Member>,
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
            Member::StaticMemberAccess { symbol, .. } => symbol,
            Member::MemberAccess { symbol, .. } => symbol,
            // Member::MemberFunctionAccess { symbol, .. } => symbol,
        }
    }
}

impl Typed for Member {
    fn get_type(&self) -> Type {
        match self {
            Member::Identifier { type_, .. } => type_.clone(),
            Member::StaticMemberAccess { type_, .. } => type_.clone(),
            Member::MemberAccess { type_, .. } => type_.clone(),
            // Member::MemberFunctionAccess { type_, .. } => type_.clone(),
        }
    }

    fn get_deep_type(&self) -> Type {
        match self {
            Member::Identifier { type_, .. } => type_.clone(),
            Member::StaticMemberAccess { member, .. } => member.get_deep_type(),
            Member::MemberAccess { member, .. } => member.get_deep_type(),
            // Member::MemberFunctionAccess { member, .. } => member.get_deep_type(),
        }
    }
}

impl Display for Member {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Member::Identifier { symbol, .. } => write!(f, "{}", symbol),
            Member::StaticMemberAccess {
                type_annotation,
                member,
                ..
            } => write!(f, "{}.{}", type_annotation, member),
            Member::MemberAccess { object, member, .. } => write!(f, "{}.{}", object, member),
            // Member::MemberFunctionAccess { object, member, .. } => {
            //     write!(f, "{}.{}", object, member)
            // }
        }
    }
}

impl ToKey for Member {
    fn to_key(&self) -> String {
        self.get_type().to_key()
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

impl From<parser::BinaryOperator> for BinaryOperator {
    fn from(value: parser::BinaryOperator) -> Self {
        match value {
            parser::BinaryOperator::Add => BinaryOperator::Add,
            parser::BinaryOperator::Subtract => BinaryOperator::Subtract,
            parser::BinaryOperator::Multiply => BinaryOperator::Multiply,
            parser::BinaryOperator::Divide => BinaryOperator::Divide,
            parser::BinaryOperator::Modulo => BinaryOperator::Modulo,
            parser::BinaryOperator::BitwiseAnd => BinaryOperator::BitwiseAnd,
            parser::BinaryOperator::BitwiseOr => BinaryOperator::BitwiseOr,
            parser::BinaryOperator::BitwiseXor => BinaryOperator::BitwiseXor,
            parser::BinaryOperator::BitwiseLeftShift => BinaryOperator::BitwiseLeftShift,
            parser::BinaryOperator::BitwiseRightShift => BinaryOperator::BitwiseRightShift,
            parser::BinaryOperator::LogicalAnd => BinaryOperator::LogicalAnd,
            parser::BinaryOperator::LogicalOr => BinaryOperator::LogicalOr,
            parser::BinaryOperator::Equal => BinaryOperator::Equal,
            parser::BinaryOperator::NotEqual => BinaryOperator::NotEqual,
            parser::BinaryOperator::LessThan => BinaryOperator::LessThan,
            parser::BinaryOperator::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
            parser::BinaryOperator::GreaterThan => BinaryOperator::GreaterThan,
            parser::BinaryOperator::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
            parser::BinaryOperator::Range => BinaryOperator::Range,
            parser::BinaryOperator::RangeInclusive => BinaryOperator::RangeInclusive,
        }
    }
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
            BinaryOperator::LogicalAnd => write!(f, "&&"),
            BinaryOperator::LogicalOr => write!(f, "||"),
            BinaryOperator::Equal => write!(f, "=="),
            BinaryOperator::NotEqual => write!(f, "!="),
            BinaryOperator::LessThan => write!(f, "<"),
            BinaryOperator::LessThanOrEqual => write!(f, "<="),
            BinaryOperator::GreaterThan => write!(f, ">"),
            BinaryOperator::GreaterThanOrEqual => write!(f, ">="),
            BinaryOperator::Range => write!(f, ".."),
            BinaryOperator::RangeInclusive => write!(f, "..="),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedMatchArm {
    pub pattern: Pattern,
    pub expression: Expression,
    pub type_environment: Rcrc<TypeEnvironment>,
}

impl Display for TypedMatchArm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} => {:?}", self.pattern, self.expression)
    }
}
