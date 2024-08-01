pub mod ast;
pub mod decision_tree;
pub mod full_name;
pub mod type_checker;
pub mod type_environment;

mod expressions;
mod scope;
mod statements;

pub use full_name::*;
pub use type_checker::*;
pub use type_environment::*;

use std::{any::TypeId, cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    parser,
    types::{GenericType, TypeAnnotation, TypeIdentifier},
};

use self::statements::check_type_annotation;

#[derive(Debug, Clone)]
pub struct Struct {
    pub type_identifier: TypeIdentifier,
    pub fields: HashMap<String, Type>,
}

impl Struct {
    pub fn type_annotation(&self) -> TypeAnnotation {
        TypeAnnotation::from(self.type_identifier.clone())
    }
}

impl FullName for Struct {
    fn full_name(&self) -> String {
        self.type_identifier.to_string()
    }
}

impl PartialEq for Struct {
    fn eq(&self, other: &Self) -> bool {
        self.type_identifier == other.type_identifier && self.fields == other.fields
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub struct_name: TypeIdentifier,
    pub field_name: String,
    pub field_type: Box<Type>,
}

impl FullName for StructField {
    fn full_name(&self) -> String {
        format!(
            "{}.{}: {}",
            self.struct_name, self.field_name, self.field_type
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub type_identifier: TypeIdentifier,
    pub shared_fields: HashMap<String, Type>,
    pub members: HashMap<String, Type>,
}

impl Enum {
    pub fn type_annotation(&self) -> TypeAnnotation {
        TypeAnnotation::from(self.type_identifier.clone())
    }
}

impl FullName for Enum {
    fn full_name(&self) -> String {
        self.type_identifier.to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumMember {
    pub enum_name: TypeIdentifier,
    pub discriminant_name: String,
    pub fields: HashMap<String, Type>,
}

impl EnumMember {
    pub fn type_annotation(&self) -> TypeAnnotation {
        TypeAnnotation::from(self.enum_name.clone())
    }
}

impl FullName for EnumMember {
    fn full_name(&self) -> String {
        format!(
            "{}::{} {{{}}}",
            self.enum_name,
            self.discriminant_name,
            self.fields
                .iter()
                .map(|(name, type_)| format!("{}: {}", name, type_.full_name()))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumMemberField {
    pub enum_name: TypeIdentifier,
    pub discriminant_name: String,
    pub field_name: String,
    pub field_type: Box<Type>,
}

impl FullName for EnumMemberField {
    fn full_name(&self) -> String {
        format!(
            "{}::{}.{}",
            self.enum_name, self.discriminant_name, self.field_name
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Union {
    pub type_identifier: TypeIdentifier,
    pub literal_type: Box<Type>,
    pub literals: Vec<Type>,
}

impl Union {
    pub fn type_annotation(&self) -> TypeAnnotation {
        TypeAnnotation::from(self.type_identifier.clone())
    }
}

impl FullName for Union {
    fn full_name(&self) -> String {
        format!(
            "{} {{ {} }}",
            self.type_identifier.to_string(),
            self.literals
                .iter()
                .map(|l| l.to_string())
                .collect::<Vec<String>>()
                .join(" | ")
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Trait {
    pub type_identifier: TypeIdentifier,
    pub associated_types: HashMap<String, Type>,
    pub functions: Vec<TypeIdentifier>,
}

impl FullName for Trait {
    fn full_name(&self) -> String {
        self.type_identifier.to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub identifier: Option<TypeIdentifier>,
    pub param: Option<Parameter>,
    pub return_type: Box<Type>,
}

impl Function {
    pub fn type_annotation(&self) -> Option<TypeAnnotation> {
        self.identifier
            .clone()
            .map(|id| TypeAnnotation::from(id.clone()))
            .or_else(|| {
                Some(TypeAnnotation::Function(
                    self.param
                        .clone()
                        .map(|p| Box::new(p.type_.type_annotation())),
                    Some(Box::new(self.return_type.type_annotation())),
                ))
            })
    }
}

impl FullName for Function {
    fn full_name(&self) -> String {
        format!(
            "fun({}): {}",
            self.param
                .clone()
                .map(|p| p.type_.full_name())
                .unwrap_or_else(|| "".to_string()),
            self.return_type.full_name()
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub identifier: String,
    pub type_: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unknown,
    Generic(GenericType),
    Void,
    Unit,
    Int,
    UInt,
    Float,
    String,
    Char,
    Bool,
    Array(Box<Type>),
    Struct(Struct),
    Enum(Enum),
    EnumMember(EnumMember),
    Union(Union),
    Trait(Trait),
    Function(Function),
    Literal {
        name: String, // String representation of the literal
        type_: Box<Type>,
    },
}

impl Type {
    pub fn option() -> Type {
        let option_name = "Option".to_string();

        let option_ident = TypeIdentifier::GenericType(
            option_name,
            vec![GenericType {
                type_name: "T".to_string(),
            }],
        );

        let some_member_ident =
            TypeIdentifier::MemberType(Box::new(option_ident.clone()), "Some".to_string());

        let none_member_ident =
            TypeIdentifier::MemberType(Box::new(option_ident.clone()), "None".to_string());

        Type::Enum(Enum {
            type_identifier: option_ident,
            shared_fields: HashMap::new(),
            members: vec![
                (
                    "Some".to_string(),
                    Type::EnumMember(EnumMember {
                        enum_name: some_member_ident,
                        discriminant_name: "Some".to_string(),
                        fields: vec![(
                            "f0".to_string(),
                            Type::Generic(GenericType {
                                type_name: "T".to_string(),
                            }),
                        )]
                        .into_iter()
                        .collect(),
                    }),
                ),
                (
                    "None".to_string(),
                    Type::EnumMember(EnumMember {
                        enum_name: none_member_ident,
                        discriminant_name: "None".to_string(),
                        fields: HashMap::new(),
                    }),
                ),
            ]
            .into_iter()
            .collect(),
        })
    }

    pub fn option_of(concrete: Type) -> Type {
        Type::Enum(Enum {
            type_identifier: TypeIdentifier::ConcreteType(
                "Option".to_string(),
                vec![concrete.type_annotation()],
            ),
            shared_fields: HashMap::new(),
            members: vec![
                (
                    "Some".to_string(),
                    Type::EnumMember(EnumMember {
                        enum_name: TypeIdentifier::ConcreteType(
                            "Option".to_string(),
                            vec![concrete.type_annotation()],
                        ),
                        discriminant_name: "Some".to_string(),
                        fields: vec![("f0".to_string(), concrete.clone())]
                            .into_iter()
                            .collect(),
                    }),
                ),
                (
                    "None".to_string(),
                    Type::EnumMember(EnumMember {
                        enum_name: TypeIdentifier::ConcreteType(
                            "Option".to_string(),
                            vec![concrete.type_annotation()],
                        ),
                        discriminant_name: "None".to_string(),
                        fields: HashMap::new(),
                    }),
                ),
            ]
            .into_iter()
            .collect(),
        })
    }

    pub fn from_string(type_name: &str) -> Option<Type> {
        match type_name {
            "void" => Some(Type::Void),
            "unit" => Some(Type::Unit),
            "int" => Some(Type::Int),
            "uint" => Some(Type::UInt),
            "float" => Some(Type::Float),
            "string" => Some(Type::String),
            "char" => Some(Type::Char),
            "bool" => Some(Type::Bool),
            _ => None,
        }
    }

    pub fn to_string(&self) -> String {
        self.full_name()
    }

    pub fn from_literal(literal: &parser::Literal) -> Result<Type, String> {
        match literal {
            parser::Literal::Unit => Ok(Type::Literal {
                name: Type::Unit.to_string(),
                type_: Box::new(Type::Unit),
            }),
            parser::Literal::Int(v) => Ok(Type::Literal {
                name: v.to_string(),
                type_: Box::new(Type::Int),
            }),
            parser::Literal::UInt(v) => Ok(Type::Literal {
                name: v.to_string(),
                type_: Box::new(Type::UInt),
            }),
            parser::Literal::Float(v) => Ok(Type::Literal {
                name: v.to_string(),
                type_: Box::new(Type::Float),
            }),
            parser::Literal::Char(v) => Ok(Type::Literal {
                name: format!("'{}'", v),
                type_: Box::new(Type::Char),
            }),
            parser::Literal::String(v) => Ok(Type::Literal {
                name: format!("\"{}\"", v),
                type_: Box::new(Type::String),
            }),
            parser::Literal::Bool(v) => Ok(Type::Literal {
                name: v.to_string(),
                type_: Box::new(Type::Bool),
            }),
            _ => Err(format!("Cannot convert literal {:?} to type", literal)),
        }
    }

    pub fn type_identifier(&self) -> TypeIdentifier {
        match self {
            Type::Generic(name) => TypeIdentifier::Type(name.type_name.clone()),
            Type::Void => TypeIdentifier::Type("void".to_string()),
            Type::Unit => TypeIdentifier::Type("unit".to_string()),
            Type::Int => TypeIdentifier::Type("int".to_string()),
            Type::UInt => TypeIdentifier::Type("uint".to_string()),
            Type::Float => TypeIdentifier::Type("float".to_string()),
            Type::String => TypeIdentifier::Type("string".to_string()),
            Type::Char => TypeIdentifier::Type("char".to_string()),
            Type::Bool => TypeIdentifier::Type("bool".to_string()),
            Type::Struct(s) => s.type_identifier.clone(),
            Type::Enum(u) => u.type_identifier.clone(),
            Type::EnumMember(um) => TypeIdentifier::MemberType(
                Box::new(um.enum_name.clone()),
                um.discriminant_name.clone(),
            ),
            Type::Union(u) => u.type_identifier.clone(),
            Type::Function(f) => f
                .identifier
                .clone()
                .unwrap_or_else(|| panic!("Closure has no identifier")),
            _ => panic!("Cannot get type identifier for type {}", self.full_name()),
        }
    }

    pub fn type_annotation(&self) -> TypeAnnotation {
        match self {
            Type::Generic(name) => TypeAnnotation::Type(name.type_name.clone()),
            Type::Void => TypeAnnotation::Type("void".to_string()),
            Type::Unit => TypeAnnotation::Type("unit".to_string()),
            Type::Int => TypeAnnotation::Type("int".to_string()),
            Type::UInt => TypeAnnotation::Type("uint".to_string()),
            Type::Float => TypeAnnotation::Type("float".to_string()),
            Type::String => TypeAnnotation::Type("string".to_string()),
            Type::Char => TypeAnnotation::Type("char".to_string()),
            Type::Bool => TypeAnnotation::Type("bool".to_string()),
            Type::Array(type_) => TypeAnnotation::Array(Box::new(type_.type_annotation())),
            Type::Struct(s) => s.type_annotation(),
            Type::Enum(e) => e.type_annotation(),
            Type::EnumMember(em) => em.type_annotation(),
            Type::Union(u) => u.type_annotation(),
            Type::Function(f) => f
                .type_annotation()
                .unwrap_or_else(|| panic!("Closure has no type annotation")),
            Type::Literal { type_, .. } => {
                TypeAnnotation::Literal(Box::new((*type_.clone()).into()))
            }
            _ => panic!("Cannot get type annotation for type {}", self.full_name()),
        }
    }

    pub fn clone_with_concrete_types(
        &self,
        concrete_types: Vec<TypeAnnotation>,
        type_environment: Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Type, String> {
        match self {
            Type::Struct(s) => {
                let TypeIdentifier::GenericType(name, generics) = s.type_identifier.clone() else {
                    return Err(format!(
                        "Cannot clone concrete types for struct {}",
                        self.full_name()
                    ));
                };

                let mut type_map = HashMap::new();

                for (ta, gt) in concrete_types.iter().zip(generics) {
                    type_map.insert(gt, ta);
                }

                let mut fields = HashMap::new();

                for (field_name, field_type) in s.fields.iter() {
                    let Type::Generic(generic) = field_type.clone() else {
                        fields.insert(field_name.clone(), field_type.clone());
                        continue;
                    };

                    let concrete_type = type_map.get(&generic).ok_or(format!(
                        "No concrete type found for generic type {}",
                        generic.type_name
                    ))?;

                    let concrete_type =
                        check_type_annotation(&concrete_type, &vec![], type_environment.clone())?;

                    fields.insert(field_name.clone(), concrete_type);
                }

                Ok(Type::Struct(Struct {
                    type_identifier: TypeIdentifier::ConcreteType(name, concrete_types),
                    fields,
                }))
            }
            Type::Enum(u) => {
                let TypeIdentifier::GenericType(name, generics) = u.type_identifier.clone() else {
                    return Err(format!(
                        "Cannot clone concrete types for enum {}",
                        self.full_name()
                    ));
                };

                let mut type_map = HashMap::new();

                for (ta, gt) in concrete_types.iter().zip(generics) {
                    type_map.insert(gt, ta);
                }

                let type_identifier =
                    TypeIdentifier::ConcreteType(name.clone(), concrete_types.clone());

                let mut shared_fields = HashMap::new();

                for (field_name, field_type) in u.shared_fields.iter() {
                    let Type::Generic(generic) = field_type.clone() else {
                        shared_fields.insert(field_name.clone(), field_type.clone());
                        continue;
                    };

                    let concrete_type = type_map.get(&generic).ok_or(format!(
                        "No concrete type found for generic type {}",
                        generic.type_name
                    ))?;

                    let concrete_type =
                        check_type_annotation(&concrete_type, &vec![], type_environment.clone())?;

                    shared_fields.insert(field_name.clone(), concrete_type);
                }

                let mut members = HashMap::new();

                for (member_name, type_) in u.members.iter() {
                    let Type::EnumMember(u) = type_ else {
                        return Err(format!(
                            "Enum members are not of type EnumMember {}",
                            type_.full_name()
                        ));
                    };

                    let mut fields = HashMap::new();

                    for (field_name, field_type) in u.fields.iter() {
                        let Type::Generic(generic) = field_type else {
                            fields.insert(field_name.clone(), field_type.clone());
                            continue;
                        };

                        let concrete_type = type_map.get(&generic).ok_or(format!(
                            "No concrete type found for generic type {}",
                            generic.type_name
                        ))?;

                        let concrete_type = check_type_annotation(
                            &concrete_type,
                            &vec![],
                            type_environment.clone(),
                        )?;

                        fields.insert(field_name.clone(), concrete_type);
                    }

                    let member_type = Type::EnumMember(EnumMember {
                        enum_name: TypeIdentifier::ConcreteType(
                            name.clone(),
                            concrete_types.clone(),
                        ),
                        discriminant_name: member_name.clone(),
                        fields,
                    });

                    members.insert(member_name.clone(), member_type);
                }

                let enum_ = Type::Enum(Enum {
                    type_identifier,
                    shared_fields,
                    members,
                });

                Ok(enum_)
            }
            _ => Err(format!(
                "Cannot clone concrete types for type {}",
                self.full_name()
            )),
        }
    }
}

impl FullName for Type {
    fn full_name(&self) -> String {
        match self {
            Type::Unknown => "{unknown}".to_string(),
            Type::Generic(GenericType { type_name }) => type_name.to_string(),
            Type::Void => "void".to_string(),
            Type::Unit => "unit".to_string(),
            Type::Int => "int".to_string(),
            Type::UInt => "uint".to_string(),
            Type::Float => "float".to_string(),
            Type::String => "string".to_string(),
            Type::Char => "char".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Array(t) => format!("[{}]", t.full_name()),
            Type::Struct(s) => s.full_name(),
            Type::Enum(u) => u.full_name(),
            Type::EnumMember(um) => um.full_name(),
            Type::Union(u) => u.full_name(),
            Type::Function(f) => f.full_name(),
            Type::Literal { name, type_ } => format!("#{}: {}", type_.full_name(), name),
            Type::Trait(t) => t.full_name(),
        }
    }
}

impl Into<parser::Literal> for Type {
    fn into(self) -> parser::Literal {
        match self {
            Type::Void => parser::Literal::Unit,
            Type::Unit => parser::Literal::Unit,
            Type::Int => parser::Literal::Int(0),
            Type::UInt => parser::Literal::UInt(0),
            Type::Float => parser::Literal::Float(0.0),
            Type::String => parser::Literal::String("".to_string()),
            Type::Char => parser::Literal::Char(' '),
            Type::Bool => parser::Literal::Bool(false),
            _ => panic!("Cannot convert type {} to literal", self.full_name()),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.to_string().fmt(f)
    }
}

/// Check if two types are equal
/// The logic is as follows:
/// - The 'left' type can be made from the 'right' type
/// - The 'right' type cannot be made from the 'left' type
///
/// # Examples
/// ```
/// use crate::types::{Type, type_equals};
///
/// let literal_int = Type::Literal(Literal::Int(0));
/// let int = Type::Int;
///
/// // Left type is stricter than right type.
/// // This causes the function to return false as they are not equal
/// assert_eq!(type_equals(&literal_int, &int), false);
///
/// // Left type is less strict than right type.
/// // This causes the function to return true as the left type can be made from the right type
/// assert_eq!(type_equals(&int, &literal_int), true);
pub fn type_equals(left: &Type, right: &Type) -> bool {
    match (left, right) {
        (Type::UInt, Type::Literal { name, type_ }) if matches!(**type_, Type::Int) => {
            name.parse::<u64>().is_ok()
        }
        (Type::Int, Type::Literal { name, type_ }) if matches!(**type_, Type::UInt) => {
            name.parse::<i64>().is_ok()
        }
        (Type::Union(Union { literals, .. }), Type::Literal { .. }) => literals.contains(right),
        (other, Type::Union(Union { literal_type, .. })) => type_equals(other, literal_type),
        (
            Type::Literal { name, type_ },
            Type::Literal {
                name: name_2,
                type_: type_2,
            },
        ) => name == name_2 && type_equals(type_, type_2),
        (other, Type::Literal { type_, .. }) => type_equals(other, type_),
        (Type::Function(fl), Type::Function(fr)) => {
            type_equals(fl.return_type.as_ref(), fr.return_type.as_ref())
                && fl
                    .param
                    .as_ref()
                    .zip(fr.param.as_ref())
                    .map_or(true, |(p, p2)| type_equals(&p.type_, &p2.type_))
        }
        (
            Type::Enum(Enum {
                type_identifier,
                members,
                ..
            }),
            Type::EnumMember(EnumMember {
                enum_name,
                discriminant_name,
                ..
            }),
        ) => type_identifier == enum_name && members.contains_key(discriminant_name),
        _ => left == right,
    }
}

pub fn type_equals_coerce(left: &Type, right: &Type) -> bool {
    match (left, right) {
        (Type::Literal { type_, .. }, Type::Literal { type_: type_2, .. }) => {
            type_equals(type_, type_2)
        }
        _ => type_equals(left, right),
    }
}

pub fn type_annotation_equals(left: &TypeAnnotation, right: &TypeAnnotation) -> bool {
    match (left, right) {
        (TypeAnnotation::Literal(l), TypeAnnotation::Literal(r)) => l == r,
        (TypeAnnotation::Array(l), TypeAnnotation::Array(r)) => l == r,
        (TypeAnnotation::Function(lp, lr), TypeAnnotation::Function(rp, rr)) => {
            let p = match (lp.as_ref(), rp.as_ref()) {
                (Some(lp), Some(rp)) => type_annotation_equals(lp.as_ref(), rp.as_ref()),
                (None, None) => true,
                _ => false,
            };

            if !p {
                return false;
            }

            let r = match (lr.as_ref(), rr.as_ref()) {
                (Some(lr), Some(rr)) => type_annotation_equals(lr.as_ref(), rr.as_ref()),
                (None, None) => true,
                _ => false,
            };

            r
        }
        (TypeAnnotation::Type(l), TypeAnnotation::Type(r)) => l == r,
        _ => false,
    }
}
