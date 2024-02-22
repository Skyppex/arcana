pub mod type_checker;
pub mod type_environment;
pub mod ast;
pub mod full_name;

mod statements;
mod expressions;
mod scope;

pub use type_checker::*;
pub use type_environment::*;
pub use full_name::*;

use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::types::{GenericType, TypeAnnotation, TypeIdentifier};

use self::statements::check_type_annotation;

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub type_identifier: TypeIdentifier,
    pub fields: HashMap<String, Type>,
}

impl FullName for Struct {
    fn full_name(&self) -> String {
        self.type_identifier.to_string()
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
        format!("{}.{}: {}", self.struct_name, self.field_name, self.field_type)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Union {
    pub type_identifier: TypeIdentifier,
    pub members: HashMap<String, Type>,
}

impl FullName for Union {
    fn full_name(&self) -> String {
        self.type_identifier.to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionMember {
    pub union_name: TypeIdentifier,
    pub discriminant_name: String,
    pub fields: HashMap<String, Type>,
}

impl FullName for UnionMember {
    fn full_name(&self) -> String {
        format!("{}::{}", self.union_name, self.discriminant_name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionMemberField {
    pub union_name: TypeIdentifier,
    pub discriminant_name: String,
    pub field_name: String,
    pub field_type: Box<Type>,
}

impl FullName for UnionMemberField {
    fn full_name(&self) -> String {
        format!("{}::{}.{}", self.union_name, self.discriminant_name, self.field_name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub identifier: TypeIdentifier,
    pub parameters: HashMap<String, Type>,
    pub return_type: Box<Type>,
}

impl FullName for Function {
    fn full_name(&self) -> String {
        self.identifier.to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Generic(GenericType),
    Void,
    Unit,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    String,
    Char,
    Bool,
    Array(Box<Type>),
    Struct(Struct),
    StructField(StructField),
    Union(Union),
    UnionMember(UnionMember),
    UnionMemberField(UnionMemberField),
    Function(Function),
    Literal {
        name: String, // String representation of the literal
        type_: Box<Type>,
    }
}

impl Type {
    pub fn from_string(type_name: &str) -> Option<Type> {
        match type_name {
            "void" => Some(Type::Void),
            "unit" => Some(Type::Unit),
            "i8" => Some(Type::I8),
            "i16" => Some(Type::I16),
            "i32" => Some(Type::I32),
            "i64" => Some(Type::I64),
            "i128" => Some(Type::I128),
            "u8" => Some(Type::U8),
            "u16" => Some(Type::U16),
            "u32" => Some(Type::U32),
            "u64" => Some(Type::U64),
            "u128" => Some(Type::U128),
            "f32" => Some(Type::F32),
            "f64" => Some(Type::F64),
            "string" => Some(Type::String),
            "char" => Some(Type::Char),
            "bool" => Some(Type::Bool),
            _ => None,
        }
    }

    pub fn to_string(&self) -> String {
        self.full_name()
    }

    pub fn type_identifier(&self) -> TypeIdentifier {
        match self {
            Type::Generic(name) => TypeIdentifier::Type(name.type_name.clone()),
            Type::Void => TypeIdentifier::Type("void".to_string()),
            Type::Unit => TypeIdentifier::Type("unit".to_string()),
            Type::I8 => TypeIdentifier::Type("i8".to_string()),
            Type::I16 => TypeIdentifier::Type("i16".to_string()),
            Type::I32 => TypeIdentifier::Type("i32".to_string()),
            Type::I64 => TypeIdentifier::Type("i64".to_string()),
            Type::I128 => TypeIdentifier::Type("i128".to_string()),
            Type::U8 => TypeIdentifier::Type("u8".to_string()),
            Type::U16 => TypeIdentifier::Type("u16".to_string()),
            Type::U32 => TypeIdentifier::Type("u32".to_string()),
            Type::U64 => TypeIdentifier::Type("u64".to_string()),
            Type::U128 => TypeIdentifier::Type("u128".to_string()),
            Type::F32 => TypeIdentifier::Type("f32".to_string()),
            Type::F64 => TypeIdentifier::Type("f64".to_string()),
            Type::String => TypeIdentifier::Type("string".to_string()),
            Type::Char => TypeIdentifier::Type("char".to_string()),
            Type::Bool => TypeIdentifier::Type("bool".to_string()),
            Type::Struct(s) => s.type_identifier.clone(),
            Type::StructField(sf) => TypeIdentifier::MemberType(Box::new(sf.struct_name.clone()), sf.field_name.clone()),
            Type::Union(u) => u.type_identifier.clone(),
            _ => panic!("Cannot get type identifier for type {}", self.full_name()),
        }
    }

    pub fn clone_with_concrete_types(&self, concrete_types: Vec<TypeAnnotation>, type_environment: Rc<RefCell<TypeEnvironment>>) -> Result<Type, String> {
        match self {
            Type::Struct(s) => {
                let TypeIdentifier::GenericType(_, generics) = s.type_identifier.clone() else {
                    return Err(format!("Cannot clone concrete types for type {}", self.full_name()));
                };

                let mut type_map = HashMap::new();

                for (ta, gt) in concrete_types.iter().zip(generics) {
                    type_map.insert(gt, ta);
                }

                let mut fields = HashMap::new();

                for (field_name, type_) in s.fields.iter() {
                    let Type::Generic(generic) = type_ else {
                        fields.insert(field_name.clone(), type_.clone());
                        continue;
                    };
                    
                    let concrete_type = type_map.get(generic)
                        .ok_or(format!("No concrete type found for generic type {}", generic.type_name))?;

                    let concrete_type = check_type_annotation(&concrete_type, &vec![], type_environment.clone())?;

                    fields.insert(field_name.clone(), concrete_type);
                }

                Ok(Type::Struct(Struct {
                    type_identifier: s.type_identifier.clone(),
                    fields,
                }))
            },
            _ => Err(format!("Cannot clone concrete types for type {}", self.full_name())),
        }
    }
}

impl FullName for Type {
    fn full_name(&self) -> String {
        match self {
            Type::Generic(GenericType { type_name }) => type_name.to_string(),
            Type::Void => "void".to_string(),
            Type::Unit => "unit".to_string(),
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::I128 => "i128".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::U128 => "u128".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::String => "string".to_string(),
            Type::Char => "char".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Array(t) => format!("[{}]", t.full_name()),
            Type::Struct(s) => s.full_name(),
            Type::StructField(sf) => sf.full_name(),
            Type::Union(u) => u.full_name(),
            Type::UnionMember(um) => um.full_name(),
            Type::UnionMemberField(umf) => umf.full_name(),
            Type::Function(f) => f.full_name(),
            Type::Literal { name, .. } => name.clone(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.to_string().fmt(f)
    }
}
