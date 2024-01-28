pub mod type_checker;
pub mod type_environment;
pub mod ast;
pub mod full_name;

mod statements;
mod expressions;

pub use type_checker::*;
pub use type_environment::*;
pub use full_name::*;

use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: String,
    pub fields: HashMap<String, Type>,
}

impl FullName for Struct {
    fn full_name(&self) -> String {
        self.name.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub struct_name: String,
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
    pub name: String,
    pub members: HashMap<String, Type>,
}

impl FullName for Union {
    fn full_name(&self) -> String {
        self.name.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionMember {
    pub union_name: String,
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
    pub union_name: String,
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
    pub name: String,
    pub parameters: HashMap<String, Type>,
    pub return_type: Box<Type>,
}

impl FullName for Function {
    fn full_name(&self) -> String {
        self.name.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
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
}

impl FullName for Type {
    fn full_name(&self) -> String {
        match self {
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
