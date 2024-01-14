pub mod type_checker;
mod type_environment;
mod statements;
mod expressions;
pub mod ast;

pub use type_checker::*;

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: String,
    pub fields: HashMap<String, Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub struct_name: String,
    pub field_name: String,
    pub field_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Union {
    pub name: String,
    pub members: HashMap<String, Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionMember {
    pub union_name: String,
    pub discriminant_name: String,
    pub fields: HashMap<Option<String>, Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionMemberField {
    pub union_name: String,
    pub discriminant_name: String,
    pub field_position: usize,
    pub field_name: Option<String>,
    pub field_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub parameters: HashMap<String, Type>,
    pub return_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
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
    // Constant {
    //     name: String,
    //     type_name: String,
    //     value: Box<Type>,
    // }
}

impl Type {
    pub fn from_string(type_name: &str) -> Option<Type> {
        match type_name {
            "void" => Some(Type::Void),
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
        match self {
            Type::Void => "void".to_string(),
            Type::I8 => "int8".to_string(),
            Type::I16 => "int16".to_string(),
            Type::I32 => "int32".to_string(),
            Type::I64 => "int64".to_string(),
            Type::I128 => "int128".to_string(),
            Type::U8 => "uint8".to_string(),
            Type::U16 => "uint16".to_string(),
            Type::U32 => "uint32".to_string(),
            Type::U64 => "uint64".to_string(),
            Type::U128 => "uint128".to_string(),
            Type::F32 => "float32".to_string(),
            Type::F64 => "float64".to_string(),
            Type::String => "string".to_string(),
            Type::Char => "char".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Struct(Struct { name, .. }) => name.clone(),
            Type::StructField(StructField { struct_name, field_name, .. }) => format!("{}.{}", struct_name, field_name),
            Type::Union(Union { name, .. }) => name.clone(),
            Type::UnionMember(UnionMember { union_name, discriminant_name, .. }) => format!("{}::{}", union_name, discriminant_name),
            Type::UnionMemberField(UnionMemberField { union_name, discriminant_name, field_position, field_name, ..}) => match field_name {
                Some(field_name) => format!("{}::{}.{}", union_name, discriminant_name, field_name),
                None => format!("{}::{}::{}", union_name, discriminant_name, field_position.to_string()),
            },
            Type::Function(Function { name, .. }) => name.clone(),
            // Type::Constant { name, .. } => name.clone(),
        }
    }
}