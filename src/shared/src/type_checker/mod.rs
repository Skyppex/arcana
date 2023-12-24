mod type_checker;

use std::collections::HashMap;

pub struct Struct{
    pub name: String,
    pub fields: HashMap<String, Type>,
}

pub struct StructField{
    pub struct_name: String,
    pub field_name: String,
    pub field_type: Box<Type>,
}

pub struct Union{
    pub name: String,
    pub members: HashMap<String, HashMap<String, Type>>,
}

pub struct UnionMember{
    pub union_name: String,
    pub discriminant_name: String,
    pub fields: HashMap<String, Type>,
}

pub enum Type {
    Void,
    Int8,
    Int16,
    Int32,
    Int64,
    Int128,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    UInt128,
    Float32,
    Float64,
    String,
    Char,
    Bool,
    Struct(Struct),
    StructField(StructField),
    Union(Union),
    UnionMember(UnionMember),
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
            "int8" => Some(Type::Int8),
            "int16" => Some(Type::Int16),
            "int32" => Some(Type::Int32),
            "int64" => Some(Type::Int64),
            "int128" => Some(Type::Int128),
            "uint8" => Some(Type::UInt8),
            "uint16" => Some(Type::UInt16),
            "uint32" => Some(Type::UInt32),
            "uint64" => Some(Type::UInt64),
            "uint128" => Some(Type::UInt128),
            "float32" => Some(Type::Float32),
            "float64" => Some(Type::Float64),
            "string" => Some(Type::String),
            "char" => Some(Type::Char),
            "bool" => Some(Type::Bool),
            _ => None,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Type::Void => "void".to_string(),
            Type::Int8 => "int8".to_string(),
            Type::Int16 => "int16".to_string(),
            Type::Int32 => "int32".to_string(),
            Type::Int64 => "int64".to_string(),
            Type::Int128 => "int128".to_string(),
            Type::UInt8 => "uint8".to_string(),
            Type::UInt16 => "uint16".to_string(),
            Type::UInt32 => "uint32".to_string(),
            Type::UInt64 => "uint64".to_string(),
            Type::UInt128 => "uint128".to_string(),
            Type::Float32 => "float32".to_string(),
            Type::Float64 => "float64".to_string(),
            Type::String => "string".to_string(),
            Type::Char => "char".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Struct(Struct { name, .. }) => name.clone(),
            Type::StructField(StructField { struct_name, field_name, .. }) => format!("{}.{}", struct_name, field_name),
            Type::Union(Union { name, .. }) => name.clone(),
            Type::UnionMember(UnionMember { union_name, discriminant_name, .. }) => format!("{}::{}", union_name, discriminant_name),
            // Type::Constant { name, .. } => name.clone(),
        }
    }
}