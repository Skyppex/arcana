use std::{fmt::Display, collections::HashMap};

use shared::type_checker::ast::TypedStatement;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Uninitialized,
    Void,
    Unit,
    Bool(bool),
    Number(Number),
    Char(char),
    String(String),
    Struct { struct_name: String, fields: HashMap<String, Value> },
    Union { union_member: UnionMember, fields: UnionFields },
    Function { parameters: Vec<String>, body: Vec<TypedStatement> },
}

impl<'a> Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Uninitialized => write!(f, "uninitialized"),
            Value::Void => write!(f, "void"),
            Value::Unit => write!(f, "unit"),
            Value::Bool(boolean) => write!(f, "{}", boolean),
            Value::Number(number) => write!(f, "{}", number),
            Value::Char(character) => write!(f, "'{}'", character),
            Value::String(string) => write!(f, "\"{}\"", string),
            Value::Struct {
                struct_name,
                fields
            } => {
                write!(f, "{} {{ ", struct_name)?;

                for (index, (identifier, value)) in fields.iter().enumerate() {
                    write!(f, "{}: {}", identifier, value)?;

                    if index < fields.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, " }}")
            
            },
            Value::Union {
                union_member,
                fields
            } => {
                match fields {
                    UnionFields::None => write!(f, ""),
                    UnionFields::Named(fields) => {
                        write!(f, "{}::{}(", union_member.union_name, union_member.member_name)?;
                        
                        for (index, (identifier, value)) in fields.iter().enumerate() {
                            write!(f, "{}: {}", identifier, value)?;
                            
                            if index < fields.len() - 1 {
                                write!(f, ", ")?;
                            }
                        }
                        
                        write!(f, ")")
                    },
                    UnionFields::Unnamed(fields) => {
                        write!(f, "{}::{}(", union_member.union_name, union_member.member_name)?;

                        for (index, value) in fields.iter().enumerate() {
                            write!(f, "{}", value)?;
                            
                            if index < fields.len() - 1 {
                                write!(f, ", ")?;
                            }
                        }
                        
                        write!(f, ")")
                    },
                }
            },
            Value::Function { parameters, body } => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    F32(f32),
    F64(f64),
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::I8(number) => write!(f, "{}", number),
            Number::I16(number) => write!(f, "{}", number),
            Number::I32(number) => write!(f, "{}", number),
            Number::I64(number) => write!(f, "{}", number),
            Number::I128(number) => write!(f, "{}", number),
            Number::U8(number) => write!(f, "{}", number),
            Number::U16(number) => write!(f, "{}", number),
            Number::U32(number) => write!(f, "{}", number),
            Number::U64(number) => write!(f, "{}", number),
            Number::U128(number) => write!(f, "{}", number),
            Number::F32(number) => write!(f, "{}", number),
            Number::F64(number) => write!(f, "{}", number),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionMember {
    pub union_name: String,
    pub member_name: String,
}

impl Display for UnionMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.union_name, self.member_name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub identifier: String,
    pub value: Value,
    pub mutable: bool,
}

impl Variable {
    pub fn new(identifier: String, value: Value, mutable: bool) -> Self {
        Self {
            identifier,
            value,
            mutable,
        }
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.identifier, self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnionFields {
    None,
    Named(HashMap<String, Value>),
    Unnamed(Vec<Value>),
}
