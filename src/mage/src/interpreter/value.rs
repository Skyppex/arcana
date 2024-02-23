use std::{fmt::Display, collections::HashMap};

use shared::{type_checker::ast::TypedStatement, types::{TypeAnnotation, TypeIdentifier}};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Uninitialized,
    Void,
    Unit,
    Bool(bool),
    Number(Number),
    Char(char),
    String(String),
    Array(Vec<Value>),
    Struct { struct_name: TypeAnnotation, fields: HashMap<String, Value> },
    Enum { enum_member: EnumMember, fields: EnumFields },
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
            Value::Array(values) => {
                write!(f, "[")?;

                for (index, value) in values.iter().enumerate() {
                    write!(f, "{}", value)?;

                    if index < values.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, "]")
            },
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
            Value::Enum {
                enum_member,
                fields
            } => {
                match fields {
                    EnumFields::None => write!(f, ""),
                    EnumFields::Named(fields) => {
                        write!(f, "{}::{}(", enum_member.enum_name, enum_member.member_name)?;
                        
                        for (index, (identifier, value)) in fields.iter().enumerate() {
                            write!(f, "{}: {}", identifier, value)?;
                            
                            if index < fields.len() - 1 {
                                write!(f, ", ")?;
                            }
                        }
                        
                        write!(f, ")")
                    },
                    EnumFields::Unnamed(fields) => {
                        write!(f, "{}::{}(", enum_member.enum_name, enum_member.member_name)?;

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
            Value::Function {
                parameters: _,
                body: _
            } => todo!("Display for function value"),
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
pub struct EnumMember {
    pub enum_name: TypeAnnotation,
    pub member_name: String,
}

impl Display for EnumMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.enum_name, self.member_name)
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
pub enum EnumFields {
    None,
    Named(HashMap<String, Value>),
    Unnamed(Vec<Value>),
}
