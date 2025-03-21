use std::fmt::Display;

use shared::type_checker::ast::TypedExpression;

use crate::{environment::Rcrc, Environment};

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
    Tuple(Vec<Value>),
    Struct(Struct),
    Enum(Enum),
    Function {
        param_name: Option<String>,
        body: TypedExpression,
        environment: Rcrc<Environment>,
    },
}

impl Value {
    pub fn option_some(value: Value) -> Value {
        match value {
            Value::Void => Value::Void,
            v => Value::Enum(Enum {
                type_name: "Option".to_owned(),
                enum_member: Struct {
                    type_name: "Option::Some".to_owned(),
                    fields: vec![StructField {
                        identifier: "v".to_owned(),
                        value: v,
                    }],
                },
            }),
        }
    }

    pub fn option_none() -> Value {
        Value::Enum(Enum {
            type_name: "Option".to_owned(),
            enum_member: Struct {
                type_name: "Option::None".to_owned(),
                fields: vec![],
            },
        })
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Uninitialized => write!(f, "uninitialized"),
            Value::Void => write!(f, "void"),
            Value::Unit => write!(f, "unit"),
            Value::Bool(boolean) => write!(f, "{}", boolean),
            Value::Number(number) => write!(f, "{}", number),
            Value::Char(character) => write!(f, "'{}'", character),
            Value::String(string) => write!(f, "{}", string),
            Value::Array(values) => {
                write!(f, "[")?;

                for (index, value) in values.iter().enumerate() {
                    write!(f, "{}", value)?;

                    if index < values.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, "]")
            }
            Value::Tuple(values) => {
                write!(f, "(")?;

                for (index, value) in values.iter().enumerate() {
                    write!(f, "{}", value)?;

                    if index < values.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ")")
            }
            Value::Struct(Struct { type_name, fields }) => {
                write!(f, "{} {{ ", type_name)?;

                for (index, StructField { identifier, value }) in fields.iter().enumerate() {
                    write!(f, "{}: {}", identifier, value)?;

                    if index < fields.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, " }}")
            }
            Value::Enum(Enum {
                enum_member:
                    Struct {
                        type_name: enum_member,
                        fields,
                    },
                ..
            }) => {
                write!(f, "{} {{ ", enum_member)?;

                for (index, StructField { identifier, value }) in fields.iter().enumerate() {
                    write!(f, "{}: {}", identifier, value)?;

                    if index < fields.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, " }}")
            }
            Value::Function {
                param_name,
                body: _,
                environment: _,
            } => write!(f, "fun({})", param_name.clone().unwrap_or("".to_owned())),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Int(i64),
    UInt(u64),
    Float(f64),
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Int(number) => write!(f, "{}", number),
            Number::UInt(number) => write!(f, "{}", number),
            Number::Float(number) => write!(f, "{}", number),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub type_name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub identifier: String,
    pub value: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub type_name: String,
    pub enum_member: Struct,
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
        write!(
            f,
            "{} = {} -> {:?}",
            self.identifier, self.value, self.value
        )
    }
}
