use std::{collections::HashMap, fmt::Display};

use shared::{type_checker::ast::TypedExpression, types::TypeAnnotation};

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
    Struct {
        struct_name: TypeAnnotation,
        fields: HashMap<String, Value>,
    },
    Enum {
        enum_member: EnumMember,
        fields: EnumFields,
    },
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
            v => Value::Enum {
                enum_member: EnumMember {
                    enum_name: TypeAnnotation::ConcreteType("Option".to_owned(), vec![]),
                    member_name: "Some".to_owned(),
                },
                fields: EnumFields::Named(HashMap::from([("v".to_owned(), v)])),
            },
        }
    }

    pub fn option_none() -> Value {
        Value::Enum {
            enum_member: EnumMember {
                enum_name: TypeAnnotation::ConcreteType("Option".to_owned(), vec![]),
                member_name: "None".to_owned(),
            },
            fields: EnumFields::None,
        }
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
            Value::Struct {
                struct_name,
                fields,
            } => {
                write!(f, "{} {{ ", struct_name)?;

                for (index, (identifier, value)) in fields.iter().enumerate() {
                    write!(f, "{}: {}", identifier, value)?;

                    if index < fields.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, " }}")
            }
            Value::Enum {
                enum_member,
                fields,
            } => match fields {
                EnumFields::None => {
                    write!(f, "{}", enum_member)
                }
                EnumFields::Named(fields) => {
                    write!(f, "{} {{ ", enum_member)?;

                    for (index, (identifier, value)) in fields.iter().enumerate() {
                        write!(f, "{}: {}", identifier, value)?;

                        if index < fields.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }

                    write!(f, " }}")
                }
            },
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
pub struct EnumMember {
    pub enum_name: TypeAnnotation,
    pub member_name: String,
}

impl EnumMember {
    pub fn type_annotation(&self) -> TypeAnnotation {
        TypeAnnotation::Type(self.enum_name.name())
    }
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
        write!(
            f,
            "{} = {} -> {:?}",
            self.identifier, self.value, self.value
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumFields {
    None,
    Named(HashMap<String, Value>),
}

impl EnumFields {
    pub fn get(&self, key: &str) -> Option<&Value> {
        match self {
            EnumFields::None => None,
            EnumFields::Named(fields) => fields.get(key),
        }
    }
}
