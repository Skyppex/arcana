use std::fmt::Display;

use shared::{built_in::BuiltInFunctionType, type_checker::model::TypedExpression};

use crate::{environment::Rcrc, Environment};

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Uninitialized,
    Void,
    Unit,
    Bool(bool),
    Number(Number),
    Rune(char),
    String(String),
    Array(Vec<Value>),
    Tuple(Vec<Value>),
    Struct(Struct),
    Enum(Enum),
    Function {
        param_name: Option<String>,
        body: FunctionBody,
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

    pub fn is_void(&self) -> bool {
        *self == Value::Void
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
            Value::Rune(character) => write!(f, "{}", character),
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

#[allow(clippy::large_enum_variant)]
#[allow(unpredictable_function_pointer_comparisons)]
#[derive(Debug, Clone, PartialEq)]
pub enum FunctionBody {
    Expr(TypedExpression),
    Fn(fn(Option<Value>) -> Value),
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

pub fn get_built_in_function_value(
    function_type: BuiltInFunctionType,
    environment: Rcrc<Environment>,
) -> Value {
    match function_type {
        BuiltInFunctionType::Input => Value::Function {
            param_name: Some("prompt".to_string()),
            body: FunctionBody::Fn(|value| {
                let Some(value) = value else {
                    unreachable!("Type is known after type checking, this should never happen")
                };

                println!("{}", value);
                let mut input = String::new();
                std::io::stdin().read_line(&mut input).unwrap();
                Value::String(input.trim().to_string())
            }),
            environment,
        },
        BuiltInFunctionType::Print => Value::Function {
            param_name: Some("value".to_string()),
            body: FunctionBody::Fn(|value| {
                let Some(value) = value else {
                    return Value::Void;
                };

                print!("{}", value);
                Value::Void
            }),
            environment,
        },
        BuiltInFunctionType::PrintLn => Value::Function {
            param_name: Some("value".to_string()),
            body: FunctionBody::Fn(|value| {
                let Some(value) = value else {
                    println!();
                    return Value::Void;
                };

                println!("{}", value);
                Value::Void
            }),
            environment,
        },
        BuiltInFunctionType::EPrint => Value::Function {
            param_name: Some("value".to_string()),
            body: FunctionBody::Fn(|value| {
                let Some(value) = value else {
                    return Value::Void;
                };

                eprint!("{}", value);
                Value::Void
            }),
            environment,
        },
        BuiltInFunctionType::EPrintLn => Value::Function {
            param_name: Some("value".to_string()),
            body: FunctionBody::Fn(|value| {
                let Some(value) = value else {
                    eprintln!();
                    return Value::Void;
                };

                eprintln!("{}", value);
                Value::Void
            }),
            environment,
        },
        BuiltInFunctionType::Drop => Value::Function {
            param_name: Some("var".to_string()),
            body: FunctionBody::Fn(|_| todo!()),
            environment,
        },
        BuiltInFunctionType::Len => Value::Function {
            param_name: Some("arr".to_string()),
            body: FunctionBody::Fn(|value| {
                let Some(Value::Array(arr)) = value else {
                    unreachable!("Type is known after type checking, this should never happen")
                };

                Value::Number(Number::UInt(arr.len() as u64))
            }),
            environment,
        },
        BuiltInFunctionType::Rand => Value::Function {
            param_name: Some("arr".to_string()),
            body: FunctionBody::Fn(|value| {
                let Some(Value::Array(arr)) = value else {
                    unreachable!("Type is known after type checking, this should never happen")
                };

                if arr.is_empty() {
                    return Value::Void;
                }

                let random_index = rand::random_range(0..arr.len());

                arr[random_index].clone()
            }),
            environment,
        },
    }
}
