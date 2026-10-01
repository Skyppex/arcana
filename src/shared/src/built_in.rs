use std::{
    fmt::{Display, Formatter},
    str::FromStr,
};

use crate::{
    type_checker::{Function, Meta, Parameter, Purity, Type},
    types::TypeIdentifier,
};

#[derive(Debug, Clone, PartialEq)]
pub struct BuiltInFunction {
    pub type_identifier: TypeIdentifier,
    pub function_type: BuiltInFunctionType,
    pub type_: Type,
}

impl BuiltInFunction {
    pub fn new(name: &str) -> Option<Self> {
        let type_identifier = TypeIdentifier::Type(name.to_string());

        let Ok(built_in_function_type) = BuiltInFunctionType::from_str(name) else {
            return None;
        };

        match built_in_function_type {
            BuiltInFunctionType::Input => Some(Self {
                type_identifier: type_identifier.clone(),
                function_type: BuiltInFunctionType::Input,
                type_: Type::Function(Function {
                    purity: built_in_function_type.purity(),
                    identifier: Some(type_identifier),
                    param: Some(Parameter {
                        identifier: "prompt".to_string(),
                        type_: Box::new(Type::String),
                    }),
                    return_type: Box::new(Type::String),
                }),
            }),
            BuiltInFunctionType::Print => Some(Self {
                type_identifier: type_identifier.clone(),
                function_type: BuiltInFunctionType::Print,
                type_: Type::Function(Function {
                    purity: built_in_function_type.purity(),
                    identifier: Some(type_identifier),
                    param: Some(Parameter {
                        identifier: "value".to_string(),
                        type_: Box::new(Type::Any),
                    }),
                    return_type: Box::new(Type::Void),
                }),
            }),
            BuiltInFunctionType::PrintLn => Some(Self {
                type_identifier: type_identifier.clone(),
                function_type: BuiltInFunctionType::PrintLn,
                type_: Type::Function(Function {
                    purity: built_in_function_type.purity(),
                    identifier: Some(type_identifier),
                    param: Some(Parameter {
                        identifier: "value".to_string(),
                        type_: Box::new(Type::Any),
                    }),
                    return_type: Box::new(Type::Void),
                }),
            }),
            BuiltInFunctionType::EPrint => Some(Self {
                type_identifier: type_identifier.clone(),
                function_type: BuiltInFunctionType::EPrint,
                type_: Type::Function(Function {
                    purity: built_in_function_type.purity(),
                    identifier: Some(type_identifier),
                    param: Some(Parameter {
                        identifier: "value".to_string(),
                        type_: Box::new(Type::Any),
                    }),
                    return_type: Box::new(Type::Void),
                }),
            }),
            BuiltInFunctionType::EPrintLn => Some(Self {
                type_identifier: type_identifier.clone(),
                function_type: BuiltInFunctionType::EPrintLn,
                type_: Type::Function(Function {
                    purity: built_in_function_type.purity(),
                    identifier: Some(type_identifier),
                    param: Some(Parameter {
                        identifier: "value".to_string(),
                        type_: Box::new(Type::Any),
                    }),
                    return_type: Box::new(Type::Void),
                }),
            }),
            BuiltInFunctionType::Drop => Some(Self {
                type_identifier: type_identifier.clone(),
                function_type: BuiltInFunctionType::Drop,
                type_: Type::Function(Function {
                    purity: built_in_function_type.purity(),
                    identifier: Some(type_identifier),
                    param: Some(Parameter {
                        identifier: "var".to_string(),
                        type_: Box::new(Type::Meta(Meta::Ident)),
                    }),
                    return_type: Box::new(Type::Void),
                }),
            }),
            BuiltInFunctionType::Len => Some(Self {
                type_identifier: type_identifier.clone(),
                function_type: BuiltInFunctionType::Len,
                type_: Type::Function(Function {
                    purity: built_in_function_type.purity(),
                    identifier: Some(type_identifier),
                    param: Some(Parameter {
                        identifier: "arr".to_string(),
                        type_: Box::new(Type::Array(Box::new(Type::Any))),
                    }),
                    return_type: Box::new(Type::UInt),
                }),
            }),
            BuiltInFunctionType::TypeOf => Some(Self {
                type_identifier: type_identifier.clone(),
                function_type: BuiltInFunctionType::TypeOf,
                type_: Type::Function(Function {
                    purity: built_in_function_type.purity(),
                    identifier: Some(type_identifier),
                    param: Some(Parameter {
                        identifier: "value".to_string(),
                        type_: Box::new(Type::Any),
                    }),
                    return_type: Box::new(Type::String),
                }),
            }),
            BuiltInFunctionType::Rand => Some(Self {
                type_identifier: type_identifier.clone(),
                function_type: BuiltInFunctionType::Rand,
                type_: Type::Function(Function {
                    purity: built_in_function_type.purity(),
                    identifier: Some(type_identifier),
                    param: Some(Parameter {
                        identifier: "arr".to_string(),
                        type_: Box::new(Type::Array(Box::new(Type::Any))),
                    }),
                    return_type: Box::new(Type::Any),
                }),
            }),
        }
    }
}

impl BuiltInFunctionType {
    /// Whether calling this built-in can be observed.
    ///
    /// Written out rather than inferred, because these are where every effect
    /// in the language originates: nothing else is impure except by reaching
    /// one of them.
    pub fn purity(&self) -> Purity {
        match self {
            // Writes to a stream.
            BuiltInFunctionType::Print
            | BuiltInFunctionType::PrintLn
            | BuiltInFunctionType::EPrint
            | BuiltInFunctionType::EPrintLn => Purity::Impure,
            // Reads a stream, and gives a different answer every time.
            BuiltInFunctionType::Input => Purity::Impure,
            // Removes a binding from the environment.
            BuiltInFunctionType::Drop => Purity::Impure,
            // Has no effects, but is not deterministic — and `Purity` merges
            // those two properties, so it lands here. Deleting an unused
            // `rand` would in fact be sound; substituting one call's value for
            // another's would not.
            BuiltInFunctionType::Rand => Purity::Impure,
            BuiltInFunctionType::Len | BuiltInFunctionType::TypeOf => Purity::Pure,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltInFunctionType {
    Input,
    Print,
    PrintLn,
    EPrint,
    EPrintLn,
    Drop,
    Len,
    Rand,
    TypeOf,
}

impl Display for BuiltInFunctionType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BuiltInFunctionType::Input => write!(f, "input"),
            BuiltInFunctionType::Print => write!(f, "print"),
            BuiltInFunctionType::PrintLn => write!(f, "println"),
            BuiltInFunctionType::EPrint => write!(f, "print"),
            BuiltInFunctionType::EPrintLn => write!(f, "println"),
            BuiltInFunctionType::Drop => write!(f, "drop"),
            BuiltInFunctionType::Len => write!(f, "len"),
            BuiltInFunctionType::Rand => write!(f, "rand"),
            BuiltInFunctionType::TypeOf => write!(f, "typeof"),
        }
    }
}

impl FromStr for BuiltInFunctionType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "input" => Ok(BuiltInFunctionType::Input),
            "print" => Ok(BuiltInFunctionType::Print),
            "println" => Ok(BuiltInFunctionType::PrintLn),
            "eprint" => Ok(BuiltInFunctionType::EPrint),
            "eprintln" => Ok(BuiltInFunctionType::EPrintLn),
            "drop" => Ok(BuiltInFunctionType::Drop),
            "len" => Ok(BuiltInFunctionType::Len),
            "rand" => Ok(BuiltInFunctionType::Rand),
            "typeof" => Ok(BuiltInFunctionType::TypeOf),
            _ => Err(format!("Unknown built-in function type: {}", s)),
        }
    }
}
