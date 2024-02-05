use std::collections::HashMap;

use super::{FullName, Type};

#[derive(Debug, Clone)]
pub struct TypeEnvironment<'a> {
    parent: Option<&'a TypeEnvironment<'a>>,
    types: HashMap<String, Type>,
    variables: HashMap<String, Type>,
    impls: HashMap<String, HashMap<String, Type>>,
}

impl<'a> TypeEnvironment<'a> {
    pub fn new() -> Self {
        Self {
            parent: None,
            types: HashMap::from([
                ("void".to_string(), Type::Void),
                ("unit".to_string(), Type::Unit),
                ("bool".to_string(), Type::Bool),
                ("i8".to_string(), Type::I8),
                ("i16".to_string(), Type::I16),
                ("i32".to_string(), Type::I32),
                ("i64".to_string(), Type::I64),
                ("i128".to_string(), Type::I128),
                ("u8".to_string(), Type::U8),
                ("u16".to_string(), Type::U16),
                ("u32".to_string(), Type::U32),
                ("u64".to_string(), Type::U64),
                ("u128".to_string(), Type::U128),
                ("f32".to_string(), Type::F32),
                ("f64".to_string(), Type::F64),
                ("char".to_string(), Type::Char),
                ("string".to_string(), Type::String),
            ]),
            variables: HashMap::new(),
            impls: HashMap::new(),
        }
    }

    pub fn new_child(&'a self) -> Self {
        Self {
            parent: Some(self),
            types: HashMap::new(),
            variables: HashMap::new(),
            impls: HashMap::new(),
        }
    }

    pub fn add_type(&mut self, type_: Type) -> Result<(), String> {
        let full_name = type_.full_name();
        if self.types.contains_key(&full_name) {
            return Err(format!("Type {} already exists", type_.full_name()));
        }

        self.types.insert(type_.full_name(), type_);
        Ok(())
    }

    pub fn add_impl_function(&mut self, type_: Type, function_name: String, function_type: Type) -> Result<(), String> {
        let full_name = type_.full_name();
        match self.impls.get_mut(&full_name) {
            Some(impl_) => {
                if impl_.contains_key(&function_name) {
                    return Err(format!("Function {} already exists for type {}", function_name, full_name));
                }

                impl_.insert(function_name, function_type);
            },
            None => {
                let mut impl_ = HashMap::new();
                impl_.insert(function_name, function_type);
                self.impls.insert(full_name, impl_);
            }
        }

        Ok(())
    }

    pub fn add_variable(&mut self, name: String, type_: Type) {
        self.variables.insert(name, type_);
    }

    pub fn get_type(&self, name: &str) -> Option<Type> {
        if let Some(type_) = self.types.get(name) {
            Some(type_.clone())
        } else if let Some(parent) = &self.parent {
            parent.get_type(name)
        } else {
            if name.starts_with("[") {
                let type_name = &name[1..name.len() - 1];
                if let Some(type_) = self.get_type(type_name) {
                    return Some(Type::Array(Box::new(type_.clone())));
                }
            }
    
            None
        }
    }

    pub fn get_variable(&self, name: &str) -> Option<Type> {
        if let Some(type_) = self.variables.get(name) {
            Some(type_.clone())
        } else if let Some(parent) = &self.parent {
            parent.get_variable(name)
        } else {
            None
        }
    }

    pub fn get_types(&self) -> &HashMap<String, Type> {
        &self.types
    }

    pub fn get_variables(&self) -> &HashMap<String, Type> {
        &self.variables
    }

    pub fn lookup_type<T: FullName>(&self, full_name: &T) -> bool {
        self.types.get(&full_name.full_name())
            .is_some() ||
            self.parent
                .map_or(false, |parent|
                    parent.lookup_type(full_name))
    }
}
