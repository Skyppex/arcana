use std::collections::HashMap;

use super::Type;



pub struct TypeEnvironment<'a> {
    parent: Option<&'a TypeEnvironment<'a>>,
    types: HashMap<String, Type>,
    variables: HashMap<String, Type>,
}

impl<'a> TypeEnvironment<'a> {
    pub fn new() -> Self {
        Self {
            parent: None,
            types: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    pub fn new_child(&'a self) -> Self {
        Self {
            parent: Some(self),
            types: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    pub fn add_type(&mut self, name: String, type_: Type) {
        self.types.insert(name, type_);
    }

    pub fn add_variable(&mut self, name: String, type_: Type) {
        self.variables.insert(name, type_);
    }

    pub fn get_type(&self, name: &str) -> Option<&Type> {
        if let Some(type_) = self.types.get(name) {
            Some(type_)
        } else if let Some(parent) = &self.parent {
            parent.get_type(name)
        } else {
            None
        }
    }

    pub fn get_variable(&self, name: &str) -> Option<&Type> {
        if let Some(type_) = self.variables.get(name) {
            Some(type_)
        } else if let Some(parent) = &self.parent {
            parent.get_variable(name)
        } else {
            None
        }
    }
}