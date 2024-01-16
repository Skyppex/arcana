use std::collections::HashMap;

use super::value::{Variable, Value};

pub struct Environment {
    pub parent: Option<Box<Environment>>,
    pub variables: HashMap<String, Variable>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            parent: None,
            variables: HashMap::new(),
        }
    }

    pub fn new_with_parent(parent: Box<Environment>) -> Self {
        Self {
            parent: Some(parent),
            variables: HashMap::new(),
        }
    }

    pub fn add_variable(&mut self, identifier: String, value: Value, mutable: bool) {
        self.variables.insert(identifier.clone(), Variable::new(identifier, value, mutable));
    }

    pub fn get_variable(&self, identifier: &str) -> Option<&Variable> {
        if let Some(variable) = self.variables.get(identifier) {
            Some(variable)
        } else if let Some(parent) = &self.parent {
            parent.get_variable(identifier)
        } else {
            None
        }
    }
}
