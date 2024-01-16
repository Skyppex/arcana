use std::collections::HashMap;

use shared::type_checker::ast::Member;

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

    pub fn set_variable(&mut self, member: Member, value: Value) -> Result<Value, String> {
        match member {
            Member::Identifier {
                symbol,
                type_: _
            } => {
                let mut variable = self.resolve(&symbol)
                    .ok_or(format!("Variable '{}' not found", symbol))?.clone();

                if matches!(variable.value, Value::Uninitialized) && !variable.mutable {
                    return Err(format!("Cannot assign to immutable variable '{}'", symbol));
                }

                variable.value = value.clone();
                return Ok(value);
            }
            Member::MemberAccess {
                object: _,
                member,
                symbol: _,
                type_: _
            } => {
                self.set_variable(*member.clone(), value.clone())
            }
        }
    }

    pub fn resolve(&self, name: &str) -> Option<&Variable> {
        if let Some(variable) = self.variables.get(name) {
            Some(variable)
        } else if let Some(parent) = &self.parent {
            parent.resolve(name)
        } else {
            None
        }
    }
}
