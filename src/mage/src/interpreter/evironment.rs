use std::collections::HashMap;

use shared::type_checker::ast::Member;

use super::value::{Variable, Value};

#[derive(Debug, Clone, PartialEq)]
pub struct Environment<'a> {
    pub parent: Option<&'a Environment<'a>>,
    pub variables: HashMap<String, Variable>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            parent: None,
            variables: HashMap::new(),
        }
    }

    pub fn new_child(&'a self) -> Self {
        Self {
            parent: Some(self),
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

                if !matches!(variable.value, Value::Uninitialized) && !variable.mutable {
                    return Err(format!("Cannot assign to immutable variable '{}'", symbol));
                }

                variable.value = value.clone();
                self.variables.insert(symbol, variable);
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

    pub fn remove_variable(&mut self, identifier: &str) -> Option<Variable> {
        self.variables.remove(identifier)
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
