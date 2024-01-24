use std::{cell::RefCell, collections::HashMap, rc::Rc};

use shared::type_checker::ast::Member;

use super::value::{Variable, Value};

pub type Rcrc<T> = Rc<RefCell<T>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub parent: Option<Rcrc<Environment>>,
    pub variables: HashMap<String, Rcrc<Variable>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            parent: None,
            variables: HashMap::new(),
        }
    }

    pub fn new_parent(parent: Rcrc<Environment>) -> Self {
        Self {
            parent: Some(parent),
            variables: HashMap::new(),
        }
    }

    pub fn add_variable(&mut self, identifier: String, value: Value, mutable: bool) {
        self.variables.insert(identifier.clone(), Rc::new(RefCell::new(Variable::new(identifier, value, mutable))));
    }

    pub fn get_variable(&self, identifier: &str) -> Option<Rcrc<Variable>> {
        self.resolve(identifier)
    }

    pub fn set_variable(&mut self, member: Member, value: Value) -> Result<Value, String> {
        match member {
            Member::Identifier {
                symbol,
                type_: _
            } => {
                let mut variable = self.resolve(&symbol)
                    .ok_or(format!("Variable '{}' not found", symbol))?.clone();

                if !matches!(variable.borrow().value, Value::Uninitialized) && !variable.borrow().mutable {
                    return Err(format!("Cannot assign to immutable variable '{}'", symbol));
                }

                variable.borrow_mut().value = value.clone();
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

    pub fn remove_variable(&mut self, identifier: &str) -> Option<Rcrc<Variable>> {
        self.variables.remove(identifier)
    }

    pub fn resolve(&self, name: &str) -> Option<Rcrc<Variable>> {
        if let Some(variable) = self.variables.get(name) {
            Some(variable.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().resolve(name)
        } else {
            None
        }
    }
}
