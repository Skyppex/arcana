use std::{cell::RefCell, collections::HashMap, rc::Rc};

use shared::type_checker::ast::Member;

use super::{scope::{Scope, ScopeState, ScopeType}, value::{Variable, Value}};

pub type Rcrc<T> = Rc<RefCell<T>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub parent: Option<Rcrc<Environment>>,
    pub variables: HashMap<String, Rcrc<Variable>>,
    pub scopes: Vec<ScopeState>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            parent: None,
            variables: HashMap::new(),
            scopes: vec![],
        }
    }

    pub fn new_parent(parent: Rcrc<Environment>) -> Self {
        Self {
            parent: Some(parent),
            variables: HashMap::new(),
            scopes: vec![],
        }
    }

    pub fn new_scope(parent: Rcrc<Environment>, scopes: Scope) -> Self {
        Self::new_scopes(parent, [scopes])
    }

    pub fn new_scopes<T: IntoIterator<Item = Scope>>(parent: Rcrc<Environment>, scopes: T) -> Self {
        Self {
            parent: Some(parent),
            variables: HashMap::new(),
            scopes: scopes.into_iter().map(|scope| scope.into()).collect::<Vec<ScopeState>>(),
        }
    }

    pub fn has_scope(&self, scope_type: &ScopeType) -> bool {
        self.scopes.iter().any(|s| s.scope_type == *scope_type) ||
            self.parent.as_ref().map(|p| p.borrow().has_scope(scope_type)).unwrap_or(false)
    }

    pub fn get_scope(&self, scope_type: &ScopeType) -> Option<Scope> {
        self.scopes.iter().find(|s| s.scope_type == *scope_type && s.active)
            .map(|f| f.scope.clone())
            .or_else(|| self.parent.as_ref().and_then(|p| p.borrow().get_scope(scope_type)))
    }

    pub fn activate_scope(&mut self, scope: Scope) -> Result<(), String> {
        let scope_type: ScopeType = scope.clone().into();
        if !self.has_scope(&scope_type) {
            return Err(format!("Scope '{:?}' not found", scope_type));
        }

        match self.scopes.iter_mut().find(|s| s.scope_type == scope_type) {
            Some(scope_state) => {
                scope_state.scope = scope;
                scope_state.active = true
            },
            None => self.parent.as_mut().unwrap().borrow_mut().activate_scope(scope)?,
        }
        
        Ok(())
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
                let variable = self.resolve(&symbol)
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
