use std::{cell::RefCell, collections::HashMap, rc::Rc};

use shared::{type_checker::ast::Member, types::TypeAnnotation};

use super::{
    scope::{Scope, ScopeState, ScopeType},
    value::{Value, Variable},
};

pub type Rcrc<T> = Rc<RefCell<T>>;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Environment {
    pub parent: Option<Rcrc<Environment>>,
    pub modules: Vec<Vec<String>>,
    pub variables: HashMap<String, Rcrc<Variable>>,
    pub functions: HashMap<String, Rcrc<Variable>>,
    pub static_members: HashMap<TypeAnnotation, HashMap<String, Rcrc<Variable>>>,
    pub scopes: Vec<ScopeState>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            parent: None,
            modules: Vec::new(),
            variables: HashMap::new(),
            functions: HashMap::new(),
            static_members: HashMap::new(),
            scopes: vec![],
        }
    }

    pub fn new_parent(parent: Rcrc<Environment>) -> Self {
        Self {
            parent: Some(parent),
            modules: Vec::new(),
            variables: HashMap::new(),
            functions: HashMap::new(),
            static_members: HashMap::new(),
            scopes: vec![],
        }
    }

    pub fn new_scope<T: Into<ScopeState>>(parent: Rcrc<Environment>, scope: T) -> Self {
        Self::new_scopes(parent, [scope])
    }

    pub fn new_scopes<T: Into<ScopeState>, U: IntoIterator<Item = T>>(
        parent: Rcrc<Environment>,
        scopes: U,
    ) -> Self {
        Self {
            parent: Some(parent),
            modules: Vec::new(),
            variables: HashMap::new(),
            functions: HashMap::new(),
            static_members: HashMap::new(),
            scopes: scopes
                .into_iter()
                .map(|scope| scope.into())
                .collect::<Vec<ScopeState>>(),
        }
    }

    pub fn has_scope(&self, scope_type: &ScopeType) -> bool {
        self.scopes.iter().any(|s| s.scope_type == *scope_type)
            || self
                .parent
                .as_ref()
                .map(|p| p.borrow().has_scope(scope_type))
                .unwrap_or(false)
    }

    pub fn get_scope(&self, scope_type: &ScopeType) -> Option<Scope> {
        self.scopes
            .iter()
            .find(|s| s.scope_type == *scope_type && s.active)
            .map(|f: &ScopeState| f.scope.clone())
            .or_else(|| {
                self.parent
                    .as_ref()
                    .and_then(|p| p.borrow().get_scope(scope_type))
            })
    }

    pub fn activate_scope(&mut self, scope: Scope) -> Result<(), String> {
        let scope_type: ScopeType = scope.clone().into();
        if !self.has_scope(&scope_type) {
            return Err(format!("Scope '{:?}' not found", scope_type));
        }

        match self.scopes.iter_mut().find(|s| s.scope_type == scope_type) {
            Some(scope_state) => {
                if scope_state.active {
                    panic!("Scope '{:?}' already active", scope_type);
                }

                scope_state.scope = scope;
                scope_state.active = true
            },
            None => self.parent.as_mut()
                .expect("Already checked if the scope exists, if it's not in the current environment, it must be in the parent")
                .borrow_mut()
                .activate_scope(scope)?,
        }

        Ok(())
    }

    pub fn add_module(&mut self, module_path: Vec<String>) {
        self.modules.push(module_path);
    }

    pub fn add_variable(&mut self, identifier: String, value: Value, mutable: bool) {
        self.variables.insert(
            identifier.clone(),
            Rc::new(RefCell::new(Variable::new(identifier, value, mutable))),
        );
    }

    pub fn add_function(&mut self, identifier: String, value: Value, mutable: bool) {
        self.functions.insert(
            identifier.clone(),
            Rc::new(RefCell::new(Variable::new(identifier, value, mutable))),
        );
    }

    pub fn add_static_member(
        &mut self,
        type_annotation: &TypeAnnotation,
        member_name: String,
        variable: Rcrc<Variable>,
    ) {
        match self.static_members.get_mut(type_annotation) {
            Some(static_members) => {
                static_members.insert(member_name, variable);
            }
            None => {
                let mut static_members = HashMap::new();
                static_members.insert(member_name, variable);

                self.static_members
                    .insert(type_annotation.clone(), static_members);
            }
        }
    }

    pub fn get_variable(&self, identifier: &str) -> Option<Rcrc<Variable>> {
        self.resolve(identifier)
    }

    pub fn get_variables(&self) -> HashMap<String, Rcrc<Variable>> {
        let current_funcs = self.variables.clone();
        let parent_funcs = self
            .parent
            .clone()
            .map(|p| p.borrow().get_variables())
            .unwrap_or_default();

        current_funcs.into_iter().chain(parent_funcs).collect()
    }

    pub fn get_function(&self, identifier: &str) -> Option<Rcrc<Variable>> {
        self.resolve_function(identifier)
    }

    pub fn get_functions(&self) -> HashMap<String, Rcrc<Variable>> {
        let current_funcs = self.functions.clone();
        let parent_funcs = self
            .parent
            .clone()
            .map(|p| p.borrow().get_functions())
            .unwrap_or_default();

        current_funcs.into_iter().chain(parent_funcs).collect()
    }

    pub fn get_static_member(
        &self,
        type_annotation: &TypeAnnotation,
        member_name: &str,
    ) -> Option<Rcrc<Variable>> {
        if let Some(static_members) = self.static_members.get(type_annotation) {
            static_members.get(member_name).cloned()
        } else if let Some(parent) = &self.parent {
            parent
                .borrow()
                .get_static_member(type_annotation, member_name)
        } else {
            None
        }
    }

    pub fn set_variable(&mut self, member: Member, value: Value) -> Result<Value, String> {
        match member {
            Member::Identifier { symbol, type_: _ } => {
                let variable = self
                    .resolve(&symbol)
                    .ok_or(format!("Variable '{}' not found", symbol))?
                    .clone();

                if !matches!(variable.borrow().value, Value::Uninitialized)
                    && !variable.borrow().mutable
                {
                    return Err(format!("Cannot assign to immutable variable '{}'", symbol));
                }

                variable.borrow_mut().value = value.clone();
                Ok(value)
            }
            Member::StaticMemberAccess { .. } => Err("Cannot assign to static member".to_owned()),
            Member::MemberAccess { member, .. } => {
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

    pub fn resolve_function(&self, name: &str) -> Option<Rcrc<Variable>> {
        if let Some(functions) = self.functions.get(name) {
            Some(functions.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().resolve_function(name)
        } else {
            None
        }
    }
}
