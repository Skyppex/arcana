use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::types::{parse_type_annotation_from_str, TypeAnnotation, TypeName};

use super::{scope::{Scope, ScopeType}, FullName, Type};

pub type Rcrc<T> = Rc<RefCell<T>>;

#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    parent: Option<Rcrc<TypeEnvironment>>,
    types: HashMap<String, Type>,
    variables: HashMap<String, Type>,
    impls: HashMap<String, HashMap<String, Type>>,
    scopes: Vec<Scope>,
}

impl TypeEnvironment {
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
            scopes: Vec::new(),
        }
    }

    pub fn new_parent(parent: Rcrc<Self>) -> Self {
        Self {
            parent: Some(parent),
            types: HashMap::new(),
            variables: HashMap::new(),
            impls: HashMap::new(),
            scopes: Vec::new(),
        }
    }

    pub fn new_scope<T: Into<Scope>>(parent: Rcrc<Self>, scope: T) -> Self {
        Self::new_scopes(parent, [scope])
    }

    pub fn new_scopes<T: Into<Scope>, U: IntoIterator<Item = T>>
    (parent: Rcrc<Self>, scopes: U) -> Self {
        Self {
            parent: Some(parent),
            variables: HashMap::new(),
            types: HashMap::new(),
            impls: HashMap::new(),
            scopes: scopes.into_iter()
                .map(|scope| scope.into())
                .collect::<Vec<Scope>>(),
        }
    }

    pub fn has_scope(&self, scope_type: &ScopeType) -> bool {
        self.scopes.iter().any(|s| s.scope_type == *scope_type) ||
            self.parent.as_ref().map(|p| p.borrow().has_scope(scope_type)).unwrap_or(false)
    }

    pub fn get_scope(&self, scope_type: &ScopeType) -> Option<Scope> {
        self.scopes.iter().find(|s| s.scope_type == *scope_type && s.active())
            .map(|f: &Scope| f.clone())
            .or_else(|| self.parent.as_ref().and_then(|p|
                p.borrow().get_scope(scope_type)))
    }

    pub fn activate_scope(&mut self, scope_type: ScopeType, type_: Type) -> Result<(), String> {
        if !self.has_scope(&scope_type) {
            return Err(format!("Scope '{:?}' not found", scope_type));
        }

        match self.scopes.iter_mut().find(|s| s.scope_type == scope_type) {
            Some(scope_state) => {
                scope_state.types.push(type_)
            },
            None => self.parent.as_mut()
                .expect("Already checked if the scope exists, if it's not in the current environment, it must be in the parent")
                .borrow_mut()
                .activate_scope(scope_type, type_)?,
        }
        
        Ok(())
    }

    pub fn add_type(&mut self, type_: Type) -> Result<(), String> {
        let full_name = type_.full_name();
        if self.types.contains_key(&full_name) {
            return Err(format!("Type {} already exists", type_.full_name()));
        }

        self.types.insert(type_.full_name(), type_);
        Ok(())
    }

    pub fn add_impl_function(&mut self, type_: Type, function_name: TypeName, function_type: Type) -> Result<(), String> {
        todo!()
    }

    pub fn add_variable(&mut self, name: String, type_: Type) {
        self.variables.insert(name, type_);
    }

    pub fn get_type_from_str(&self, type_str: &str) -> Option<Type> {
        todo!()          
    }

    pub fn get_type_from_annotation(&self, type_annotation: &TypeAnnotation) -> Option<Type> {
        match type_annotation {
            TypeAnnotation::Type(type_name) => {
                self.types.get(type_name).cloned()
                    .or(self.parent.as_ref()
                        .and_then(|parent| parent.borrow().get_type_from_annotation(type_annotation)))
            },
            TypeAnnotation::GenericType(type_name, _) =>{
                self.types.get(type_name).cloned()
                    .or(self.parent.as_ref()
                        .and_then(|parent| parent.borrow().get_type_from_annotation(type_annotation)))
            },
            TypeAnnotation::Array(type_annotation) => {
                self.get_type_from_annotation(type_annotation)
                    .map(|t| Type::Array(Box::new(t)))
            },
        }
    }

    pub fn get_type_from_name(&self, type_name: &TypeName) -> Option<Type> {
        todo!("get_type_from_name")
    }

    pub fn get_variable(&self, name: &str) -> Option<Type> {
        if let Some(type_) = self.variables.get(name) {
            Some(type_.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get_variable(name)
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
            self.parent.as_ref()
                .map_or(false, |parent|
                    parent.borrow().lookup_type(full_name))
    }
}
