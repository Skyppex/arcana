use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::types::{TypeAnnotation, TypeIdentifier};

use super::{scope::{Scope, ScopeType}, FullName, Type};

pub type Rcrc<T> = Rc<RefCell<T>>;

#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    parent: Option<Rcrc<TypeEnvironment>>,
    types: HashMap<TypeIdentifier, Type>,
    variables: HashMap<String, Type>,
    impls: HashMap<String, HashMap<String, Type>>,
    scopes: Vec<Scope>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            parent: None,
            types: HashMap::from([
                (TypeIdentifier::Type("void".to_string()), Type::Void),
                (TypeIdentifier::Type("unit".to_string()), Type::Unit),
                (TypeIdentifier::Type("bool".to_string()), Type::Bool),
                (TypeIdentifier::Type("i8".to_string()), Type::I8),
                (TypeIdentifier::Type("i16".to_string()), Type::I16),
                (TypeIdentifier::Type("i32".to_string()), Type::I32),
                (TypeIdentifier::Type("i64".to_string()), Type::I64),
                (TypeIdentifier::Type("i128".to_string()), Type::I128),
                (TypeIdentifier::Type("u8".to_string()), Type::U8),
                (TypeIdentifier::Type("u16".to_string()), Type::U16),
                (TypeIdentifier::Type("u32".to_string()), Type::U32),
                (TypeIdentifier::Type("u64".to_string()), Type::U64),
                (TypeIdentifier::Type("u128".to_string()), Type::U128),
                (TypeIdentifier::Type("f32".to_string()), Type::F32),
                (TypeIdentifier::Type("f64".to_string()), Type::F64),
                (TypeIdentifier::Type("char".to_string()), Type::Char),
                (TypeIdentifier::Type("string".to_string()), Type::String),
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
        if self.types.contains_key(&type_.type_identifier()) {
            return Err(format!("Type {} already exists", type_.full_name()));
        }

        self.types.insert(type_.type_identifier(), type_);
        Ok(())
    }

    pub fn add_impl_function(&mut self, type_: Type, function_name: TypeIdentifier, function_type: Type) -> Result<(), String> {
        todo!()
    }

    pub fn add_variable(&mut self, name: String, type_: Type) {
        self.variables.insert(name, type_);
    }

    pub fn get_type_from_str(&self, type_str: &str) -> Option<Type> {
        todo!()          
    }

    pub fn get_type_from_annotation(&self, type_annotation: &TypeAnnotation, type_environment: Rc<RefCell<TypeEnvironment>>) -> Result<Type, String> {
        match type_annotation {
            TypeAnnotation::Type(type_name) => {
                if let Some(t) = self.types.get(&TypeIdentifier::Type(type_name.clone())) {
                    Ok(t.clone())
                } else if let Some(parent) = &self.parent {
                    parent.borrow().get_type_from_annotation(type_annotation, type_environment)
                } else {
                    Err(format!("Type {} not found", type_name))
                }
            },
            TypeAnnotation::ConcreteType(type_name, concrete_types) => {
                if let Some((_, t)) = self.types.iter().find(|(k, _)| k.eq_names(&TypeIdentifier::GenericType(type_name.clone(), vec![]))) {
                    let concrete = t.clone_with_concrete_types(concrete_types.clone(), type_environment);
                    concrete
                } else if let Some(parent) = &self.parent {
                    parent.borrow().get_type_from_annotation(type_annotation, type_environment)
                } else {
                    Err(format!("Type {} not found", type_name))
                }
            },
            TypeAnnotation::Array(type_annotation) => {
                self.get_type_from_annotation(type_annotation, type_environment)
                    .map(|t| Type::Array(Box::new(t)))
            },
        }
    }

    pub fn get_type_from_identifier(&self, type_identifier: &TypeIdentifier) -> Option<Type> {
        todo!("get_type_from_identifier")
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

    pub fn get_types(&self) -> &HashMap<TypeIdentifier, Type> {
        &self.types
    }

    pub fn get_variables(&self) -> &HashMap<String, Type> {
        &self.variables
    }

    pub fn lookup_type(&self, type_: &Type) -> bool {
        self.types.values().into_iter().any(|t| t == type_) ||
            self.parent.as_ref()
                .map_or(false, |parent|
                    parent.borrow().lookup_type(type_))
    }
}
