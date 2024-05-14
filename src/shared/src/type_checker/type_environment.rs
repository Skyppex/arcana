use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::types::{GenericType, TypeAnnotation, TypeIdentifier};

use super::{
    scope::{Scope, ScopeType},
    FullName, Function, Type,
};

pub type Rcrc<T> = Rc<RefCell<T>>;

#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    parent: Option<Rcrc<TypeEnvironment>>,
    types: HashMap<TypeIdentifier, Type>,
    variables: HashMap<String, Type>,
    static_member_functions: HashMap<TypeAnnotation, HashMap<TypeIdentifier, Type>>,
    instance_member_functions: HashMap<TypeAnnotation, HashMap<TypeIdentifier, Type>>,
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
                (TypeIdentifier::Type("int".to_string()), Type::Int),
                (TypeIdentifier::Type("uint".to_string()), Type::UInt),
                (TypeIdentifier::Type("float".to_string()), Type::Float),
                (TypeIdentifier::Type("char".to_string()), Type::Char),
                (TypeIdentifier::Type("string".to_string()), Type::String),
                (
                    TypeIdentifier::GenericType(
                        "Option".to_string(),
                        vec![GenericType {
                            type_name: "T".to_string(),
                        }],
                    ),
                    Type::option(),
                ),
            ]),
            variables: HashMap::new(),
            static_member_functions: HashMap::new(),
            instance_member_functions: HashMap::new(),
            scopes: Vec::new(),
        }
    }

    pub fn new_parent(parent: Rcrc<Self>) -> Self {
        Self {
            parent: Some(parent),
            types: HashMap::new(),
            variables: HashMap::new(),
            static_member_functions: HashMap::new(),
            instance_member_functions: HashMap::new(),
            scopes: Vec::new(),
        }
    }

    pub fn new_scope<T: Into<Scope>>(parent: Rcrc<Self>, scope: T) -> Self {
        Self::new_scopes(parent, [scope])
    }

    pub fn new_scopes<T: Into<Scope>, U: IntoIterator<Item = T>>(
        parent: Rcrc<Self>,
        scopes: U,
    ) -> Self {
        Self {
            parent: Some(parent),
            variables: HashMap::new(),
            types: HashMap::new(),
            static_member_functions: HashMap::new(),
            instance_member_functions: HashMap::new(),
            scopes: scopes
                .into_iter()
                .map(|scope| scope.into())
                .collect::<Vec<Scope>>(),
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
            .find(|s| s.scope_type == *scope_type && s.active())
            .map(|f: &Scope| f.clone())
            .or_else(|| {
                self.parent
                    .as_ref()
                    .and_then(|p| p.borrow().get_scope(scope_type))
            })
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

    pub fn add_impl_function(
        &mut self,
        type_annotation: TypeAnnotation,
        function_name: TypeIdentifier,
        function_type: Type,
    ) -> Result<(), String> {
        let Type::Function(Function { parameters, .. }) = &function_type else {
            return Err("Impl function must be a function".to_string());
        };

        if let Some((param_name, _)) = parameters.iter().next() {
            if param_name == "self" {
                // There probably needs to be a check here
                return self.add_instance_member_function(
                    type_annotation,
                    function_name,
                    function_type,
                );
            }
        }

        return self.add_static_member_function(type_annotation, function_name, function_type);
    }

    fn add_instance_member_function(
        &mut self,
        type_annotation: TypeAnnotation,
        function_name: TypeIdentifier,
        function_type: Type,
    ) -> Result<(), String> {
        if !self
            .instance_member_functions
            .contains_key(&type_annotation)
        {
            self.instance_member_functions
                .insert(type_annotation.clone(), HashMap::new());
        }

        let functions = self
            .instance_member_functions
            .get_mut(&type_annotation)
            .expect("Just inserted");

        if functions.contains_key(&function_name) {
            return Err(format!("Function {} already exists", function_name));
        }

        functions.insert(function_name, function_type);
        Ok(())
    }

    fn add_static_member_function(
        &mut self,
        type_annotation: TypeAnnotation,
        function_name: TypeIdentifier,
        function_type: Type,
    ) -> Result<(), String> {
        if !self.static_member_functions.contains_key(&type_annotation) {
            self.static_member_functions
                .insert(type_annotation.clone(), HashMap::new());
        }

        let functions = self
            .static_member_functions
            .get_mut(&type_annotation)
            .expect("Just inserted");

        if functions.contains_key(&function_name) {
            return Err(format!("Function {} already exists", function_name));
        }

        functions.insert(function_name, function_type);
        Ok(())
    }

    pub fn add_variable(&mut self, name: String, type_: Type) {
        self.variables.insert(name, type_);
    }

    pub fn get_type_from_str(&self, type_str: &str) -> Option<Type> {
        self.types
            .get(&TypeIdentifier::Type(type_str.to_string()))
            .cloned()
            .or_else(|| {
                self.parent
                    .as_ref()
                    .and_then(|p| p.borrow().get_type_from_str(type_str))
            })
    }

    pub fn get_type_from_annotation(
        &self,
        type_annotation: &TypeAnnotation,
        type_environment: Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Type, String> {
        match type_annotation {
            TypeAnnotation::Type(type_name) => {
                if let Some(t) = self.types.get(&TypeIdentifier::Type(type_name.clone())) {
                    Ok(t.clone())
                } else if let Some(parent) = &self.parent {
                    parent
                        .borrow()
                        .get_type_from_annotation(type_annotation, type_environment)
                } else {
                    Err(format!("Type {} not found", type_name))
                }
            }
            TypeAnnotation::ConcreteType(type_name, concrete_types) => {
                if let Some((_, t)) = self.types.iter().find(|(k, _)| {
                    k.eq_names(&TypeIdentifier::GenericType(type_name.clone(), vec![]))
                }) {
                    let concrete =
                        t.clone_with_concrete_types(concrete_types.clone(), type_environment);
                    concrete
                } else if let Some(parent) = &self.parent {
                    parent
                        .borrow()
                        .get_type_from_annotation(type_annotation, type_environment)
                } else {
                    Err(format!("Type {} not found", type_name))
                }
            }
            TypeAnnotation::Array(type_annotation) => self
                .get_type_from_annotation(type_annotation, type_environment)
                .map(|t| Type::Array(Box::new(t))),
            TypeAnnotation::Literal(literal) => Ok(Type::from_literal(literal)?),
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
        self.types.values().into_iter().any(|t| t == type_)
            || self
                .parent
                .as_ref()
                .map_or(false, |parent| parent.borrow().lookup_type(type_))
    }

    pub fn lookup_type_str(&self, type_name: &str) -> bool {
        self.types
            .values()
            .into_iter()
            .any(|t| t.full_name() == type_name)
            || self
                .parent
                .as_ref()
                .map_or(false, |parent| parent.borrow().lookup_type_str(type_name))
    }
}
