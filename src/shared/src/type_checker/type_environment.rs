use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    type_checker::Protocol,
    types::{GenericConstraint, GenericType, TypeAnnotation, TypeIdentifier},
};

use super::{
    scope::{Scope, ScopeType},
    FullName, Parameter, Type,
};

pub type Rcrc<T> = Rc<RefCell<T>>;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeEnvironment {
    parent: Option<Rcrc<TypeEnvironment>>,
    modules: Vec<Vec<String>>,
    types: HashMap<TypeIdentifier, Type>,
    static_members: HashMap<TypeAnnotation, HashMap<String, Type>>,
    variables: HashMap<String, Type>,
    scopes: Vec<Scope>,
    allow_override_types: bool,
}

impl TypeEnvironment {
    pub fn new(allow_override_types: bool) -> Self {
        Self {
            parent: None,
            modules: Vec::new(),
            types: HashMap::from([
                (TypeIdentifier::Type("Void".to_string()), Type::Void),
                (TypeIdentifier::Type("Unit".to_string()), Type::Unit),
                (TypeIdentifier::Type("Bool".to_string()), Type::Bool),
                (TypeIdentifier::Type("Int".to_string()), Type::Int),
                (TypeIdentifier::Type("UInt".to_string()), Type::UInt),
                (TypeIdentifier::Type("Float".to_string()), Type::Float),
                (TypeIdentifier::Type("Char".to_string()), Type::Char),
                (TypeIdentifier::Type("String".to_string()), Type::String),
            ]),
            static_members: HashMap::new(),
            variables: HashMap::new(),
            scopes: Vec::new(),
            allow_override_types,
        }
    }

    pub fn new_parent(parent: Rcrc<Self>) -> Self {
        let allow_override_types = parent.borrow().allow_override_types;

        Self {
            parent: Some(parent),
            modules: Vec::new(),
            types: HashMap::new(),
            static_members: HashMap::new(),
            variables: HashMap::new(),
            scopes: Vec::new(),
            allow_override_types,
        }
    }

    pub fn new_scope<T: Into<Scope>>(parent: Rcrc<Self>, scope: T) -> Self {
        Self::new_scopes(parent, [scope])
    }

    pub fn new_scopes<T: Into<Scope>, U: IntoIterator<Item = T>>(
        parent: Rcrc<Self>,
        scopes: U,
    ) -> Self {
        let allow_override_types = parent.borrow().allow_override_types;

        Self {
            parent: Some(parent),
            modules: Vec::new(),
            variables: HashMap::new(),
            types: HashMap::new(),
            static_members: HashMap::new(),
            scopes: scopes
                .into_iter()
                .map(|scope| scope.into())
                .collect::<Vec<Scope>>(),
            allow_override_types,
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

    pub fn add_module(&mut self, module_path: Vec<String>) {
        self.modules.push(module_path)
    }

    pub fn add_type(&mut self, type_: Type) -> Result<(), String> {
        if !self.allow_override_types && self.types.contains_key(&type_.type_identifier()) {
            return Err(format!("Type {} already exists", type_.full_name()));
        }

        self.types.insert(type_.type_identifier(), type_);
        Ok(())
    }

    pub fn add_variable(&mut self, name: String, type_: Type) {
        self.variables.insert(name, type_);
    }

    pub fn add_static_member(
        &mut self,
        type_annotation: TypeAnnotation,
        name: String,
        member_type: Type,
    ) -> Result<(), String> {
        if let Some(members) = self.static_members.get_mut(&type_annotation) {
            if !self.allow_override_types && members.contains_key(&name) {
                return Err(format!(
                    "Static member {} already exists in type {}",
                    name, type_annotation
                ));
            }

            members.insert(name, member_type);
        } else {
            let mut members = HashMap::new();
            members.insert(name, member_type);
            self.static_members.insert(type_annotation, members);
        }

        Ok(())
    }

    pub fn add_generic_constraint(&mut self, constraint: &GenericConstraint) -> Result<(), String> {
        let GenericConstraint {
            generic: GenericType { type_name },
            constraints,
        } = constraint;

        for constraint in constraints {
            let constraint_type = self.get_type_from_annotation(constraint)?;
            let Some(generic_type) = self.get_type_from_str(type_name) else {
                return Err(format!("Type {} not found", type_name));
            };

            let generic_annotation = generic_type.type_annotation();

            if let Type::Protocol(Protocol { functions, .. }) = constraint_type {
                for (function_identifier, function_type) in functions {
                    println!("Function Type: {:?}", function_type);
                    let name = function_identifier.name();

                    self.add_static_member(
                        generic_annotation.clone(),
                        name.to_owned(),
                        function_type,
                    )?;
                }
            }
        }

        Ok(())
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
    ) -> Result<Type, String> {
        match type_annotation {
            TypeAnnotation::Type(type_name) => {
                if let Some(t) = self.types.get(&TypeIdentifier::Type(type_name.clone())) {
                    Ok(t.clone())
                } else if type_name.contains("::") {
                    let parts: Vec<&str> = type_name.split("::").collect();
                    let type_name = parts[0];
                    let variant_name = parts[1];

                    let Some(t) = self.types.get(&TypeIdentifier::MemberType(
                        Box::new(TypeIdentifier::Type(type_name.to_string())),
                        variant_name.to_string(),
                    )) else {
                        return Err(format!("Type {} not found", type_name));
                    };

                    Ok(t.clone())
                } else if let Some(parent) = &self.parent {
                    parent.borrow().get_type_from_annotation(type_annotation)
                } else {
                    Err(format!("Type {} not found", type_name))
                }
            }
            TypeAnnotation::ConcreteType(type_name, concrete_types) => {
                if let Some((_, t)) = self.types.iter().find(|(k, _)| {
                    k.eq_names(&TypeIdentifier::GenericType(type_name.clone(), vec![]))
                }) {
                    let concrete = t.clone_with_concrete_types(
                        concrete_types.clone(),
                        Rc::new(RefCell::new(self.clone())),
                    );
                    concrete
                } else if let Some(parent) = &self.parent {
                    parent.borrow().get_type_from_annotation(type_annotation)
                } else {
                    Err(format!("Type {} not found", type_name))
                }
            }
            TypeAnnotation::Array(type_annotation) => self
                .get_type_from_annotation(type_annotation)
                .map(|t| Type::Array(Box::new(t))),
            TypeAnnotation::Literal(literal) => Ok(Type::from_literal(literal)?),
            TypeAnnotation::Function(param_type_annotation, return_type_annotation) => {
                let param_type = param_type_annotation
                    .as_ref()
                    .map(|p| {
                        self.get_type_from_annotation(&p)
                            .map_err(|e| format!("Error getting type from annotation: {}", e))
                    })
                    .transpose()?;

                let return_type = return_type_annotation
                    .clone()
                    .map(|rt| {
                        self.get_type_from_annotation(&rt)
                            .map_err(|e| format!("Error getting type from annotation: {}", e))
                    })
                    .transpose()?;

                Ok(Type::Function(super::Function {
                    identifier: None,
                    param: param_type.map(|pt| Parameter {
                        identifier: pt.full_name(),
                        type_: Box::new(pt),
                    }),
                    return_type: Box::new(return_type.unwrap_or(Type::Void)),
                }))
            }
        }
    }

    pub fn get_type_from_identifier(&self, _type_identifier: &TypeIdentifier) -> Option<Type> {
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

    pub fn get_static_member(
        &self,
        type_annotation: TypeAnnotation,
        member_name: &str,
    ) -> Option<Type> {
        self.static_members
            .get(&type_annotation)
            .and_then(|members| members.get(member_name))
            .cloned()
            .or_else(|| {
                if type_annotation.has_double_colon() {
                    let type_annotation_name = &type_annotation.to_string();
                    let parts = type_annotation_name.split("::").collect::<Vec<_>>();
                    let type_name = parts[0];

                    self.static_members
                        .get(&TypeAnnotation::Type(type_name.to_owned()))
                        .and_then(|members| members.get(member_name))
                        .cloned()
                } else {
                    self.parent
                        .as_ref()
                        .and_then(|p| p.borrow().get_static_member(type_annotation, member_name))
                }
            })
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
