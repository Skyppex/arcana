use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::{self, Debug},
    rc::Rc,
    str::FromStr,
};

use crate::{
    ast::ModPath,
    built_in::BuiltInFunction,
    type_checker::Protocol,
    types::{GenericConstraint, GenericType, ToKey, TypeAnnotation, TypeIdentifier},
};

use super::{
    scope::{Scope, ScopeType},
    DiscoveredType, FullName, Parameter, Struct, Type,
};

pub type Rcrc<T> = Rc<RefCell<T>>;

#[derive(Clone, PartialEq)]
pub struct TypeEnvironment {
    parent: Option<Rcrc<TypeEnvironment>>,
    modules: HashMap<ModPath, Rcrc<TypeEnvironment>>,
    types: HashMap<String, Type>,
    discovered_types: Vec<DiscoveredType>,
    static_members: HashMap<String, HashMap<String, Type>>,
    /// Bounds declared in a `where` clause, by the type they belong to, so an
    /// instantiation can be checked against them.
    generic_constraints: HashMap<String, Vec<GenericConstraint>>,
    /// Which protocols each type implements. A protocol is satisfied by having
    /// an `imp`, not by happening to have the right methods, so this is
    /// recorded rather than inferred from the members.
    implementations: HashMap<String, HashSet<String>>,
    variables: HashMap<String, Type>,
    scopes: Vec<Scope>,
    pub allow_override_types: bool,
}

impl TypeEnvironment {
    pub fn new(allow_override_types: bool) -> Self {
        Self {
            parent: None,
            modules: HashMap::new(),
            types: HashMap::from([
                ("Void".to_string(), Type::Void),
                ("Unit".to_string(), Type::Unit),
                ("Bool".to_string(), Type::Bool),
                ("Int".to_string(), Type::Int),
                ("UInt".to_string(), Type::UInt),
                ("Float".to_string(), Type::Float),
                ("Rune".to_string(), Type::Rune),
                ("String".to_string(), Type::String),
            ]),
            discovered_types: Vec::new(),
            static_members: HashMap::new(),
            generic_constraints: HashMap::new(),
            implementations: HashMap::new(),
            variables: HashMap::new(),
            scopes: Vec::new(),
            allow_override_types,
        }
    }

    pub fn new_parent(parent: Rcrc<Self>) -> Self {
        let allow_override_types = parent.borrow().allow_override_types;

        Self {
            parent: Some(parent),
            modules: HashMap::new(),
            types: HashMap::new(),
            discovered_types: Vec::new(),
            static_members: HashMap::new(),
            generic_constraints: HashMap::new(),
            implementations: HashMap::new(),
            variables: HashMap::new(),
            scopes: Vec::new(),
            allow_override_types,
        }
    }

    pub fn set_discovered_types(&mut self, discovered_types: Vec<DiscoveredType>) {
        self.discovered_types = discovered_types;
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
            modules: HashMap::new(),
            variables: HashMap::new(),
            types: HashMap::new(),
            discovered_types: Vec::new(),
            static_members: HashMap::new(),
            generic_constraints: HashMap::new(),
            implementations: HashMap::new(),
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
            .cloned()
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

    pub fn add_module(&mut self, module_path: ModPath, type_environment: Rcrc<Self>) {
        self.modules.insert(module_path, type_environment);
    }

    pub fn get_module<M: AsRef<ModPath>>(&self, module_path: M) -> Option<Rcrc<Self>> {
        self.modules.get(module_path.as_ref()).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|p| p.borrow().get_module(module_path))
        })
    }

    pub fn add_symbol(
        &mut self,
        mod_path: impl AsRef<ModPath>,
        item_name: impl ToKey,
    ) -> Result<(), String> {
        let mod_path = mod_path.as_ref();
        let item_name = item_name.to_key();

        let mod_type_environment = self
            .get_module(mod_path)
            .ok_or(format!("Module '{}' not found", mod_path))?;

        let type_ = mod_type_environment.borrow().get_type(&item_name);

        let Some(type_) = type_ else {
            return Err(format!("Type '{}' not found", item_name))?;
        };

        self.add_type(type_)
    }

    pub fn add_type(&mut self, type_: Type) -> Result<(), String> {
        if !self.allow_override_types && self.types.contains_key(&type_.to_key()) {
            return Err(format!("Type {} already exists", type_.full_name()));
        }

        self.types.insert(type_.to_key(), type_);
        Ok(())
    }

    pub fn add_variable(&mut self, name: String, type_: Type) {
        self.variables.insert(name, type_);
    }

    /// The key a type's own implementations are stored under — the full
    /// instantiation, so `B<Int>` and `B<String>` are kept apart.
    fn instantiation_key(type_: &Type) -> String {
        type_.type_annotation().to_string()
    }

    /// The key an implementation covering every instantiation is stored under —
    /// the bare constructor, shared by `B<Int>`, `B<String>` and the rest.
    fn constructor_key(type_: &Type) -> String {
        type_.to_key()
    }

    pub fn add_static_member(
        &mut self,
        type_: Type,
        name: String,
        member_type: Type,
    ) -> Result<(), String> {
        self.add_static_member_covering(type_, name, member_type, true)
    }

    /// Registers a static member.
    ///
    /// `covers_all_instantiations` is true for a blanket implementation, which
    /// is stored against the constructor so every instantiation finds it, and
    /// false for one written for a single instantiation such as `B<Int>`.
    pub fn add_static_member_covering(
        &mut self,
        type_: Type,
        name: String,
        member_type: Type,
        covers_all_instantiations: bool,
    ) -> Result<(), String> {
        let key = if covers_all_instantiations {
            Self::constructor_key(&type_)
        } else {
            Self::instantiation_key(&type_)
        };

        if let Some(members) = self.static_members.get_mut(&key) {
            if !self.allow_override_types && members.contains_key(&name) {
                return Err(format!(
                    "Static member {} already exists in type {}",
                    name, type_
                ));
            }

            members.insert(name, member_type);
        } else {
            let mut members = HashMap::new();
            members.insert(name, member_type);
            self.static_members.insert(key, members);
        }

        Ok(())
    }

    /// Records that a type implements a protocol.
    ///
    /// `covers_all_instantiations` follows the same rule as static members: a
    /// blanket implementation is recorded against the constructor, one written
    /// for a single instantiation against that instantiation.
    pub fn add_implementation(
        &mut self,
        type_: &Type,
        protocol: String,
        covers_all_instantiations: bool,
    ) {
        let key = if covers_all_instantiations {
            Self::constructor_key(type_)
        } else {
            Self::instantiation_key(type_)
        };

        self.implementations
            .entry(key)
            .or_default()
            .insert(protocol);
    }

    /// Whether a type implements a protocol, by name.
    pub fn implements(&self, type_: &Type, protocol: &str) -> bool {
        let implements_under = |key: &str| {
            self.implementations
                .get(key)
                .is_some_and(|protocols| protocols.contains(protocol))
        };

        implements_under(&Self::instantiation_key(type_))
            || implements_under(&Self::constructor_key(type_))
            || self
                .parent
                .as_ref()
                .is_some_and(|parent| parent.borrow().implements(type_, protocol))
    }

    /// Records the bounds declared for a type, so that instantiating it can
    /// check each type argument against them.
    pub fn add_generic_constraints(&mut self, key: String, constraints: Vec<GenericConstraint>) {
        if constraints.is_empty() {
            return;
        }

        self.generic_constraints.insert(key, constraints);
    }

    /// The bounds declared for a type, looked up through the parent chain.
    pub fn get_generic_constraints(&self, key: &str) -> Vec<GenericConstraint> {
        self.generic_constraints
            .get(key)
            .cloned()
            .or_else(|| {
                self.parent
                    .as_ref()
                    .map(|parent| parent.borrow().get_generic_constraints(key))
            })
            .unwrap_or_default()
    }

    pub fn add_generic_constraint(&mut self, constraint: &GenericConstraint) -> Result<(), String> {
        let GenericConstraint {
            generic: GenericType { type_name },
            constraints,
        } = constraint;

        for constraint in constraints {
            let constraint_type = self.get_type_from_annotation(constraint)?;
            let Some(generic_type) = self.get_type(type_name) else {
                return Err(format!("Type {} not found", type_name));
            };

            let generic_annotation = generic_type.type_annotation();

            if let Type::Protocol(Protocol { functions, .. }) = constraint_type {
                for (function_identifier, function_type) in functions {
                    let name = function_identifier.name();

                    self.add_static_member(
                        self.get_type_from_annotation(&generic_annotation)?,
                        name.to_owned(),
                        function_type,
                    )?;
                }
            }
        }

        Ok(())
    }

    pub fn get_type(&self, key: impl ToKey) -> Option<Type> {
        let key = &key.to_key();

        self.get_built_in_function(key)
            .or_else(|| self.types.get(key).cloned())
            .or_else(|| self.variables.get(key).cloned())
            .or_else(|| self.parent.as_ref().and_then(|p| p.borrow().get_type(key)))
    }

    pub fn get_type_from_annotation(
        &self,
        type_annotation: &TypeAnnotation,
    ) -> Result<Type, String> {
        match type_annotation {
            TypeAnnotation::Type(type_name) => self
                .types
                .get(type_name)
                .cloned()
                .or_else(|| {
                    type_name
                        .contains("::")
                        .then(|| {
                            let parts: Vec<&str> = type_name.split("::").collect();
                            let type_name = parts[0];
                            let variant_name = parts[1];

                            self.types
                                .get(
                                    &TypeIdentifier::MemberType(
                                        Box::new(TypeIdentifier::Type(type_name.to_string())),
                                        variant_name.to_string(),
                                    )
                                    .to_key(),
                                )
                                .cloned()
                        })
                        .flatten()
                })
                .or_else(|| {
                    self.parent
                        .as_ref()
                        .and_then(|p| p.borrow().get_type_from_annotation(type_annotation).ok())
                })
                .ok_or_else(|| format!("Type {} not found", type_name)),
            TypeAnnotation::ConcreteType(type_name, concrete_types) => {
                // The declaration may live in an enclosing scope, but the type
                // arguments are written here — `imp<T> P for B<T>` resolves `B`
                // outside while `T` is only in scope inside. So the declaration
                // is looked up through the parents and then substituted in
                // *this* environment.
                let Some(declaration) =
                    self.find_generic_declaration(type_name, concrete_types.len())
                else {
                    return Err(format!("Type {} not found", type_name));
                };

                declaration.clone_with_concrete_types(
                    concrete_types.clone(),
                    &self.discovered_types,
                    Rc::new(RefCell::new(self.clone())),
                    None,
                )
            }
            TypeAnnotation::Array(type_annotation) => self
                .get_type_from_annotation(type_annotation)
                .map(|t| Type::Array(Box::new(t))),
            TypeAnnotation::Literal(literal) => Ok(literal.get_type()),
            TypeAnnotation::Tuple(annotations) => {
                let types = annotations
                    .iter()
                    .map(|a| {
                        self.get_type_from_annotation(a)
                            .map_err(|e| format!("Error getting type from annotation: {}", e))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Type::Tuple(types))
            }
            TypeAnnotation::Function(param_type_annotation, return_type_annotation) => {
                let param_type = param_type_annotation
                    .as_ref()
                    .map(|p| {
                        self.get_type_from_annotation(p)
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

    pub fn get_type_from_identifier(&self, type_identifier: &TypeIdentifier) -> Option<Type> {
        match type_identifier {
            TypeIdentifier::Type(name) => Type::from_str(name).ok(),
            TypeIdentifier::GenericType(name, _) => Type::from_str(name).ok(),
            TypeIdentifier::ConcreteType(name, _) => Type::from_str(name).ok(),
            TypeIdentifier::MemberType(type_identifier, member_name) => self
                .get_type(format!(
                    "{}::{}",
                    type_identifier.to_key(),
                    member_name.to_key()
                ))
                .or_else(|| {
                    self.get_type(format!(
                        "{}.{}",
                        type_identifier.to_key(),
                        member_name.to_key()
                    ))
                }),
            TypeIdentifier::ModType(type_identifier, member_name) => self.get_type(format!(
                "{}::{}",
                type_identifier.to_key(),
                member_name.to_key()
            )),
        }
    }

    pub fn get_variable<K: ToKey>(&self, key: K) -> Option<Type> {
        if let Some(type_) = self.variables.get(&key.to_key()) {
            Some(type_.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get_variable(key)
        } else {
            None
        }
    }

    pub fn get_static_members(&self) -> &HashMap<String, HashMap<String, Type>> {
        &self.static_members
    }

    pub fn get_types(&self) -> &HashMap<String, Type> {
        &self.types
    }

    pub fn get_variables(&self) -> &HashMap<String, Type> {
        &self.variables
    }

    pub fn get_static_member<K: ToKey>(&self, type_: &Type, member_key: K) -> Option<Type> {
        let member_key = member_key.to_key();
        // An implementation written for this exact instantiation wins over one
        // written for every instantiation. The overlap check has already made
        // sure both cannot exist, so this is only about where to look.
        self.static_members
            .get(&Self::instantiation_key(type_))
            .and_then(|members| members.get(&member_key))
            .or_else(|| {
                self.static_members
                    .get(&Self::constructor_key(type_))
                    .and_then(|members| members.get(&member_key))
            })
            .cloned()
            .or_else(|| {
                let Type::Struct(Struct {
                    embedded_structs, ..
                }) = &type_
                else {
                    return None;
                };

                embedded_structs.iter().fold(None, |acc, es| {
                    if let Some(members) = self.static_members.get(&es.to_key()) {
                        members.get(&member_key).cloned()
                    } else {
                        acc
                    }
                })
            })
            .or_else(|| {
                if type_.type_annotation().has_double_colon() {
                    let type_annotation_name = &type_.to_string();
                    let parts = type_annotation_name.split("::").collect::<Vec<_>>();
                    let type_name = parts[0];

                    self.static_members
                        .get(type_name)
                        .and_then(|members| members.get(&member_key))
                        .cloned()
                } else {
                    self.parent
                        .as_ref()
                        .and_then(|p| p.borrow().get_static_member(type_, member_key))
                }
            })
    }

    /// The declaration of a generic type with this name and arity, searched
    /// through enclosing scopes without resolving it.
    ///
    /// Separated from resolution so that substitution happens in the scope that
    /// wrote the type arguments, not the one that holds the declaration.
    fn find_generic_declaration(&self, type_name: &str, arity: usize) -> Option<Type> {
        let key = TypeIdentifier::GenericType(
            type_name.to_owned(),
            vec![
                GenericType {
                    type_name: "T".to_string()
                };
                arity
            ],
        )
        .to_key();

        self.types
            .iter()
            .find(|(k, _)| **k == key)
            .map(|(_, t)| t.clone())
            .or_else(|| {
                self.parent
                    .as_ref()
                    .and_then(|parent| parent.borrow().find_generic_declaration(type_name, arity))
            })
    }

    pub fn lookup_type(&self, type_: &Type) -> bool {
        // Structural types are not registered in their own right; they are
        // known exactly when the types they are built from are.
        match type_ {
            Type::Array(inner) => return self.lookup_type(inner),
            Type::Tuple(types) => return types.iter().all(|t| self.lookup_type(t)),
            _ => {}
        }

        // Only declarations are registered, so an instantiation such as
        // `Foo<Int>` never matches one exactly. Its key is the bare name, which
        // the declaration `Foo<T>` shares.
        self.types
            .values()
            .any(|t| t == type_ || t.to_key() == type_.to_key())
            || self
                .parent
                .as_ref()
                .is_some_and(|parent| parent.borrow().lookup_type(type_))
    }

    pub fn lookup_type_str(&self, type_name: &str) -> bool {
        self.types.values().any(|t| t.full_name() == type_name)
            || self
                .parent
                .as_ref()
                .is_some_and(|parent| parent.borrow().lookup_type_str(type_name))
    }

    pub fn get_built_in_function(&self, key: impl ToKey) -> Option<Type> {
        BuiltInFunction::new(&key.to_key()).map(|b| b.type_)
    }
}

impl Debug for TypeEnvironment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let modules = self.modules.keys().collect::<Vec<_>>();

        f.debug_struct("TypeEnvironment")
            .field("parent", &self.parent)
            .field("modules", &modules)
            .field("types", &self.types)
            .field("static_members", &self.static_members)
            .field("variables", &self.variables)
            .field("scopes", &self.scopes)
            .field("allow_override_types", &self.allow_override_types)
            .finish()
    }
}
