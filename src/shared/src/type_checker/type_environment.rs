use std::{
    cell::RefCell,
    collections::HashMap,
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
    DiscoveredType, FullName, Function, Parameter, Struct, Type,
};

pub type Rcrc<T> = Rc<RefCell<T>>;

/// One protocol implementation, as far as satisfaction is concerned.
///
/// The type it was written for and the names bound by `imp<..>` are kept so
/// that a conditional implementation can bind its parameters against the type
/// being asked about before testing its bounds.
#[derive(Debug, Clone, PartialEq)]
struct ImplementationRecord {
    protocol_annotation: TypeAnnotation,
    type_annotation: TypeAnnotation,
    scoped_generics: Vec<GenericType>,
    where_clause: Vec<GenericConstraint>,
}

/// An implementation written for a bare type parameter — `imp<T> P for T` —
/// which applies to every type rather than to one named type.
///
/// It cannot be filed under a type's name like the others, so it is kept aside
/// and matched against whatever type is being asked about.
#[derive(Debug, Clone, PartialEq)]
struct UniversalImplementation {
    protocol: String,
    protocol_annotation: TypeAnnotation,
    /// The parameter the implementation was written for, bound to the type
    /// being asked about.
    target: String,
    scoped_generics: Vec<GenericType>,
    where_clause: Vec<GenericConstraint>,
    members: HashMap<String, Type>,
    /// The source of each member, so a copy can be specialised to the type it
    /// is used on — the body may dispatch on the implementation's parameters.
    member_sources: HashMap<String, crate::ast::FunctionDeclaration>,
}

#[derive(Clone, PartialEq)]
pub struct TypeEnvironment {
    parent: Option<Rcrc<TypeEnvironment>>,
    modules: HashMap<ModPath, Rcrc<TypeEnvironment>>,
    types: HashMap<String, Type>,
    discovered_types: Vec<DiscoveredType>,
    /// Static members by type, then by name. A name can hold several
    /// candidates, because one type may implement `From<A>` and `From<B>` and
    /// so provide two `from`s; they are told apart by their parameter type.
    static_members: HashMap<String, HashMap<String, Vec<Type>>>,
    /// Bounds declared in a `where` clause, by the type they belong to, so an
    /// instantiation can be checked against them.
    generic_constraints: HashMap<String, Vec<GenericConstraint>>,
    /// Which protocols each type implements, by protocol name. A protocol is
    /// satisfied by having an `imp`, not by happening to have the right
    /// methods, so this is recorded rather than inferred from the members.
    implementations: HashMap<String, HashMap<String, ImplementationRecord>>,
    /// Implementations written for a bare type parameter, which no type key can
    /// hold.
    universal_implementations: Vec<UniversalImplementation>,
    /// Source of generic functions whose body dispatches on a type parameter.
    /// Such a body cannot run as written — `T::show()` has no meaning once the
    /// program is running — so a copy is specialised per call.
    generic_functions: HashMap<String, crate::ast::FunctionDeclaration>,
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
            universal_implementations: Vec::new(),
            generic_functions: HashMap::new(),
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
            universal_implementations: Vec::new(),
            generic_functions: HashMap::new(),
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
            universal_implementations: Vec::new(),
            generic_functions: HashMap::new(),
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

    /// Binds a type under a name of its own, for substituting a type parameter
    /// while a specialised body is checked.
    pub fn add_type_alias(&mut self, name: String, type_: Type) {
        self.types.insert(name, type_);
    }

    /// Keeps a generic function's source for specialisation.
    pub fn add_generic_function(
        &mut self,
        name: String,
        declaration: crate::ast::FunctionDeclaration,
    ) {
        self.generic_functions.insert(name, declaration);
    }

    /// The source of a generic function, looked up through enclosing scopes.
    pub fn get_generic_function(&self, name: &str) -> Option<crate::ast::FunctionDeclaration> {
        self.generic_functions.get(name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().get_generic_function(name))
        })
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

        let candidates = self
            .static_members
            .entry(key)
            .or_default()
            .entry(name.clone())
            .or_default();

        // Two implementations may both provide `from` as long as they take
        // different arguments. Two that take the same could never be told
        // apart at a call, so they are rejected here rather than there.
        let already_present = candidates
            .iter()
            .any(|candidate| parameter_types_match(candidate, &member_type));

        if already_present {
            if self.allow_override_types {
                candidates.retain(|candidate| !parameter_types_match(candidate, &member_type));
            } else {
                return Err(format!(
                    "`{}` already provides `{}` taking the same argument",
                    type_, name
                ));
            }
        }

        candidates.push(member_type);
        Ok(())
    }

    /// Records that a type implements a protocol.
    ///
    /// `covers_all_instantiations` follows the same rule as static members: a
    /// blanket implementation is recorded against the constructor, one written
    /// for a single instantiation against that instantiation.
    #[allow(clippy::too_many_arguments)]
    pub fn add_implementation(
        &mut self,
        type_: &Type,
        protocol: String,
        covers_all_instantiations: bool,
        protocol_annotation: TypeAnnotation,
        type_annotation: TypeAnnotation,
        scoped_generics: Vec<GenericType>,
        where_clause: Vec<GenericConstraint>,
    ) {
        let key = if covers_all_instantiations {
            Self::constructor_key(type_)
        } else {
            Self::instantiation_key(type_)
        };

        self.implementations.entry(key).or_default().insert(
            protocol,
            ImplementationRecord {
                protocol_annotation,
                type_annotation,
                scoped_generics,
                where_clause,
            },
        );
    }

    /// Records an implementation written for a bare type parameter, which
    /// applies to every type rather than to one named type.
    #[allow(clippy::too_many_arguments)]
    pub fn add_universal_implementation(
        &mut self,
        protocol: String,
        protocol_annotation: TypeAnnotation,
        target: String,
        scoped_generics: Vec<GenericType>,
        where_clause: Vec<GenericConstraint>,
        members: HashMap<String, Type>,
        member_sources: HashMap<String, crate::ast::FunctionDeclaration>,
    ) {
        self.universal_implementations
            .push(UniversalImplementation {
                protocol,
                protocol_annotation,
                target,
                scoped_generics,
                where_clause,
                members,
                member_sources,
            });
    }

    /// The source of a member a type gets from a universal implementation,
    /// with the implementation's parameters bound for this type.
    pub fn universal_member_source(
        &self,
        type_: &Type,
        member_key: &str,
    ) -> Option<(
        crate::ast::FunctionDeclaration,
        HashMap<String, TypeAnnotation>,
    )> {
        self.universal_implementations()
            .into_iter()
            .find_map(|universal| {
                let declaration = universal.member_sources.get(member_key)?.clone();
                let bindings = self.universal_applies(&universal, type_)?;

                Some((declaration, bindings))
            })
    }

    /// Every universal implementation in scope, innermost first.
    fn universal_implementations(&self) -> Vec<UniversalImplementation> {
        let mut all = self.universal_implementations.clone();

        if let Some(parent) = &self.parent {
            all.extend(parent.borrow().universal_implementations());
        }

        all
    }

    /// The types known to implement a protocol, matched with its arguments —
    /// which types are `From<Point3>`, not merely which are `From` of
    /// something.
    fn types_implementing(&self, protocol: &TypeAnnotation) -> Vec<TypeAnnotation> {
        let mut found: Vec<TypeAnnotation> = self
            .implementations
            .values()
            .flat_map(|protocols| protocols.values())
            .filter(|record| annotations_match(&record.protocol_annotation, protocol))
            .map(|record| record.type_annotation.clone())
            .collect();

        if let Some(parent) = &self.parent {
            for candidate in parent.borrow().types_implementing(protocol) {
                if !found.contains(&candidate) {
                    found.push(candidate);
                }
            }
        }

        found
    }

    /// Whether a type implements a protocol, by name.
    ///
    /// A conditional implementation applies only to the instantiations that
    /// satisfy its bounds, so `imp<T> Show for B<T> where T is Show` makes
    /// `B<Dog>` showable but not `B<Int>`.
    pub fn implements(&self, type_: &Type, protocol: &str) -> bool {
        let record = self
            .implementations
            .get(&Self::instantiation_key(type_))
            .and_then(|protocols| protocols.get(protocol))
            .or_else(|| {
                self.implementations
                    .get(&Self::constructor_key(type_))
                    .and_then(|protocols| protocols.get(protocol))
            });

        if let Some(record) = record {
            return self.implementation_applies(record, type_);
        }

        if self
            .parent
            .as_ref()
            .is_some_and(|parent| parent.borrow().implements(type_, protocol))
        {
            return true;
        }

        // An implementation written for a bare parameter covers this type too,
        // as long as its bounds hold once the parameter is bound to it.
        self.universal_implementations()
            .iter()
            .filter(|universal| universal.protocol == protocol)
            .any(|universal| self.universal_applies(universal, type_).is_some())
    }

    /// Binds a universal implementation's parameters against a type, returning
    /// the bindings when its bounds hold.
    ///
    /// The target parameter is bound to the type itself; any other parameter is
    /// solved from the bounds — `T2 is From<T1>` is answered by looking for the
    /// types that implement `From<T1>`, and is only settled if exactly one
    /// does.
    fn universal_applies(
        &self,
        universal: &UniversalImplementation,
        type_: &Type,
    ) -> Option<HashMap<String, TypeAnnotation>> {
        let mut bindings = HashMap::new();
        bindings.insert(universal.target.clone(), type_.type_annotation());

        for constraint in &universal.where_clause {
            let name = &constraint.generic.type_name;

            if let Some(bound) = bindings.get(name).cloned() {
                // Already known, so the bound is a check rather than a search.
                let bound_type = self.get_type_from_annotation(&bound).ok()?;

                let satisfied = constraint.constraints.iter().all(|protocol| {
                    match self.get_type_from_annotation(protocol) {
                        Ok(Type::Protocol(Protocol {
                            type_identifier, ..
                        })) => self.implements(&bound_type, type_identifier.name()),
                        _ => true,
                    }
                });

                if !satisfied {
                    return None;
                }

                continue;
            }

            // Unknown, so solve it: exactly one type may satisfy every bound.
            let mut solutions: Option<Vec<TypeAnnotation>> = None;

            for protocol in &constraint.constraints {
                let wanted = substitute_annotation(protocol, &bindings);
                let implementers = self.types_implementing(&wanted);

                solutions = Some(match solutions {
                    None => implementers,
                    Some(previous) => previous
                        .into_iter()
                        .filter(|candidate| implementers.contains(candidate))
                        .collect(),
                });
            }

            match solutions.as_deref() {
                Some([only]) => {
                    bindings.insert(name.clone(), only.clone());
                }
                // None at all, or more than one, leaves it unsettled.
                _ => return None,
            }
        }

        Some(bindings)
    }

    /// Whether a conditional implementation's bounds hold for this type.
    fn implementation_applies(&self, record: &ImplementationRecord, type_: &Type) -> bool {
        if record.where_clause.is_empty() {
            return true;
        }

        let mut bindings = HashMap::new();

        bind_scoped_generics(
            &record.type_annotation,
            &type_.type_annotation(),
            &record.scoped_generics,
            &mut bindings,
        );

        record.where_clause.iter().all(|constraint| {
            let Some(argument) = bindings.get(&constraint.generic.type_name) else {
                // Nothing bound it, so there is nothing to disprove.
                return true;
            };

            let Ok(argument_type) = self.get_type_from_annotation(argument) else {
                return true;
            };

            constraint.constraints.iter().all(|bound| {
                let Ok(Type::Protocol(Protocol {
                    type_identifier, ..
                })) = self.get_type_from_annotation(bound)
                else {
                    return true;
                };

                self.implements(&argument_type, type_identifier.name())
            })
        })
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

                    // Inside the bound, `Self` is the constrained parameter, so
                    // `T::from(..)` returns a `T`.
                    let function_type = substitute_self(&function_type, &generic_type);

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

    pub fn get_static_members(&self) -> &HashMap<String, HashMap<String, Vec<Type>>> {
        &self.static_members
    }

    pub fn get_types(&self) -> &HashMap<String, Type> {
        &self.types
    }

    pub fn get_variables(&self) -> &HashMap<String, Type> {
        &self.variables
    }

    /// One static member of this name, for callers that only need to know it
    /// exists or expect a single candidate.
    pub fn get_static_member<K: ToKey>(&self, type_: &Type, member_key: K) -> Option<Type> {
        self.get_static_member_candidates(type_, member_key)
            .into_iter()
            .next()
    }

    /// Every static member of this name, which is more than one when several
    /// implementations provide it with different arguments.
    pub fn get_static_member_candidates<K: ToKey>(&self, type_: &Type, member_key: K) -> Vec<Type> {
        let member_key = member_key.to_key();

        let named = |key: &str| {
            self.static_members
                .get(key)
                .and_then(|members| members.get(&member_key))
                .cloned()
        };

        // An implementation written for this exact instantiation wins over one
        // written for every instantiation. The overlap check has already made
        // sure both cannot exist, so this is only about where to look.
        if let Some(candidates) = named(&Self::instantiation_key(type_)) {
            return candidates;
        }

        if let Some(candidates) = named(&Self::constructor_key(type_)) {
            return candidates;
        }

        if let Type::Struct(Struct {
            embedded_structs, ..
        }) = &type_
        {
            for embedded in embedded_structs {
                if let Some(candidates) = named(&embedded.to_key()) {
                    return candidates;
                }
            }
        }

        if type_.type_annotation().has_double_colon() {
            let type_annotation_name = type_.to_string();
            let type_name = type_annotation_name.split("::").collect::<Vec<_>>()[0];

            return named(type_name).unwrap_or_default();
        }

        if let Some(parent) = &self.parent {
            let inherited = parent
                .borrow()
                .get_static_member_candidates(type_, member_key.clone());

            if !inherited.is_empty() {
                return inherited;
            }
        }

        // Nothing is filed under this type's name, but an implementation
        // written for a bare parameter may still provide the member.
        self.universal_members(type_, &member_key)
    }

    /// Members this type gets from implementations written for a bare
    /// parameter, with the parameters and `Self` filled in.
    fn universal_members(&self, type_: &Type, member_key: &str) -> Vec<Type> {
        let mut found = vec![];

        for universal in self.universal_implementations() {
            let Some(member_type) = universal.members.get(member_key) else {
                continue;
            };

            let Some(bindings) = self.universal_applies(&universal, type_) else {
                continue;
            };

            let mut resolved = substitute_self(member_type, type_);

            for generic in &universal.scoped_generics {
                let Some(argument) = bindings.get(&generic.type_name) else {
                    continue;
                };

                let Ok(argument_type) = self.get_type_from_annotation(argument) else {
                    continue;
                };

                resolved = substitute_generic(&resolved, &generic.type_name, &argument_type);
            }

            found.push(resolved);
        }

        found
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

/// Binds an implementation's type parameters by matching the type it was
/// written for against the type being asked about — `B<T>` against `B<Dog>`
/// binds `T` to `Dog`.
fn bind_scoped_generics(
    pattern: &TypeAnnotation,
    actual: &TypeAnnotation,
    scoped_generics: &[GenericType],
    bindings: &mut HashMap<String, TypeAnnotation>,
) {
    if let TypeAnnotation::Type(name) = pattern {
        if scoped_generics.iter().any(|g| &g.type_name == name) {
            bindings.insert(name.clone(), actual.clone());
            return;
        }
    }

    match (pattern, actual) {
        (TypeAnnotation::Array(pattern), TypeAnnotation::Array(actual)) => {
            bind_scoped_generics(pattern, actual, scoped_generics, bindings)
        }
        (TypeAnnotation::Tuple(patterns), TypeAnnotation::Tuple(actuals)) => {
            for (pattern, actual) in patterns.iter().zip(actuals) {
                bind_scoped_generics(pattern, actual, scoped_generics, bindings);
            }
        }
        (TypeAnnotation::ConcreteType(_, patterns), TypeAnnotation::ConcreteType(_, actuals)) => {
            for (pattern, actual) in patterns.iter().zip(actuals) {
                bind_scoped_generics(pattern, actual, scoped_generics, bindings);
            }
        }
        _ => {}
    }
}

/// Replaces `Self` throughout a type with the type it stands for.
///
/// A protocol's signatures are written in terms of `Self`; once the protocol is
/// attached to something — an implementing type, or a constrained parameter —
/// `Self` is that something.
fn substitute_self(type_: &Type, self_type: &Type) -> Type {
    match type_ {
        Type::Substitution {
            type_identifier, ..
        } if type_identifier.name() == "Self" => self_type.clone(),
        Type::Function(Function {
            identifier,
            param,
            return_type,
        }) => Type::Function(Function {
            identifier: identifier.clone(),
            param: param.as_ref().map(|param| Parameter {
                identifier: param.identifier.clone(),
                type_: Box::new(substitute_self(&param.type_, self_type)),
            }),
            return_type: Box::new(substitute_self(return_type, self_type)),
        }),
        Type::Array(inner) => Type::Array(Box::new(substitute_self(inner, self_type))),
        Type::Tuple(types) => Type::Tuple(
            types
                .iter()
                .map(|type_| substitute_self(type_, self_type))
                .collect(),
        ),
        other => other.clone(),
    }
}

/// Whether two function types take the same argument, which is what makes two
/// members of the same name indistinguishable at a call.
fn parameter_types_match(left: &Type, right: &Type) -> bool {
    match (left, right) {
        (
            Type::Function(Function { param: left, .. }),
            Type::Function(Function { param: right, .. }),
        ) => match (left, right) {
            (Some(left), Some(right)) => left.type_.to_key() == right.type_.to_key(),
            (None, None) => true,
            _ => false,
        },
        _ => left.to_key() == right.to_key(),
    }
}

/// Whether two protocol annotations name the same protocol with the same
/// arguments.
fn annotations_match(left: &TypeAnnotation, right: &TypeAnnotation) -> bool {
    left.to_string() == right.to_string()
}

/// Replaces parameter names in an annotation with what they are bound to.
fn substitute_annotation(
    annotation: &TypeAnnotation,
    bindings: &HashMap<String, TypeAnnotation>,
) -> TypeAnnotation {
    match annotation {
        TypeAnnotation::Type(name) => bindings
            .get(name)
            .cloned()
            .unwrap_or_else(|| annotation.clone()),
        TypeAnnotation::ConcreteType(name, arguments) => TypeAnnotation::ConcreteType(
            name.clone(),
            arguments
                .iter()
                .map(|argument| substitute_annotation(argument, bindings))
                .collect(),
        ),
        TypeAnnotation::Array(inner) => {
            TypeAnnotation::Array(Box::new(substitute_annotation(inner, bindings)))
        }
        TypeAnnotation::Tuple(elements) => TypeAnnotation::Tuple(
            elements
                .iter()
                .map(|element| substitute_annotation(element, bindings))
                .collect(),
        ),
        other => other.clone(),
    }
}

/// Replaces one named type parameter throughout a type.
fn substitute_generic(type_: &Type, name: &str, replacement: &Type) -> Type {
    match type_ {
        Type::Generic(generic) if generic.type_name == name => replacement.clone(),
        Type::Function(Function {
            identifier,
            param,
            return_type,
        }) => Type::Function(Function {
            identifier: identifier.clone(),
            param: param.as_ref().map(|param| Parameter {
                identifier: param.identifier.clone(),
                type_: Box::new(substitute_generic(&param.type_, name, replacement)),
            }),
            return_type: Box::new(substitute_generic(return_type, name, replacement)),
        }),
        Type::Array(inner) => Type::Array(Box::new(substitute_generic(inner, name, replacement))),
        Type::Tuple(types) => Type::Tuple(
            types
                .iter()
                .map(|type_| substitute_generic(type_, name, replacement))
                .collect(),
        ),
        Type::Substitution {
            type_identifier,
            actual_type,
        } => Type::Substitution {
            type_identifier: type_identifier.clone(),
            actual_type: Box::new(substitute_generic(actual_type, name, replacement)),
        },
        other => other.clone(),
    }
}
