use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    ast::{EnumVariant, Expression, Parameter, Statement},
    types::{GenericType, TypeAnnotation, TypeIdentifier},
};

use super::{model::TypedStatement, statements, type_environment::TypeEnvironment, Rcrc};

#[derive(Debug, Clone, PartialEq)]
pub struct DiscoveredEmbeddedStruct {
    pub type_annotation: TypeAnnotation,
    pub initialized_fields: Vec<(String, Expression)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DiscoveredType {
    Struct {
        type_identifier: TypeIdentifier,
        embedded_structs: Vec<DiscoveredEmbeddedStruct>,
        fields: HashMap<String, TypeAnnotation>,
    },
    Enum {
        type_identifier: TypeIdentifier,
        shared_fields: HashMap<String, TypeAnnotation>,
        members: Vec<EnumVariant>,
    },
    Union(TypeIdentifier, Vec<TypeAnnotation>),
    TypeAlias(TypeIdentifier, Vec<TypeAnnotation>),
    Protocol {
        type_identifier: TypeIdentifier,
        associated_types: Vec<TypeIdentifier>,
        function_identifiers: Vec<TypeIdentifier>,
    },
    Function {
        type_identifier: TypeIdentifier,
        param: Option<Parameter>,
        return_type_annotation: TypeAnnotation,
    },
    UseItem {
        type_identifier: TypeIdentifier,
    },
    /// A protocol implementation, kept so that overlapping implementations can
    /// be rejected before any type checking happens.
    Implementation {
        protocol_annotation: TypeAnnotation,
        type_annotation: TypeAnnotation,
        /// Names bound by `imp<..>`, which stand for any type rather than for
        /// a type of that name.
        scoped_generics: Vec<GenericType>,
    },
}

pub fn discover_user_defined_types(
    program: Statement,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<Vec<DiscoveredType>, String> {
    // Discover user-defined types. Only store their names and fields with type names.
    let discovered_types = statements::discover_user_defined_types(&program)?;

    check_implementations_do_not_overlap(&discovered_types)?;

    type_environment
        .borrow_mut()
        .set_discovered_types(discovered_types.clone());

    Ok(discovered_types)
}

/// Rejects two implementations of one protocol that could both apply to the
/// same type.
///
/// A type has at most one implementation of a protocol, so nothing is ever
/// chosen between — `B<Int>` and `B<String>` may each have their own, but a
/// blanket `B<T>` covers both and cannot sit alongside either.
///
/// This needs only the names written in the source, so it runs during
/// discovery, before anything is type checked.
fn check_implementations_do_not_overlap(discovered_types: &[DiscoveredType]) -> Result<(), String> {
    let aliases = alias_table(discovered_types);

    let implementations = discovered_types
        .iter()
        .filter_map(|discovered_type| match discovered_type {
            DiscoveredType::Implementation {
                protocol_annotation,
                type_annotation,
                scoped_generics,
            } => Some((protocol_annotation, type_annotation, scoped_generics)),
            _ => None,
        })
        .collect::<Vec<_>>();

    for (index, (protocol, type_annotation, scoped_generics)) in implementations.iter().enumerate()
    {
        for (other_protocol, other_type, other_scoped_generics) in
            implementations.iter().skip(index + 1)
        {
            // The protocol's own arguments matter as much as the target's:
            // `From<Q>` and `From<R>` are different protocols to implement.
            if !annotations_overlap(
                &resolve_aliases(protocol, &aliases),
                scoped_generics,
                &resolve_aliases(other_protocol, &aliases),
                other_scoped_generics,
            ) {
                continue;
            }

            if !annotations_overlap(
                &resolve_aliases(type_annotation, &aliases),
                scoped_generics,
                &resolve_aliases(other_type, &aliases),
                other_scoped_generics,
            ) {
                continue;
            }

            return Err(format!(
                "Conflicting implementations of `{}` for `{}` and `{}`: both can apply to the same type",
                protocol,
                type_annotation,
                other_type
            ));
        }
    }

    Ok(())
}

/// Single-type aliases by name, so `type MyInt = Int` cannot disguise one type
/// as two. Aliases of several types, and generic ones, are left alone.
fn alias_table(discovered_types: &[DiscoveredType]) -> HashMap<String, TypeAnnotation> {
    discovered_types
        .iter()
        .filter_map(|discovered_type| match discovered_type {
            DiscoveredType::TypeAlias(type_identifier, annotations) => match annotations.as_slice()
            {
                [single] if !matches!(type_identifier, TypeIdentifier::GenericType(_, _)) => {
                    Some((type_identifier.name().to_owned(), single.clone()))
                }
                _ => None,
            },
            _ => None,
        })
        .collect()
}

/// Replaces alias names with what they stand for, throughout the annotation.
fn resolve_aliases(
    annotation: &TypeAnnotation,
    aliases: &HashMap<String, TypeAnnotation>,
) -> TypeAnnotation {
    // An alias chain is finite, but a cyclic one would not be; the bound keeps
    // this total without needing to detect the cycle.
    let mut resolved = annotation.clone();

    for _ in 0..aliases.len().saturating_add(1) {
        let next = match &resolved {
            TypeAnnotation::Type(name) => match aliases.get(name) {
                Some(target) => target.clone(),
                None => break,
            },
            TypeAnnotation::ConcreteType(name, arguments) => TypeAnnotation::ConcreteType(
                name.clone(),
                arguments
                    .iter()
                    .map(|argument| resolve_aliases(argument, aliases))
                    .collect(),
            ),
            TypeAnnotation::Array(inner) => {
                TypeAnnotation::Array(Box::new(resolve_aliases(inner, aliases)))
            }
            TypeAnnotation::Tuple(elements) => TypeAnnotation::Tuple(
                elements
                    .iter()
                    .map(|element| resolve_aliases(element, aliases))
                    .collect(),
            ),
            other => other.clone(),
        };

        if next == resolved {
            break;
        }

        resolved = next;
    }

    resolved
}

/// Whether some type is covered by both annotations, treating each side's
/// scoped generics as standing for any type.
fn annotations_overlap(
    left: &TypeAnnotation,
    left_generics: &[GenericType],
    right: &TypeAnnotation,
    right_generics: &[GenericType],
) -> bool {
    let is_variable = |annotation: &TypeAnnotation, generics: &[GenericType]| match annotation {
        TypeAnnotation::Type(name) => generics.iter().any(|g| &g.type_name == name),
        _ => false,
    };

    // A type parameter stands for anything, so it covers whatever is opposite.
    if is_variable(left, left_generics) || is_variable(right, right_generics) {
        return true;
    }

    match (left, right) {
        (TypeAnnotation::Type(left), TypeAnnotation::Type(right)) => left == right,
        (TypeAnnotation::Array(left), TypeAnnotation::Array(right)) => {
            annotations_overlap(left, left_generics, right, right_generics)
        }
        (TypeAnnotation::Tuple(left), TypeAnnotation::Tuple(right)) => {
            left.len() == right.len()
                && left.iter().zip(right).all(|(left, right)| {
                    annotations_overlap(left, left_generics, right, right_generics)
                })
        }
        (
            TypeAnnotation::ConcreteType(left_name, left_arguments),
            TypeAnnotation::ConcreteType(right_name, right_arguments),
        ) => {
            left_name == right_name
                && left_arguments.len() == right_arguments.len()
                && left_arguments
                    .iter()
                    .zip(right_arguments)
                    .all(|(left, right)| {
                        annotations_overlap(left, left_generics, right, right_generics)
                    })
        }
        // `B` and `B<Int>` name the same type, since a name may not be reused
        // at a different arity.
        (TypeAnnotation::Type(name), TypeAnnotation::ConcreteType(other, _))
        | (TypeAnnotation::ConcreteType(other, _), TypeAnnotation::Type(name)) => name == other,
        _ => false,
    }
}

pub fn type_check_program(
    program: &Statement,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<TypedStatement, String> {
    statements::check_type(program, discovered_types, type_environment)
}

pub fn create_typed_ast(
    program: Statement,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<TypedStatement, String> {
    // Discover user-defined types. Only store their names and fields with type names.
    let discovered_types = statements::discover_user_defined_types(&program)?;

    check_implementations_do_not_overlap(&discovered_types)?;

    type_environment
        .borrow_mut()
        .set_discovered_types(discovered_types.clone());

    // Then check the types of the entire AST.
    statements::check_type(&program, &discovered_types, type_environment)
}

/// The names the core library puts in scope everywhere without an explicit
/// `use`.
///
/// Keeping it an explicit list rather than "everything the module declares"
/// means what is in scope by default is something you can read off, and the
/// compiler binds to these by name knowing exactly which they are.
pub const PRELUDE: &[&str] = &["Option", "Result"];

/// Type checks the core library into a module of its own and puts the prelude
/// names into `type_environment`.
///
/// The library declares `mod core;`, so its contents belong to that module
/// rather than to whoever loads it; everything outside reaches them either
/// through the prelude or by naming the module.
pub fn register_core(
    source_tokens: Vec<crate::lexer::token::Token>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<(TypedStatement, Rcrc<TypeEnvironment>), String> {
    let Some((_, module_path, module)) = crate::ast::discover_module(source_tokens)? else {
        return Err("The core library must declare the module it belongs to".to_string());
    };

    let core_type_environment = Rc::new(RefCell::new(TypeEnvironment::new(
        type_environment.borrow().allow_override_types,
    )));

    let typed = create_typed_ast(module, core_type_environment.clone())?;

    type_environment
        .borrow_mut()
        .add_module(module_path.clone(), core_type_environment.clone());

    for name in PRELUDE {
        type_environment
            .borrow_mut()
            .add_symbol(&module_path, *name)
            .map_err(|e| format!("The core library does not export `{name}`: {e}"))?;
    }

    Ok((typed, core_type_environment))
}

/// Puts the prelude names into `type_environment`, taking them from an already
/// registered core library.
///
/// Every module needs this: a module gets a type environment of its own, so
/// without it `Option` would be in scope in the main file and nowhere else.
pub fn add_prelude(
    type_environment: Rcrc<TypeEnvironment>,
    core_type_environment: &Rcrc<TypeEnvironment>,
) -> Result<(), String> {
    for name in PRELUDE {
        let type_ = core_type_environment
            .borrow()
            .get_type(*name)
            .ok_or_else(|| format!("The core library does not export `{name}`"))?;

        type_environment.borrow_mut().add_type(type_)?;
    }

    Ok(())
}
