use crate::ast::StatementKind;
use crate::diagnostic::{Diagnostic, Spanned};
use crate::type_checker::{
    purity::{carry_body_purity, purity_of},
    Purity,
};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
    vec,
};

use crate::{
    ast::{
        self, ImplementationDeclaration, ModPath, ModuleDeclaration, ProtocolDeclaration,
        Statement, StructData, UnionDeclaration, Use, UseItem,
    },
    types::{ToKey, TypeAnnotation, TypeIdentifier},
};

use super::{
    expressions, get_enum_member,
    model::{self, FieldInitializer, Typed, TypedExpression, TypedParameter, TypedStatement},
    scope::ScopeType,
    type_checker::DiscoveredType,
    type_environment::TypeEnvironment,
    type_equals, type_equals_coerce, DiscoveredEmbeddedStruct, EmbeddedStruct, Enum, Function,
    Parameter, Protocol, Rcrc, Struct, StructField, Type, TypeAlias, Union,
};

pub fn discover_user_defined_types(
    statement: &Statement,
) -> Result<Vec<DiscoveredType>, Diagnostic> {
    match &statement.kind {
        StatementKind::Program { statements } => {
            let mut discovered_types = vec![];

            for statement in statements {
                discovered_types.append(&mut discover_user_defined_types(statement)?);
            }

            Ok(discovered_types)
        }
        StatementKind::ModuleDeclaration(_) => Ok(vec![]),
        StatementKind::Use(Use { use_item }) => {
            discover_types_from_use_item(use_item, ModPath::root())
        }
        StatementKind::StructDeclaration(ast::StructDeclaration {
            body:
                StructData {
                    type_identifier,
                    embedded_structs,
                    fields,
                    ..
                },
            ..
        }) => Ok(vec![DiscoveredType::Struct {
            type_identifier: type_identifier.clone(),
            embedded_structs: embedded_structs
                .iter()
                .map(|e| DiscoveredEmbeddedStruct {
                    type_annotation: e.type_annotation.clone(),
                    initialized_fields: e
                        .field_initializers
                        .iter()
                        .map(|f| (f.identifier.clone(), f.initializer.clone()))
                        .collect(),
                })
                .collect(),
            fields: fields
                .iter()
                .map(|field| (field.identifier.clone(), field.type_annotation.clone()))
                .collect(),
        }]),
        StatementKind::EnumDeclaration(ast::EnumDeclaration {
            type_identifier,
            shared_fields,
            members,
            ..
        }) => {
            check_enum_shape(type_identifier, shared_fields, members)?;

            let enum_ = DiscoveredType::Enum {
                type_identifier: type_identifier.clone(),
                shared_fields: shared_fields
                    .iter()
                    .map(|field| (field.identifier.clone(), field.type_annotation.clone()))
                    .collect(),
                members: members.clone(),
            };

            let nested = discover_enum_variants(type_identifier, members);

            Ok(nested.into_iter().chain(Some(enum_)).collect())
        }
        StatementKind::UnionDeclaration(ast::UnionDeclaration {
            access_modifier: _,
            type_identifier,
            literals,
        }) => Ok(vec![DiscoveredType::Union(
            type_identifier.clone(),
            literals
                .iter()
                .map(|literal| TypeAnnotation::Literal(Box::new(literal.clone().into())))
                .collect(),
        )]),
        StatementKind::TypeAliasDeclaration(ast::TypeAliasDeclaration {
            access_modifier: _,
            type_identifier,
            type_annotations,
        }) => Ok(vec![DiscoveredType::TypeAlias(
            type_identifier.clone(),
            type_annotations.clone(),
        )]),
        StatementKind::ProtocolDeclaration(ProtocolDeclaration {
            access_modifier: _,
            type_identifier,
            associated_types,
            functions,
        }) => Ok(vec![DiscoveredType::Protocol {
            type_identifier: type_identifier.clone(),
            associated_types: associated_types
                .clone()
                .into_iter()
                .map(|at| at.type_identifier)
                .collect(),
            function_identifiers: functions
                .clone()
                .into_iter()
                .map(|f| f.type_identifier)
                .collect(),
        }]),
        StatementKind::ImplementationDeclaration(ImplementationDeclaration {
            scoped_generics,
            protocol_annotation,
            type_annotation,
            ..
        }) => Ok(vec![DiscoveredType::Implementation {
            protocol_annotation: protocol_annotation.clone(),
            type_annotation: type_annotation.clone(),
            scoped_generics: scoped_generics.clone(),
        }]),
        StatementKind::FunctionDeclaration(ast::FunctionDeclaration {
            type_identifier,
            param,
            return_type_annotation,
            ..
        }) => Ok(vec![DiscoveredType::Function {
            type_identifier: type_identifier.clone(),
            param: param.clone(),
            return_type_annotation: return_type_annotation
                .clone()
                .unwrap_or(Type::Void.type_annotation()),
        }]),
        StatementKind::Semi(_) => Ok(vec![]),
        StatementKind::Expression(_) => Ok(vec![]),
    }
}

fn discover_types_from_use_item(
    use_item: &ast::UseItem,
    mod_path: ModPath,
) -> Result<Vec<DiscoveredType>, Diagnostic> {
    match use_item {
        ast::UseItem::Item(item_name) => {
            let mut type_identifier = None;

            for component in mod_path.components() {
                let comp_ident = TypeIdentifier::Type(component.to_key());

                if let Some(ti) = type_identifier {
                    type_identifier =
                        Some(TypeIdentifier::ModType(Box::new(ti), Box::new(comp_ident)));
                } else {
                    type_identifier = Some(comp_ident);
                }
            }

            let item_ident = TypeIdentifier::Type(item_name.clone());

            let type_identifier = match type_identifier {
                Some(ti) => TypeIdentifier::ModType(Box::new(ti), Box::new(item_ident)),
                None => item_ident,
            };

            Ok(vec![DiscoveredType::UseItem { type_identifier }])
        }
        ast::UseItem::Navigation(mod_name, use_item) => {
            discover_types_from_use_item(use_item, mod_path.join(mod_name))
        }
        ast::UseItem::List(use_items) => Ok(use_items
            .iter()
            .map(|use_item| discover_types_from_use_item(use_item, mod_path.clone()))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()),
    }
}

/// Type checks one statement, pointing any error at it.
///
/// Same arrangement as [`expressions::check_type`]: the span is attached once,
/// here, and only fills a primary that is still empty — so an error from
/// somewhere deeper keeps the tighter span it already has.
pub fn check_type(
    statement: &Statement,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<TypedStatement, Diagnostic> {
    check_type_of(statement, discovered_types, type_environment).at(statement.span)
}

fn check_type_of(
    statement: &Statement,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<TypedStatement, Diagnostic> {
    match &statement.kind {
        StatementKind::Program { statements } => {
            let statements: Result<Vec<TypedStatement>, Diagnostic> = statements
                .iter()
                .map(|s| check_type(s, discovered_types, type_environment.clone()))
                .collect();

            Ok(TypedStatement::Program {
                statements: statements?,
            })
        }
        StatementKind::ModuleDeclaration(ModuleDeclaration {
            access_modifier,
            module_path,
        }) => Ok(TypedStatement::ModuleDeclaration {
            access_modifier: access_modifier
                .clone()
                .map(|access_modifier| access_modifier.into()),
            module_path: module_path.clone(),
            type_: Type::Never,
        }),
        StatementKind::Use(Use { use_item }) => {
            check_use_item(use_item, ModPath::root(), type_environment.clone())
        }
        StatementKind::StructDeclaration(ast::StructDeclaration {
            access_modifier: _,
            body:
                StructData {
                    type_identifier,
                    embedded_structs,
                    fields,
                },
            where_clause,
        }) => {
            let struct_type_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
                type_environment.clone(),
            )));

            if let TypeIdentifier::GenericType(_, generics) = type_identifier {
                for generic in generics {
                    struct_type_environment
                        .borrow_mut()
                        .add_type(Type::Generic(generic.clone()))?;
                }
            }

            // A bound both brings the protocol's functions into scope on the
            // parameter and is remembered, so instantiating the struct can
            // check the type argument against it.
            for constraint in where_clause {
                struct_type_environment
                    .borrow_mut()
                    .add_generic_constraint(constraint)?;
            }

            type_environment
                .borrow_mut()
                .add_generic_constraints(type_identifier.to_key(), where_clause.clone());

            let embedded_structs: Result<Vec<_>, Diagnostic> = embedded_structs
                .iter()
                .map(|e| {
                    let embedded_type = check_type_annotation(
                        &e.type_annotation,
                        discovered_types,
                        struct_type_environment.clone(),
                    )?;

                    Ok((embedded_type, e.field_initializers.clone()))
                })
                .collect();

            let embedded_structs = embedded_structs?;

            let fields: Result<Vec<model::StructField>, Diagnostic> = fields
                .iter()
                .map(|field| {
                    match check_type_annotation(
                        &field.type_annotation,
                        discovered_types,
                        struct_type_environment.clone(),
                    ) {
                        Ok(t) => Ok(model::StructField {
                            struct_identifier: type_identifier.clone(),
                            mutable: field.mutable,
                            identifier: field.identifier.clone(),
                            default_value: None,
                            type_: t,
                        }),
                        Err(e) => Err(e),
                    }
                })
                .collect();

            let mut fields = fields?;

            for (embedded_struct_type, field_initializers) in embedded_structs.iter().rev() {
                let Type::Struct(embedded_struct) = embedded_struct_type else {
                    return Err(Diagnostic::error("Embedded struct must be a struct"));
                };

                for field in embedded_struct.fields.clone() {
                    if fields.iter().any(|f| f.identifier == field.field_name) {
                        return Err(Diagnostic::error(format!(
                            "Embedded field {} already exists in struct {}",
                            field.field_name, type_identifier
                        )));
                    }

                    let default_value = field_initializers
                        .iter()
                        .find(|f| f.identifier == field.field_name)
                        .map(|f| {
                            expressions::check_type(
                                &f.initializer,
                                discovered_types,
                                type_environment.clone(),
                                None,
                            )
                        })
                        .transpose()?;

                    fields.insert(
                        0,
                        model::StructField {
                            struct_identifier: type_identifier.clone(),
                            mutable: false,
                            identifier: field.field_name.clone(),
                            default_value,
                            type_: field.field_type.clone(),
                        },
                    );
                }
            }

            let mut recursive_embedded_structs = embedded_structs.clone();

            for (embedded_struct, field_initializers) in &embedded_structs {
                let Type::Struct(Struct {
                    embedded_structs: es,
                    ..
                }) = embedded_struct
                else {
                    unreachable!("Expected struct, found {}", embedded_struct);
                };

                for embedded_struct in es {
                    recursive_embedded_structs.push((
                        check_type_annotation(
                            &embedded_struct.type_annotation,
                            discovered_types,
                            type_environment.clone(),
                        )?,
                        field_initializers.clone(),
                    ));
                }
            }

            let embedded_structs: Result<Vec<EmbeddedStruct>, Diagnostic> =
                recursive_embedded_structs
                    .iter()
                    .map(|(es, fis)| {
                        let mut field_initializers = vec![];

                        for fi in fis {
                            field_initializers.push(FieldInitializer {
                                identifier: fi.identifier.clone(),
                                initializer: expressions::check_type(
                                    &fi.initializer,
                                    discovered_types,
                                    type_environment.clone(),
                                    None,
                                )?,
                            })
                        }

                        Ok(EmbeddedStruct {
                            type_annotation: es.type_annotation(),
                            field_initializers,
                            type_: es.clone(),
                        })
                    })
                    .collect();

            let embedded_structs = embedded_structs?;

            let field_types: Result<Vec<StructField>, Diagnostic> = fields
                .clone()
                .iter()
                .map(|f| {
                    Ok(StructField {
                        struct_name: type_identifier.clone(),
                        field_name: f.identifier.clone(),
                        default_value: f.default_value.clone().map(|t| t.get_type()),
                        field_type: {
                            if let Some(default_value) = &f.default_value {
                                let default_type = default_value.get_type();

                                if !type_equals(&f.type_, &default_type) {
                                    return Err(Diagnostic::error(format!(
                                        "Default value for field '{}' must be of type '{}'",
                                        f.identifier, f.type_
                                    )));
                                }

                                default_type
                            } else {
                                f.type_.clone()
                            }
                        },
                    })
                })
                .collect();

            let type_ = Type::Struct(Struct {
                type_identifier: type_identifier.clone(),
                embedded_structs: embedded_structs.clone(),
                fields: field_types?,
            });

            type_environment.borrow_mut().add_type(type_.clone())?;

            Ok(TypedStatement::StructDeclaration(
                super::model::StructData {
                    type_identifier: type_identifier.clone(),
                    embedded_structs,
                    fields,
                    type_,
                },
            ))
        }
        StatementKind::EnumDeclaration(ast::EnumDeclaration {
            access_modifier: _,
            type_identifier,
            shared_fields,
            members,
            where_clause,
        }) => {
            let enum_type_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
                type_environment.clone(),
            )));

            if let TypeIdentifier::GenericType(_, generics) = type_identifier {
                for generic in generics {
                    enum_type_environment
                        .borrow_mut()
                        .add_type(Type::Generic(generic.clone()))?;
                }
            }

            for constraint in where_clause {
                enum_type_environment
                    .borrow_mut()
                    .add_generic_constraint(constraint)?;
            }

            type_environment
                .borrow_mut()
                .add_generic_constraints(type_identifier.to_key(), where_clause.clone());

            let shared_fields = check_enum_shared_fields(
                type_identifier,
                shared_fields,
                discovered_types,
                enum_type_environment.clone(),
            )?;

            let (members, member_types) = check_enum_variants(
                type_identifier,
                &shared_fields,
                members,
                discovered_types,
                type_environment.clone(),
                enum_type_environment.clone(),
            )?;

            let enum_type = Type::Enum(Enum {
                type_identifier: type_identifier.clone(),
                shared_fields: shared_fields
                    .iter()
                    .map(|sf| StructField {
                        struct_name: sf.struct_identifier.clone(),
                        field_name: sf.identifier.clone(),
                        default_value: sf.default_value.clone().map(|t| t.get_type()),
                        field_type: sf.type_.clone(),
                    })
                    .collect(),
                members: member_types,
            });

            type_environment.borrow_mut().add_type(enum_type.clone())?;

            Ok(TypedStatement::EnumDeclaration {
                type_identifier: type_identifier.clone(),
                shared_fields,
                members,
                type_: enum_type,
            })
        }
        StatementKind::UnionDeclaration(UnionDeclaration {
            access_modifier: _,
            type_identifier,
            literals,
        }) => {
            let union_type_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
                type_environment.clone(),
            )));

            if let TypeIdentifier::GenericType(_, generics) = type_identifier {
                for generic in generics {
                    union_type_environment
                        .borrow_mut()
                        .add_type(Type::Generic(generic.clone()))?;
                }
            }

            let literal_types = literals
                .iter()
                .map(|literal| {
                    check_type_annotation(
                        &TypeAnnotation::Literal(Box::new(literal.clone().into())),
                        discovered_types,
                        union_type_environment.clone(),
                    )
                })
                .collect::<Result<Vec<Type>, Diagnostic>>()?;

            let literal_type =
                literal_types
                    .iter()
                    .map(|t| t.unstrict())
                    .try_fold(Type::Never, |acc, t| {
                        if type_equals(&acc.clone(), &Type::Never) {
                            Ok(t.clone())
                        } else if !type_equals_coerce(&acc.clone(), &t) {
                            Err(format!(
                        "All literals in a union must have the same type. Expected {}, found {}",
                        acc, t
                    ))
                        } else {
                            Ok(acc)
                        }
                    })?;

            let type_ = Type::Union(Union {
                type_identifier: type_identifier.clone(),
                literal_type: Box::new(literal_type.clone()),
                literals: literal_types,
            });

            type_environment.borrow_mut().add_type(type_.clone())?;

            Ok(TypedStatement::UnionDeclaration {
                type_identifier: type_identifier.clone(),
                literals: literals
                    .clone()
                    .iter()
                    .map(|l| TypeAnnotation::Literal(Box::new(l.clone().into())))
                    .collect(),
                type_,
            })
        }
        StatementKind::TypeAliasDeclaration(ast::TypeAliasDeclaration {
            access_modifier: _,
            type_identifier,
            type_annotations,
        }) => {
            let type_decl_type_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
                type_environment.clone(),
            )));

            if let TypeIdentifier::GenericType(_, generics) = type_identifier {
                for generic in generics {
                    type_decl_type_environment
                        .borrow_mut()
                        .add_type(Type::Generic(generic.clone()))?;
                }
            }

            let types = type_annotations
                .iter()
                .map(|type_annotation| {
                    check_type_annotation(
                        type_annotation,
                        discovered_types,
                        type_decl_type_environment.clone(),
                    )
                })
                .collect::<Result<Vec<Type>, Diagnostic>>()?;

            let type_ = Type::TypeAlias(TypeAlias {
                type_identifier: type_identifier.clone(),
                types,
            });

            type_environment.borrow_mut().add_type(type_.clone())?;

            Ok(TypedStatement::TypeAliasDeclaration {
                type_identifier: type_identifier.clone(),
                type_annotations: type_annotations.clone(),
                type_,
            })
        }
        StatementKind::ProtocolDeclaration(ProtocolDeclaration {
            access_modifier: _,
            type_identifier,
            associated_types,
            functions,
        }) => {
            let protocol_type_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
                type_environment.clone(),
            )));

            let self_type = Type::Substitution {
                type_identifier: TypeIdentifier::Type("Self".to_owned()),
                actual_type: Box::new(Type::Unknown),
            };

            protocol_type_environment
                .borrow_mut()
                .add_type(self_type.clone())?;

            // An associated type is a projection on `Self`: inside the protocol
            // it is a type nobody has chosen yet, and each implementation
            // chooses it. It is named unqualified — `Self::Item` would collide
            // with an enum's variants, which `::` already spells.
            let associated_type_names: Vec<String> = associated_types
                .iter()
                .map(|a| a.type_identifier.name().to_owned())
                .collect();

            for name in &associated_type_names {
                let projection = Type::AssociatedType {
                    on: Box::new(self_type.clone()),
                    protocol: type_identifier.name().to_owned(),
                    name: name.clone(),
                };

                protocol_type_environment
                    .borrow_mut()
                    .add_type_alias(name.clone(), projection);
            }

            // A protocol's own type parameters have to be in scope for the
            // signatures that mention them, exactly as for a struct or enum.
            if let TypeIdentifier::GenericType(_, generics) = type_identifier {
                for generic in generics {
                    protocol_type_environment
                        .borrow_mut()
                        .add_type(Type::Generic(generic.clone()))?;
                }
            }

            let functions: Result<Vec<TypedStatement>, Diagnostic> = functions
                .clone()
                .into_iter()
                .map(|function| {
                    check_type(
                        &StatementKind::FunctionDeclaration(function).at(statement.span),
                        discovered_types,
                        Rc::clone(&protocol_type_environment),
                    )
                })
                .collect();

            let functions = functions?;
            let function_tuples = functions
                .iter()
                .map(|f| match f {
                    TypedStatement::FunctionDeclaration {
                        type_identifier: identifier,
                        param,
                        return_type,
                        body,
                        ..
                    } => {
                        let type_ = Type::Function(Function {
                            purity: body.as_ref().map_or(Purity::Impure, purity_of),
                            identifier: Some(identifier.clone()),
                            param: param.clone().map(|p| Parameter {
                                identifier: p.identifier,
                                type_: p.type_,
                            }),
                            return_type: Box::new(return_type.clone()),
                        });

                        (identifier.clone(), type_)
                    }
                    _ => unreachable!("Expected function declaration, found {}", f),
                })
                .collect();

            let type_ = Type::Protocol(Protocol {
                type_identifier: type_identifier.clone(),
                associated_types: associated_type_names.clone(),
                functions: function_tuples,
            });

            type_environment.borrow_mut().add_type(type_.clone())?;

            Ok(TypedStatement::ProtocolDeclaration {
                type_identifier: type_identifier.clone(),
                associated_types: associated_types.clone(),
                functions,
                type_,
            })
        }
        StatementKind::ImplementationDeclaration(ImplementationDeclaration {
            scoped_generics,
            protocol_annotation,
            type_annotation,
            associated_types,
            functions,
            where_clause,
        }) => {
            let implementation_type_environment = Rc::new(RefCell::new(
                TypeEnvironment::new_parent(type_environment.clone()),
            ));

            // add generics to type environment
            for generic in scoped_generics {
                implementation_type_environment
                    .borrow_mut()
                    .add_type(Type::Generic(generic.clone()))?;
            }

            // The bounds hold inside the implementation, so the protocol's
            // functions are available on the parameter there.
            for constraint in where_clause {
                implementation_type_environment
                    .borrow_mut()
                    .add_generic_constraint(constraint)?;
            }

            // check generics in protocol_annotation
            if let TypeAnnotation::ConcreteType(_, generics) = protocol_annotation {
                for generic in generics {
                    let generic_type = check_type_annotation(
                        generic,
                        discovered_types,
                        implementation_type_environment.clone(),
                    )?;

                    if !implementation_type_environment
                        .borrow()
                        .lookup_type(&generic_type)
                    {
                        return Err(Diagnostic::error(format!(
                            "Generic type '{}' not found in protocol annotation",
                            generic_type
                        )));
                    }
                }
            }

            // check generics in type_annotation
            if let TypeAnnotation::ConcreteType(_, generics) = type_annotation {
                for generic in generics {
                    let generic_type = check_type_annotation(
                        generic,
                        discovered_types,
                        implementation_type_environment.clone(),
                    )?;

                    if !implementation_type_environment
                        .borrow()
                        .lookup_type(&generic_type)
                    {
                        return Err(Diagnostic::error(format!(
                            "Generic type '{}' not found in type annotation",
                            generic_type
                        )));
                    }
                }
            }

            let imp_type = implementation_type_environment
                .borrow()
                .get_type_from_annotation(type_annotation)?;

            implementation_type_environment
                .borrow_mut()
                .add_type(Type::Substitution {
                    type_identifier: TypeIdentifier::Type("Self".to_owned()),
                    actual_type: Box::new(imp_type.clone()),
                })?;

            let protocol_type = implementation_type_environment
                .borrow()
                .get_type_from_annotation(protocol_annotation)?;

            let Type::Protocol(Protocol {
                functions: protocol_functions,
                associated_types: protocol_associated_types,
                ..
            }) = protocol_type.clone()
            else {
                return Err(Diagnostic::error(format!(
                    "Expected protocol, found {}",
                    protocol_type
                )));
            };

            let mut bound_associated_types = HashMap::new();

            for associated_type in associated_types {
                let name = associated_type.type_identifier.name().to_owned();

                if !protocol_associated_types.contains(&name) {
                    return Err(Diagnostic::error(format!(
                        "`{}` has no associated type `{}`",
                        protocol_annotation, name
                    )));
                }

                let Some(annotation) = &associated_type.default_type_annotation else {
                    return Err(Diagnostic::error(format!(
                        "Associated type `{}` needs a type: write `type {} = ..;`",
                        name, name
                    )));
                };

                // `E::Item` cannot mean both a variant and a projection: inside
                // the implementation the associated type would shadow the
                // variant, and outside the variant would shadow the
                // projection. Rejecting the collision is better than a name
                // that means different things in different places.
                if let Type::Enum(enum_) = imp_type.clone().unsubstitute() {
                    if get_enum_member(&enum_.members, &enum_.type_identifier, &name).is_some() {
                        return Err(Diagnostic::error(format!(
                            "`{}` has both a variant and an associated type named `{}`; `{}::{}` would be ambiguous",
                            type_annotation, name, type_annotation, name
                        )));
                    }
                }

                let bound = check_type_annotation(
                    annotation,
                    discovered_types,
                    implementation_type_environment.clone(),
                )?;

                implementation_type_environment
                    .borrow_mut()
                    .add_type_alias(name.clone(), bound.clone());

                bound_associated_types.insert(name, bound);
            }

            // Every associated type the protocol declares has to be chosen, or
            // a projection on this type would have nothing to resolve to.
            for name in &protocol_associated_types {
                if !bound_associated_types.contains_key(name) {
                    return Err(Diagnostic::error(format!(
                        "Implementation of `{}` for `{}` is missing associated type `{}`",
                        protocol_annotation, type_annotation, name
                    )));
                }
            }

            // `imp<T> P for B<T>` covers every `B`; `imp P for B<Int>` covers
            // only that one, and the two cannot both exist because overlapping
            // implementations are rejected during discovery.
            let covers_all_instantiations = !scoped_generics.is_empty();

            // Recorded once for the implementation itself, not per function, so
            // that implementing a protocol with no functions still counts.
            // An implementation written for a bare parameter applies to every
            // type, so it has no name to be filed under and is kept aside.
            let universal_target = match type_annotation {
                TypeAnnotation::Type(name)
                    if scoped_generics.iter().any(|g| &g.type_name == name) =>
                {
                    Some(name.clone())
                }
                _ => None,
            };

            if universal_target.is_none() {
                type_environment.borrow_mut().add_implementation(
                    &imp_type,
                    protocol_annotation.name().to_owned(),
                    covers_all_instantiations,
                    protocol_annotation.clone(),
                    type_annotation.clone(),
                    scoped_generics.clone(),
                    where_clause.clone(),
                    bound_associated_types.clone(),
                );
            }

            let mut typed_functions = vec![];
            let mut universal_members = HashMap::new();
            let mut universal_member_sources = HashMap::new();

            for (protocol_function_identifier, _) in protocol_functions {
                let function = functions
                    .iter()
                    .find(|f| f.type_identifier == protocol_function_identifier);

                let Some(function) = function else {
                    return Err(Diagnostic::error(format!(
                        "Protocol function '{}' not implemented",
                        protocol_function_identifier
                    )));
                };

                if function.body.is_none() {
                    return Err(Diagnostic::error(format!(
                        "Protocol function '{}' must have a body",
                        protocol_function_identifier
                    )));
                };

                let typed_function = check_type(
                    &StatementKind::FunctionDeclaration(function.clone()).at(statement.span),
                    discovered_types,
                    implementation_type_environment.clone(),
                )?;

                let function_name = protocol_function_identifier.name().to_owned();

                match &universal_target {
                    // Kept aside with the implementation, to be matched against
                    // whatever type is asked about rather than filed by name.
                    Some(_) => {
                        universal_members.insert(function_name.clone(), typed_function.get_type());
                        universal_member_sources.insert(function_name.clone(), function.clone());
                    }
                    None => type_environment.borrow_mut().add_static_member_covering(
                        imp_type.clone(),
                        function_name.clone(),
                        typed_function.get_type(),
                        covers_all_instantiations,
                    )?,
                }

                typed_functions.push((function_name, typed_function));
            }

            if let Some(target) = universal_target {
                type_environment.borrow_mut().add_universal_implementation(
                    protocol_annotation.name().to_owned(),
                    protocol_annotation.clone(),
                    target,
                    scoped_generics.clone(),
                    where_clause.clone(),
                    universal_members,
                    universal_member_sources,
                );
            }

            Ok(TypedStatement::ImplementationDeclaration {
                scoped_generics: scoped_generics.clone(),
                protocol_annotation: protocol_annotation.clone(),
                type_annotation: type_annotation.clone(),
                associated_types: vec![],
                functions: typed_functions,
                type_: Type::Never,
            })
        }
        StatementKind::FunctionDeclaration(ast::FunctionDeclaration {
            access_modifier: _,
            type_identifier,
            param,
            return_type_annotation,
            body,
            signature_only,
            where_clause,
        }) => {
            let function_type_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
                type_environment.clone(),
            )));

            if let TypeIdentifier::GenericType(_, generics) = type_identifier {
                for generic in generics {
                    function_type_environment
                        .borrow_mut()
                        .add_type(Type::Generic(generic.clone()))?;
                }
            }

            // A bound brings the protocol's functions into scope on the
            // parameter inside the body, and is remembered so that calling the
            // function can check the type argument against it.
            for constraint in where_clause {
                function_type_environment
                    .borrow_mut()
                    .add_generic_constraint(constraint)?;
            }

            type_environment
                .borrow_mut()
                .add_generic_constraints(type_identifier.to_key(), where_clause.clone());
            // A body that names a type parameter where a type belongs cannot
            // run as written, so its source is kept and a copy is specialised
            // at each call.
            if let TypeIdentifier::GenericType(_, generics) = type_identifier {
                if let Some(body) = body {
                    if expressions::dispatches_on_type_parameter(body, generics) {
                        type_environment.borrow_mut().add_generic_function(
                            type_identifier.name().to_owned(),
                            ast::FunctionDeclaration {
                                access_modifier: None,
                                type_identifier: type_identifier.clone(),
                                param: param.clone(),
                                return_type_annotation: return_type_annotation.clone(),
                                body: Some(body.clone()),
                                signature_only: *signature_only,
                                where_clause: where_clause.clone(),
                            },
                        );
                    }
                }
            }

            let return_type = check_type_annotation(
                &return_type_annotation
                    .clone()
                    .unwrap_or(TypeAnnotation::Type(Type::Void.to_string())),
                discovered_types,
                function_type_environment.clone(),
            )?;

            let body_environment = Rc::new(RefCell::new(TypeEnvironment::new_scope(
                function_type_environment.clone(),
                ScopeType::Return,
            )));

            let param: Option<Parameter> = match param {
                Some(param) => {
                    let param_type_annotation = param.type_annotation.clone();
                    let param_name = param.identifier.clone();

                    let param_type = match check_type_annotation(
                        &param_type_annotation,
                        discovered_types,
                        function_type_environment.clone(),
                    ) {
                        Ok(t) => {
                            body_environment
                                .borrow_mut()
                                .add_variable(param_name.clone(), t.clone());

                            Ok(t)
                        }
                        Err(e) => Err(e),
                    }?;

                    if param_name.is_empty() {
                        return Err(Diagnostic::error("Parameter must have a name"));
                    }

                    Some(Parameter {
                        identifier: param_name,
                        type_: Box::new(param_type),
                    })
                }
                None => None,
            };

            // The declared return type is what the body is expected to produce,
            // so it is pushed down as context. Without it an expression that
            // cannot type itself — an empty array literal, say — has nothing to
            // go on and lands on `{unknown}`.
            let body_typed_expression: Option<TypedExpression> = body
                .as_ref()
                .map(|body| {
                    expressions::check_type(
                        body,
                        discovered_types,
                        body_environment.clone(),
                        Some(return_type.clone()),
                    )
                })
                .transpose()?;

            if *signature_only {
                let type_ = Type::Function(Function {
                    // A requirement, not an implementation: there is no body
                    // here, and nothing can be assumed about the ones that will
                    // satisfy it.
                    purity: Purity::Impure,
                    identifier: Some(type_identifier.clone()),
                    param: param.clone(),
                    return_type: Box::new(return_type.clone()),
                });

                type_environment.borrow_mut().add_type(type_.clone())?;

                return Ok(TypedStatement::FunctionDeclaration {
                    type_identifier: type_identifier.clone(),
                    param: param.map(|p| TypedParameter {
                        identifier: p.identifier,
                        type_annotation: p.type_.type_annotation(),
                        type_: p.type_,
                    }),
                    return_type,
                    body: body_typed_expression,
                    type_,
                });
            }

            let return_scope = body_environment.borrow().get_scope(&ScopeType::Return);

            let type_ = Type::Function(Function {
                purity: body_typed_expression
                    .as_ref()
                    .map_or(Purity::Impure, purity_of),
                identifier: Some(type_identifier.clone()),
                param: param.clone(),
                return_type: Box::new(carry_body_purity(
                    return_type.clone(),
                    body_typed_expression.as_ref().map(|body| body.get_type()),
                )),
            });

            let Some(body_typed_expression) = body_typed_expression else {
                return Ok(TypedStatement::FunctionDeclaration {
                    type_identifier: type_identifier.clone(),
                    param: param.map(|p| TypedParameter {
                        identifier: p.identifier,
                        type_annotation: p.type_.type_annotation(),
                        type_: p.type_,
                    }),
                    return_type,
                    body: None,
                    type_,
                });
            };

            let body_type = return_scope
                .map(|s| s.fold())
                .unwrap_or_else(|| Ok(body_typed_expression.get_type()))?;

            // Asking *is this type Never* has to be a direct comparison, not
            // `type_equals`: `Never` satisfies every expectation, so
            // `type_equals(t, Never)` is true for every `t` and would wave the
            // whole check through.
            let declared_nothing = return_type == Type::Never || return_type == Type::Void;

            if !declared_nothing && !type_equals(&return_type, &body_type) {
                return Err(Diagnostic::error(format!(
                    "Function body's return type {} does not match function return type {}",
                    body_type, return_type
                )));
            }

            type_environment.borrow_mut().add_type(type_.clone())?;

            Ok(TypedStatement::FunctionDeclaration {
                type_identifier: type_identifier.clone(),
                param: param.map(|p| TypedParameter {
                    identifier: p.identifier,
                    type_annotation: p.type_.type_annotation(),
                    type_: p.type_,
                }),
                return_type,
                body: Some(body_typed_expression),
                type_,
            })
        }
        StatementKind::Semi(s) => Ok(TypedStatement::Semi(Box::new(check_type(
            s,
            discovered_types,
            type_environment,
        )?))),
        StatementKind::Expression(e) => Ok(TypedStatement::Expression(expressions::check_type(
            e,
            discovered_types,
            type_environment,
            None,
        )?)),
    }
}

fn check_use_item(
    use_item: &UseItem,
    mod_path: ModPath,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<TypedStatement, Diagnostic> {
    match use_item {
        UseItem::Item(item_name) => {
            type_environment
                .borrow_mut()
                .add_symbol(mod_path, item_name)?;
        }
        UseItem::Navigation(mod_name, use_item) => {
            check_use_item(use_item, mod_path.join(mod_name), type_environment)?;
        }
        UseItem::List(use_items) => {
            use_items
                .iter()
                .map(|use_item| {
                    check_use_item(use_item, mod_path.clone(), type_environment.clone())
                })
                .collect::<Result<Vec<_>, Diagnostic>>()?;
        }
    }

    Ok(TypedStatement::Use {
        use_item: use_item.clone(),
        type_: Type::Never,
    })
}

pub fn check_type_annotation(
    type_annotation: &TypeAnnotation,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<Type, Diagnostic> {
    if let Ok(type_) = type_environment
        .borrow()
        .get_type_from_annotation(type_annotation)
    {
        return Ok(type_);
    }

    match discovered_types
        .iter()
        .find(|discovered_type| match discovered_type {
            DiscoveredType::Struct {
                type_identifier, ..
            } => type_identifier.name() == type_annotation.name(),
            DiscoveredType::Enum {
                type_identifier, ..
            } => type_identifier.name() == type_annotation.name(),
            DiscoveredType::Union(type_identifier, ..) => {
                type_identifier.name() == type_annotation.name()
            }
            DiscoveredType::TypeAlias(type_identifier, ..) => {
                type_identifier.name() == type_annotation.name()
            }
            DiscoveredType::Protocol {
                type_identifier, ..
            } => type_identifier.name() == type_annotation.name(),
            DiscoveredType::Function {
                type_identifier, ..
            } => type_identifier.name() == type_annotation.name(),
            DiscoveredType::UseItem { type_identifier } => {
                type_identifier.name() == type_annotation.name()
            }
            // Implementations name no type of their own.
            DiscoveredType::Implementation { .. } => false,
        }) {
        Some(DiscoveredType::Struct {
            type_identifier,
            embedded_structs,
            fields,
        }) => Ok(Type::Struct(Struct {
            type_identifier: type_identifier.clone(),
            embedded_structs: embedded_structs
                .iter()
                .map(|e| {
                    let mut field_initializers = vec![];

                    for (identifier, initializer) in &e.initialized_fields {
                        field_initializers.push(FieldInitializer {
                            identifier: identifier.clone(),
                            initializer: expressions::check_type(
                                initializer,
                                discovered_types,
                                type_environment.clone(),
                                None,
                            )?,
                        })
                    }

                    Ok(EmbeddedStruct {
                        type_annotation: e.type_annotation.clone(),
                        field_initializers,
                        type_: check_type_annotation(
                            &e.type_annotation,
                            discovered_types,
                            type_environment.clone(),
                        )?,
                    })
                })
                .collect::<Result<Vec<_>, Diagnostic>>()?,
            fields: {
                let mut map = Vec::new();

                for (identifier, type_annotation) in fields {
                    map.push(StructField {
                        struct_name: type_identifier.clone(),
                        field_name: identifier.clone(),
                        default_value: None,
                        field_type: check_type_annotation(
                            type_annotation,
                            discovered_types,
                            type_environment.clone(),
                        )?,
                    });
                }

                map
            },
        })),
        Some(DiscoveredType::Enum {
            type_identifier,
            shared_fields,
            members,
        }) => discovered_enum_type(
            type_identifier,
            shared_fields,
            members,
            discovered_types,
            type_environment,
        ),
        Some(DiscoveredType::Union(type_identifier, literals)) => {
            let literal_types = literals
                .iter()
                .map(|literal| {
                    check_type_annotation(literal, discovered_types, type_environment.clone())
                })
                .collect::<Result<Vec<Type>, Diagnostic>>()?;

            let literal_type =
                literal_types
                    .iter()
                    .map(|t| t.unstrict())
                    .try_fold(Type::Never, |acc, t| {
                        if type_equals(&acc.clone(), &Type::Never) {
                            Ok(t.clone())
                        } else if !type_equals_coerce(&acc.clone(), &t) {
                            Err(format!(
                        "All literals in a union must have the same type. Expected {}, found {}",
                        acc, t
                    ))
                        } else {
                            Ok(acc)
                        }
                    })?;

            Ok(Type::Union(Union {
                type_identifier: type_identifier.clone(),
                literal_type: Box::new(literal_type.clone()),
                literals: literal_types,
            }))
        }
        Some(DiscoveredType::TypeAlias(type_identifier, type_annotations)) => {
            let types = type_annotations
                .iter()
                .map(|literal| {
                    check_type_annotation(literal, discovered_types, type_environment.clone())
                })
                .collect::<Result<Vec<Type>, Diagnostic>>()?;

            Ok(Type::TypeAlias(TypeAlias {
                type_identifier: type_identifier.clone(),
                types,
            }))
        }
        Some(DiscoveredType::Protocol {
            type_identifier,
            associated_types,
            function_identifiers,
        }) => Ok(Type::Protocol(Protocol {
            type_identifier: type_identifier.clone(),
            associated_types: associated_types
                .iter()
                .map(|a| a.name().to_owned())
                .collect(),
            functions: function_identifiers
                .iter()
                .map(|f| {
                    (
                        f.clone(),
                        type_environment
                            .borrow()
                            .get_type_from_identifier(f)
                            .unwrap(),
                    )
                })
                .collect(),
        })),
        Some(DiscoveredType::Function {
            type_identifier,
            param,
            return_type_annotation,
        }) => {
            let param = match param {
                Some(param) => Some(Parameter {
                    identifier: param.identifier.clone(),
                    type_: Box::new(check_type_annotation(
                        &param.type_annotation,
                        discovered_types,
                        type_environment.clone(),
                    )?),
                }),
                None => None,
            };

            Ok(Type::Function(Function {
                // Built from a signature alone, with no body to look
                // at, so nothing can be concluded. Impure is the safe
                // answer: it only costs optimisation.
                purity: Purity::Impure,
                identifier: Some(type_identifier.clone()),
                param,
                return_type: Box::new(check_type_annotation(
                    return_type_annotation,
                    discovered_types,
                    type_environment,
                )?),
            }))
        }
        Some(DiscoveredType::UseItem { .. }) => Ok(Type::Never),
        // Implementations are not types, and are never found by name.
        Some(DiscoveredType::Implementation { .. }) => Ok(Type::Never),
        None => type_environment
            .borrow()
            .get_type_from_annotation(type_annotation),
    }
}

/// Rejects an enum whose shape is invalid, at every level of nesting.
///
/// Beyond the duplicate-field checks that have always been here, this enforces
/// the rule that makes nested enums tractable: an enum that declares an enum
/// variant declares no shared fields. A shared field has to exist on every
/// variant, and a nested enum is not a place to put one.
fn check_enum_shape(
    type_identifier: &TypeIdentifier,
    shared_fields: &[ast::StructField],
    members: &[ast::EnumVariant],
) -> Result<(), Diagnostic> {
    // An enum with no variants has no values, so nothing could ever construct
    // or match one. Rejecting it here is what lets exhaustiveness checking
    // assume every enum is inhabited.
    if members.is_empty() {
        return Err(Diagnostic::error(format!(
            "Enum '{}' has no variants; an enum with no variants has no values",
            type_identifier
        )));
    }

    let mut shared_field_identifiers = HashSet::new();

    for field in shared_fields {
        if !shared_field_identifiers.insert(field.identifier.clone()) {
            return Err(Diagnostic::error(format!(
                "Shared field '{}' previously defined in shared fields of enum '{}'",
                field.identifier, type_identifier
            )));
        }
    }

    if !shared_fields.is_empty() {
        if let Some(nested) = members
            .iter()
            .find(|member| matches!(member, ast::EnumVariant::Enum(_)))
        {
            return Err(Diagnostic::error(format!(
                "Enum '{}' declares shared fields and the enum variant '{}'; an enum with a nested enum cannot declare shared fields",
                type_identifier,
                nested.type_identifier()
            )));
        }
    }

    let mut variant_identifiers = HashSet::new();

    for member in members {
        if !variant_identifiers.insert(member.type_identifier().to_key()) {
            return Err(Diagnostic::error(format!(
                "Variant '{}' previously defined in enum '{}'",
                member.type_identifier(),
                type_identifier
            )));
        }

        match member {
            ast::EnumVariant::Struct(data) => {
                let mut field_identifiers = HashSet::new();

                for field in &data.fields {
                    if shared_field_identifiers.contains(&field.identifier) {
                        return Err(Diagnostic::error(format!(
                            "Field '{}' previously defined in shared fields of enum '{}'",
                            field.identifier, data.type_identifier
                        )));
                    }

                    if !field_identifiers.insert(field.identifier.clone()) {
                        return Err(Diagnostic::error(format!(
                            "Field '{}' previously defined in member '{}' of enum '{}'",
                            field.identifier, data.type_identifier, type_identifier
                        )));
                    }
                }
            }
            ast::EnumVariant::Enum(data) => check_enum_shape(
                &TypeIdentifier::MemberType(
                    Box::new(type_identifier.clone()),
                    data.type_identifier.to_key(),
                ),
                &data.shared_fields,
                &data.variants,
            )?,
        }
    }

    Ok(())
}

/// One `DiscoveredType` per variant, at every depth, so a variant is findable
/// as a type by its own qualified name.
///
/// A struct variant is discovered as a struct, a nested enum as an enum *and*
/// recursively for everything inside it.
fn discover_enum_variants(
    owner: &TypeIdentifier,
    members: &[ast::EnumVariant],
) -> Vec<DiscoveredType> {
    let mut discovered = vec![];

    for member in members {
        let member_identifier =
            TypeIdentifier::MemberType(Box::new(owner.clone()), member.type_identifier().to_key());

        match member {
            ast::EnumVariant::Struct(data) => discovered.push(DiscoveredType::Struct {
                type_identifier: member_identifier,
                embedded_structs: data
                    .embedded_structs
                    .iter()
                    .map(|e| DiscoveredEmbeddedStruct {
                        type_annotation: e.type_annotation.clone(),
                        initialized_fields: e
                            .field_initializers
                            .iter()
                            .map(|f| (f.identifier.clone(), f.initializer.clone()))
                            .collect(),
                    })
                    .collect(),
                fields: data
                    .fields
                    .iter()
                    .map(|field| (field.identifier.clone(), field.type_annotation.clone()))
                    .collect(),
            }),
            ast::EnumVariant::Enum(data) => {
                discovered.extend(discover_enum_variants(&member_identifier, &data.variants));

                discovered.push(DiscoveredType::Enum {
                    type_identifier: member_identifier,
                    shared_fields: data
                        .shared_fields
                        .iter()
                        .map(|field| (field.identifier.clone(), field.type_annotation.clone()))
                        .collect(),
                    members: data.variants.clone(),
                });
            }
        }
    }

    discovered
}

/// Checks an enum's shared fields, which every one of its variants carries.
fn check_enum_shared_fields(
    owner: &TypeIdentifier,
    shared_fields: &[ast::StructField],
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<Vec<model::StructField>, Diagnostic> {
    shared_fields
        .iter()
        .map(|field| {
            let type_ = check_type_annotation(
                &field.type_annotation,
                discovered_types,
                type_environment.clone(),
            )?;

            Ok(model::StructField {
                struct_identifier: owner.clone(),
                mutable: field.mutable,
                identifier: field.identifier.clone(),
                default_value: None,
                type_,
            })
        })
        .collect()
}

/// Checks an enum's variants, registering each as a type of its own and
/// returning both the checked forms and the `members` map for the enum type.
///
/// A struct variant carries its enum's shared fields alongside its own. A
/// nested enum recurses: it becomes a `Type::Enum` under a chained
/// `MemberType`, and its variants are checked against *its* shared fields. The
/// two never interact, because an enum with a nested enum declares no shared
/// fields of its own (see `check_enum_shape`).
#[allow(clippy::type_complexity)]
fn check_enum_variants(
    owner: &TypeIdentifier,
    shared_fields: &[model::StructField],
    variants: &[ast::EnumVariant],
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
    enum_type_environment: Rcrc<TypeEnvironment>,
) -> Result<(Vec<model::EnumVariant>, HashMap<String, Type>), Diagnostic> {
    let mut checked = vec![];
    let mut member_types = HashMap::new();

    for variant in variants {
        let member_identifier =
            TypeIdentifier::MemberType(Box::new(owner.clone()), variant.type_identifier().to_key());

        let checked_variant = match variant {
            ast::EnumVariant::Struct(member) => model::EnumVariant::Struct(check_struct_variant(
                owner,
                &member_identifier,
                shared_fields,
                member,
                discovered_types,
                type_environment.clone(),
                enum_type_environment.clone(),
            )?),
            ast::EnumVariant::Enum(member) => {
                let nested_shared_fields = check_enum_shared_fields(
                    &member_identifier,
                    &member.shared_fields,
                    discovered_types,
                    enum_type_environment.clone(),
                )?;

                let (nested_variants, nested_member_types) = check_enum_variants(
                    &member_identifier,
                    &nested_shared_fields,
                    &member.variants,
                    discovered_types,
                    type_environment.clone(),
                    enum_type_environment.clone(),
                )?;

                let nested_type = Type::Enum(Enum {
                    type_identifier: member_identifier.clone(),
                    shared_fields: nested_shared_fields
                        .iter()
                        .map(|sf| StructField {
                            struct_name: sf.struct_identifier.clone(),
                            field_name: sf.identifier.clone(),
                            default_value: sf.default_value.clone().map(|t| t.get_type()),
                            field_type: sf.type_.clone(),
                        })
                        .collect(),
                    members: nested_member_types,
                });

                type_environment
                    .borrow_mut()
                    .add_type(nested_type.clone())?;

                model::EnumVariant::Enum(model::EnumData {
                    type_identifier: member_identifier.clone(),
                    shared_fields: nested_shared_fields,
                    variants: nested_variants,
                    type_: nested_type,
                })
            }
        };

        member_types.insert(member_identifier.to_key(), checked_variant.type_().clone());
        checked.push(checked_variant);
    }

    Ok((checked, member_types))
}

/// Checks one struct variant: its embedded structs, its own fields, and the
/// shared fields it inherits from the enum it belongs to.
fn check_struct_variant(
    owner: &TypeIdentifier,
    member_identifier: &TypeIdentifier,
    shared_fields: &[model::StructField],
    member: &ast::StructData,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
    enum_type_environment: Rcrc<TypeEnvironment>,
) -> Result<model::StructData, Diagnostic> {
    let embedded_structs: Result<Vec<_>, Diagnostic> = member
        .embedded_structs
        .iter()
        .map(|e| {
            let embedded_type = check_type_annotation(
                &e.type_annotation,
                discovered_types,
                enum_type_environment.clone(),
            )?;

            Ok((embedded_type, e.field_initializers.clone()))
        })
        .collect();

    let embedded_structs = embedded_structs?;

    let fields: Result<Vec<model::StructField>, Diagnostic> = member
        .fields
        .iter()
        .map(|field| {
            let type_ = check_type_annotation(
                &field.type_annotation,
                discovered_types,
                enum_type_environment.clone(),
            )?;

            Ok(model::StructField {
                struct_identifier: owner.clone(),
                mutable: field.mutable,
                identifier: field.identifier.clone(),
                default_value: None,
                type_,
            })
        })
        .collect();

    let mut fields = fields?;

    for (embedded_struct, field_initializers) in embedded_structs.iter().rev() {
        let Type::Struct(embedded_struct) = embedded_struct else {
            return Err(Diagnostic::error("Embedded struct must be a struct"));
        };

        for field in embedded_struct.fields.clone() {
            if fields.iter().any(|f| f.identifier == field.field_name) {
                return Err(Diagnostic::error(format!(
                    "Embedded field {} already exists in struct {}",
                    field.field_name, owner
                )));
            }

            let default_value = field_initializers
                .iter()
                .find(|f| f.identifier == field.field_name)
                .map(|f| {
                    expressions::check_type(
                        &f.initializer,
                        discovered_types,
                        type_environment.clone(),
                        None,
                    )
                })
                .transpose()?;

            fields.insert(
                0,
                model::StructField {
                    struct_identifier: owner.clone(),
                    mutable: false,
                    identifier: field.field_name.clone(),
                    default_value,
                    type_: field.field_type.clone(),
                },
            );
        }
    }

    let mut recursive_embedded_structs = embedded_structs.clone();

    for (embedded_struct, field_initializers) in &embedded_structs {
        let Type::Struct(Struct {
            embedded_structs: es,
            ..
        }) = embedded_struct
        else {
            unreachable!("Expected struct, found {}", embedded_struct);
        };

        for embedded_struct in es {
            recursive_embedded_structs.push((
                check_type_annotation(
                    &embedded_struct.type_annotation,
                    discovered_types,
                    type_environment.clone(),
                )?,
                field_initializers.clone(),
            ));
        }
    }

    let embedded_structs: Result<Vec<EmbeddedStruct>, Diagnostic> = recursive_embedded_structs
        .iter()
        .map(|(es, fis)| {
            let mut field_initializers = vec![];

            for fi in fis {
                field_initializers.push(FieldInitializer {
                    identifier: fi.identifier.clone(),
                    initializer: expressions::check_type(
                        &fi.initializer,
                        discovered_types,
                        type_environment.clone(),
                        None,
                    )?,
                })
            }

            Ok(EmbeddedStruct {
                type_annotation: es.type_annotation(),
                field_initializers,
                type_: es.clone(),
            })
        })
        .collect();

    let embedded_structs = embedded_structs?;

    let field_types = shared_fields
        .iter()
        .chain(fields.clone().iter())
        .cloned()
        .collect::<Vec<model::StructField>>();

    let enum_member = Type::Struct(Struct {
        type_identifier: member_identifier.clone(),
        embedded_structs: embedded_structs.clone(),
        fields: field_types
            .iter()
            .map(|ft| StructField {
                // The field belongs to the variant, not to the enum as a whole
                // — shared fields included.
                struct_name: member_identifier.clone(),
                field_name: ft.identifier.clone(),
                default_value: ft.default_value.as_ref().map(|t| t.get_type()),
                field_type: ft.type_.clone(),
            })
            .collect(),
    });

    type_environment
        .borrow_mut()
        .add_type(enum_member.clone())?;

    Ok(model::StructData {
        type_identifier: member_identifier.clone(),
        embedded_structs,
        fields,
        type_: enum_member,
    })
}

/// Builds a `Type::Enum` from what discovery recorded, recursing through nested
/// enums.
///
/// This is the shallow path taken when a type annotation is resolved before the
/// declaration itself has been checked; `check_enum_variants` is the one that
/// runs at the declaration and registers each variant as a type.
fn discovered_enum_type(
    type_identifier: &TypeIdentifier,
    shared_fields: &HashMap<String, TypeAnnotation>,
    members: &[ast::EnumVariant],
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<Type, Diagnostic> {
    let mut checked_shared_fields = Vec::new();

    for (identifier, type_annotation) in shared_fields {
        checked_shared_fields.push(StructField {
            struct_name: type_identifier.clone(),
            field_name: identifier.clone(),
            default_value: None,
            field_type: check_type_annotation(
                type_annotation,
                discovered_types,
                type_environment.clone(),
            )?,
        });
    }

    let mut member_types = HashMap::new();

    for member in members {
        let member_identifier = TypeIdentifier::MemberType(
            Box::new(type_identifier.clone()),
            member.type_identifier().to_key(),
        );

        let member_type = match member {
            ast::EnumVariant::Struct(data) => {
                let mut fields = Vec::new();

                for field in &data.fields {
                    fields.push(StructField {
                        struct_name: member_identifier.clone(),
                        field_name: field.identifier.clone(),
                        default_value: None,
                        field_type: check_type_annotation(
                            &field.type_annotation,
                            discovered_types,
                            type_environment.clone(),
                        )?,
                    });
                }

                let mut embedded_structs = Vec::new();

                for e in &data.embedded_structs {
                    let mut field_initializers = vec![];

                    for ast::model::FieldInitializer {
                        identifier,
                        initializer,
                    } in &e.field_initializers
                    {
                        field_initializers.push(FieldInitializer {
                            identifier: identifier.clone(),
                            initializer: expressions::check_type(
                                initializer,
                                discovered_types,
                                type_environment.clone(),
                                None,
                            )?,
                        })
                    }

                    embedded_structs.push(EmbeddedStruct {
                        type_annotation: e.type_annotation.clone(),
                        field_initializers,
                        type_: check_type_annotation(
                            &e.type_annotation,
                            discovered_types,
                            type_environment.clone(),
                        )?,
                    });
                }

                Type::Struct(Struct {
                    type_identifier: member_identifier.clone(),
                    embedded_structs,
                    fields,
                })
            }
            ast::EnumVariant::Enum(data) => {
                let nested_shared_fields = data
                    .shared_fields
                    .iter()
                    .map(|field| (field.identifier.clone(), field.type_annotation.clone()))
                    .collect();

                discovered_enum_type(
                    &member_identifier,
                    &nested_shared_fields,
                    &data.variants,
                    discovered_types,
                    type_environment.clone(),
                )?
            }
        };

        member_types.insert(member_identifier.to_key(), member_type);
    }

    Ok(Type::Enum(Enum {
        type_identifier: type_identifier.clone(),
        shared_fields: checked_shared_fields,
        members: member_types,
    }))
}
