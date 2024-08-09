use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    parser::{self, ModuleDeclaration, Statement, UnionDeclaration, Use},
    types::{TypeAnnotation, TypeIdentifier},
};

use super::{
    ast::{self, Typed, TypedExpression, TypedParameter, TypedStatement},
    expressions,
    scope::ScopeType,
    type_checker::DiscoveredType,
    type_environment::TypeEnvironment,
    type_equals, Enum, EnumMember, Function, Parameter, Rcrc, Struct, Type, TypeAlias, Union,
};

pub fn discover_user_defined_types(statement: &Statement) -> Result<Vec<DiscoveredType>, String> {
    match statement {
        Statement::Program { statements } => {
            let mut discovered_types = vec![];
            for statement in statements {
                discovered_types.append(&mut discover_user_defined_types(statement)?);
            }
            Ok(discovered_types)
        }
        Statement::ModuleDeclaration(_) => Ok(vec![]),
        Statement::Use(_) => Ok(vec![]),
        Statement::StructDeclaration(parser::StructDeclaration {
            access_modifier: _,
            type_identifier,
            where_clause: _,
            fields,
        }) => Ok(vec![DiscoveredType::Struct(
            type_identifier.clone(),
            fields
                .iter()
                .map(|field| (field.identifier.clone(), field.type_annotation.clone()))
                .collect(),
        )]),
        Statement::EnumDeclaration(parser::EnumDeclaration {
            access_modifier: _,
            type_identifier,
            shared_fields,
            members,
        }) => Ok(vec![DiscoveredType::Enum(
            type_identifier.clone(),
            shared_fields
                .iter()
                .map(|field| (field.identifier.clone(), field.type_annotation.clone()))
                .collect(),
            members
                .iter()
                .map(|member| {
                    (
                        member.identifier.clone(),
                        member
                            .fields
                            .iter()
                            .map(|field| (field.identifier.clone(), field.type_annotation.clone()))
                            .collect(),
                    )
                })
                .collect(),
        )]),
        Statement::UnionDeclaration(parser::UnionDeclaration {
            access_modifier: _,
            type_identifier,
            literals,
        }) => Ok(vec![DiscoveredType::Union(
            type_identifier.clone(),
            literals
                .iter()
                .map(|literal| TypeAnnotation::Literal(Box::new(literal.clone())))
                .collect(),
        )]),
        Statement::TypeAliasDeclaration(parser::TypeAliasDeclaration {
            access_modifier: _,
            type_identifier,
            type_annotations,
        }) => Ok(vec![DiscoveredType::TypeAlias(
            type_identifier.clone(),
            type_annotations.clone(),
        )]),
        Statement::FunctionDeclaration(parser::FunctionDeclaration {
            access_modifier: _,
            identifier,
            param,
            return_type_annotation,
            body: _,
        }) => Ok(vec![DiscoveredType::Function {
            type_identifier: identifier.clone(),
            param: param.clone(),
            return_type_annotation: return_type_annotation
                .clone()
                .unwrap_or(TypeAnnotation::Type(Type::Void.to_string())),
        }]),
        Statement::Semi(_) => Ok(vec![]),
        Statement::Expression(_) => Ok(vec![]),
    }
}

pub fn check_type<'a>(
    statement: &Statement,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<TypedStatement, String> {
    println!("Checking statement");
    match statement {
        Statement::Program { statements } => {
            let statements: Result<Vec<TypedStatement>, String> = statements
                .iter()
                .map(|s| check_type(s, discovered_types, type_environment.clone()))
                .collect();

            Ok(TypedStatement::Program {
                statements: statements?,
            })
        }
        Statement::ModuleDeclaration(ModuleDeclaration {
            access_modifier,
            module_path,
        }) => Ok(TypedStatement::ModuleDeclaration {
            access_modifier: access_modifier
                .clone()
                .map(|access_modifier| access_modifier.into()),
            module_path: module_path.clone(),
            type_: Type::Void,
        }),
        Statement::Use(Use { use_item }) => Ok(TypedStatement::Use {
            use_item: use_item.clone(),
            type_: Type::Void,
        }),
        Statement::StructDeclaration(parser::StructDeclaration {
            access_modifier: _,
            type_identifier,
            where_clause,
            fields,
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

            if let Some(where_clause) = where_clause {
                for constraint in where_clause {
                    if !struct_type_environment
                        .borrow()
                        .lookup_type_str(&constraint.generic.type_name)
                    {
                        return Err(format!(
                            "Generic type {} not found in struct declaration",
                            constraint.generic.type_name
                        ));
                    }
                }
            }

            let fields: Result<Vec<ast::StructField>, String> = fields
                .iter()
                .map(|field| {
                    match check_type_annotation(
                        &field.type_annotation,
                        &discovered_types,
                        struct_type_environment.clone(),
                    ) {
                        Ok(t) => Ok(ast::StructField {
                            mutable: field.mutable,
                            identifier: field.identifier.clone(),
                            type_: t,
                        }),
                        Err(e) => Err(e),
                    }
                })
                .collect();

            let field_types: Result<HashMap<String, Type>, String> = fields
                .clone()?
                .iter()
                .map(|f: &ast::StructField| Ok((f.identifier.clone(), f.type_.clone())))
                .collect();

            let type_ = Type::Struct(Struct {
                type_identifier: type_identifier.clone(),
                fields: field_types?,
            });

            type_environment.borrow_mut().add_type(type_.clone())?;

            Ok(TypedStatement::StructDeclaration {
                type_identifier: type_identifier.clone(),
                where_clause: where_clause.clone(),
                fields: fields?,
                type_,
            })
        }
        Statement::EnumDeclaration(parser::EnumDeclaration {
            access_modifier: _,
            type_identifier,
            shared_fields,
            members,
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

            let shared_fields: Result<Vec<ast::StructField>, String> = shared_fields
                .iter()
                .map(|field| {
                    match check_type_annotation(
                        &field.type_annotation,
                        &discovered_types,
                        enum_type_environment.clone(),
                    ) {
                        Ok(t) => Ok(ast::StructField {
                            mutable: field.mutable,
                            identifier: field.identifier.clone(),
                            type_: t,
                        }),
                        Err(e) => Err(e),
                    }
                })
                .collect();

            let members: Result<Vec<ast::EnumMember>, String> = members
                .iter()
                .map(|member| {
                    let fields: Result<Vec<ast::EnumMemberField>, String> = member
                        .fields
                        .iter()
                        .map(|field| {
                            match check_type_annotation(
                                &field.type_annotation,
                                &discovered_types,
                                enum_type_environment.clone(),
                            ) {
                                Ok(t) => Ok(ast::EnumMemberField {
                                    enum_name: type_identifier.clone(),
                                    discriminant_name: member.identifier.clone(),
                                    identifier: field.identifier.clone(),
                                    type_: t,
                                }),
                                Err(e) => Err(e),
                            }
                        })
                        .collect();

                    let field_types: Result<HashMap<String, Type>, String> = fields
                        .clone()?
                        .iter()
                        .map(|f| Ok((f.identifier.clone(), f.type_.clone())))
                        .collect();

                    let enum_member = Type::EnumMember(EnumMember {
                        enum_name: type_identifier.clone(),
                        discriminant_name: member.identifier.clone(),
                        fields: field_types?,
                    });

                    type_environment
                        .borrow_mut()
                        .add_type(enum_member.clone())?;

                    Ok(ast::EnumMember {
                        enum_name: type_identifier.clone(),
                        discriminant_name: member.identifier.clone(),
                        fields: fields.clone()?,
                        type_: enum_member,
                    })
                })
                .collect();

            let enum_type = Type::Enum(Enum {
                type_identifier: type_identifier.clone(),
                shared_fields: shared_fields
                    .clone()?
                    .iter()
                    .map(|f| (f.identifier.clone(), f.type_.clone()))
                    .collect(),
                members: members
                    .clone()?
                    .iter()
                    .map(|m| (m.discriminant_name.clone(), m.type_.clone()))
                    .collect(),
            });

            type_environment.borrow_mut().add_type(enum_type.clone())?;

            Ok(TypedStatement::EnumDeclaration {
                type_identifier: type_identifier.clone(),
                shared_fields: shared_fields?,
                members: members?,
                type_: enum_type,
            })
        }
        Statement::UnionDeclaration(UnionDeclaration {
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
                        &TypeAnnotation::Literal(Box::new(literal.clone())),
                        discovered_types,
                        union_type_environment.clone(),
                    )
                })
                .collect::<Result<Vec<Type>, String>>()?;

            let literal_type = literal_types.iter().fold(Ok(Type::Void), |acc, t| {
                let Type::Literal { type_, .. } = t else {
                    return Err(format!("Expected literal, found {}", t));
                };

                if type_equals(&acc.clone()?, &Type::Void) {
                    Ok(*type_.clone())
                } else if !type_equals(&acc.clone()?, type_) {
                    Err(format!(
                        "All literals in a union must have the same type. Expected {}, found {}",
                        acc.as_ref().unwrap(),
                        type_
                    ))
                } else {
                    acc
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
                    .map(|l| TypeAnnotation::Literal(Box::new(l.clone())))
                    .collect(),
                type_,
            })
        }
        Statement::TypeAliasDeclaration(parser::TypeAliasDeclaration {
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
                .collect::<Result<Vec<Type>, String>>()?;

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
        Statement::FunctionDeclaration(parser::FunctionDeclaration {
            access_modifier: _,
            identifier,
            param,
            return_type_annotation,
            body,
        }) => {
            println!("Checking function declaration");
            let function_type_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
                type_environment.clone(),
            )));

            if let TypeIdentifier::GenericType(_, generics) = identifier {
                println!("Adding generics");
                println!("{:?}", generics);
                for generic in generics {
                    function_type_environment
                        .borrow_mut()
                        .add_type(Type::Generic(generic.clone()))?;
                }
            }

            let return_type = check_type_annotation(
                &return_type_annotation
                    .clone()
                    .unwrap_or(TypeAnnotation::Type(Type::Void.to_string())),
                &discovered_types,
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
                        &discovered_types,
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
                        return Err("Parameter must have a name".to_string());
                    }

                    Some(Parameter {
                        identifier: param_name,
                        type_: Box::new(param_type),
                    })
                }
                None => None,
            };

            let body_typed_expression: TypedExpression =
                expressions::check_type(body, discovered_types, body_environment.clone(), None)?;

            let return_scope = body_environment.borrow().get_scope(&ScopeType::Return);

            let body_type = return_scope
                .map(|s| s.fold())
                .unwrap_or_else(|| Ok(body_typed_expression.get_deep_type()))?;

            if !type_equals(&return_type, &body_type) {
                return Err(format!(
                    "Function body's return type {} does not match function return type {}",
                    body_type, return_type
                ));
            }

            let type_ = Type::Function(Function {
                identifier: Some(identifier.clone()),
                param: param.clone(),
                return_type: Box::new(return_type.clone()),
            });

            type_environment.borrow_mut().add_type(type_.clone())?;

            Ok(TypedStatement::FunctionDeclaration {
                identifier: identifier.clone(),
                param: param.map(|p| TypedParameter {
                    identifier: p.identifier,
                    type_annotation: p.type_.type_annotation(),
                    type_: p.type_,
                }),
                return_type,
                body: body_typed_expression,
                type_,
            })
        }
        Statement::Semi(s) => Ok(TypedStatement::Semi(Box::new(check_type(
            s,
            discovered_types,
            type_environment,
        )?))),
        Statement::Expression(e) => Ok(TypedStatement::Expression(expressions::check_type(
            e,
            discovered_types,
            type_environment,
            None,
        )?)),
    }
}

#[allow(dead_code)]
fn check_type_identifier(
    type_identifier: &TypeIdentifier,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<Type, String> {
    if let Some(type_) = type_environment
        .borrow()
        .get_type_from_identifier(type_identifier)
    {
        return Ok(type_.clone());
    }

    match discovered_types
        .iter()
        .find(|discovered_type| match discovered_type {
            DiscoveredType::Struct(name, ..) => name == type_identifier,
            DiscoveredType::Enum(name, ..) => name == type_identifier,
            DiscoveredType::Union(name, ..) => name == type_identifier,
            DiscoveredType::TypeAlias(name, ..) => name == type_identifier,
            DiscoveredType::Function {
                type_identifier: name,
                ..
            } => name == type_identifier,
        }) {
        Some(DiscoveredType::Struct(type_identifier, fields)) => Ok(Type::Struct(Struct {
            type_identifier: type_identifier.clone(),
            fields: {
                let mut map = HashMap::new();

                for (identifier, type_annotation) in fields {
                    map.insert(
                        identifier.clone(),
                        check_type_annotation(
                            type_annotation,
                            discovered_types,
                            type_environment.clone(),
                        )?,
                    );
                }

                map
            },
        })),
        Some(DiscoveredType::Enum(type_identifier, shared_fields, members)) => {
            Ok(Type::Enum(Enum {
                type_identifier: type_identifier.clone(),
                shared_fields: {
                    let mut map = HashMap::new();

                    for (identifier, type_annotation) in shared_fields {
                        map.insert(
                            identifier.clone(),
                            check_type_annotation(
                                type_annotation,
                                discovered_types,
                                type_environment.clone(),
                            )?,
                        );
                    }

                    map
                },
                members: {
                    let mut map = HashMap::new();

                    for (identifier, fields) in members {
                        let mut fields_map = HashMap::new();

                        for (identifier, type_annotation) in fields {
                            fields_map.insert(
                                identifier.clone(),
                                check_type_annotation(
                                    type_annotation,
                                    discovered_types,
                                    type_environment.clone(),
                                )?,
                            );
                        }

                        map.insert(identifier.clone(), fields_map);
                    }

                    map.iter()
                        .map(|(k, v)| {
                            (
                                k.clone(),
                                Type::EnumMember(EnumMember {
                                    enum_name: type_identifier.clone(),
                                    discriminant_name: k.clone(),
                                    fields: v.clone(),
                                }),
                            )
                        })
                        .collect()
                },
            }))
        }
        Some(DiscoveredType::Union(type_identifier, literals)) => {
            let literal_types = literals
                .iter()
                .map(|literal| {
                    check_type_annotation(literal, discovered_types, type_environment.clone())
                })
                .collect::<Result<Vec<Type>, String>>()?;

            let literal_type = literal_types.iter().fold(Ok(Type::Void), |acc, t| {
                if type_equals(&acc.clone()?, &Type::Void) {
                    Ok(t.clone())
                } else if type_equals(&acc.clone()?, t) {
                    Err(format!(
                        "All literals in a union must have the same type. Expected {}, found {}",
                        acc.as_ref().unwrap(),
                        t
                    ))
                } else {
                    acc
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
                .map(|type_annotation| {
                    check_type_annotation(
                        type_annotation,
                        discovered_types,
                        type_environment.clone(),
                    )
                })
                .collect::<Result<Vec<Type>, String>>()?;

            Ok(Type::TypeAlias(TypeAlias {
                type_identifier: type_identifier.clone(),
                types,
            }))
        }
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

            return Ok(Type::Function(Function {
                identifier: Some(type_identifier.clone()),
                param,
                return_type: Box::new(check_type_annotation(
                    return_type_annotation,
                    discovered_types,
                    type_environment,
                )?),
            }));
        }
        None => Type::from_string(&type_identifier.to_string())
            .ok_or(format!("Unknown type {}", type_identifier)),
    }
}

pub fn check_type_annotation(
    type_annotation: &TypeAnnotation,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<Type, String> {
    if let Ok(type_) = type_environment
        .borrow()
        .get_type_from_annotation(type_annotation)
    {
        return Ok(type_);
    }

    match discovered_types
        .iter()
        .find(|discovered_type| match discovered_type {
            DiscoveredType::Struct(type_identifier, ..) => {
                type_identifier.name() == type_annotation.name()
            }
            DiscoveredType::Enum(type_identifier, ..) => {
                type_identifier.name() == type_annotation.name()
            }
            DiscoveredType::Union(type_identifier, ..) => {
                type_identifier.name() == type_annotation.name()
            }
            DiscoveredType::TypeAlias(type_identifier, ..) => {
                type_identifier.name() == type_annotation.name()
            }
            DiscoveredType::Function {
                type_identifier, ..
            } => type_identifier.name() == type_annotation.name(),
        }) {
        Some(DiscoveredType::Struct(type_identifier, fields)) => Ok(Type::Struct(Struct {
            type_identifier: type_identifier.clone(),
            fields: {
                let mut map = HashMap::new();

                for (identifier, type_annotation) in fields {
                    map.insert(
                        identifier.clone(),
                        check_type_annotation(
                            type_annotation,
                            discovered_types,
                            type_environment.clone(),
                        )?,
                    );
                }

                map
            },
        })),
        Some(DiscoveredType::Enum(type_identifier, shared_fields, members)) => {
            Ok(Type::Enum(Enum {
                type_identifier: type_identifier.clone(),
                shared_fields: {
                    let mut map = HashMap::new();

                    for (identifier, type_annotation) in shared_fields {
                        map.insert(
                            identifier.clone(),
                            check_type_annotation(
                                type_annotation,
                                discovered_types,
                                type_environment.clone(),
                            )?,
                        );
                    }

                    map
                },
                members: {
                    let mut map = HashMap::new();

                    for (identifier, fields) in members {
                        let mut fields_map = HashMap::new();

                        for (identifier, type_annotation) in fields {
                            fields_map.insert(
                                identifier.clone(),
                                check_type_annotation(
                                    type_annotation,
                                    discovered_types,
                                    type_environment.clone(),
                                )?,
                            );
                        }

                        map.insert(identifier.clone(), fields_map);
                    }

                    map.iter()
                        .map(|(k, v)| {
                            (
                                k.clone(),
                                Type::EnumMember(EnumMember {
                                    enum_name: type_identifier.clone(),
                                    discriminant_name: k.clone(),
                                    fields: v.clone(),
                                }),
                            )
                        })
                        .collect()
                },
            }))
        }
        Some(DiscoveredType::Union(type_identifier, literals)) => {
            let literal_types = literals
                .iter()
                .map(|literal| {
                    check_type_annotation(literal, discovered_types, type_environment.clone())
                })
                .collect::<Result<Vec<Type>, String>>()?;

            let literal_type = literal_types.iter().fold(Ok(Type::Void), |acc, t| {
                if type_equals(&acc.clone()?, &Type::Void) {
                    Ok(t.clone())
                } else if !type_equals(&acc.clone()?, t) {
                    Err(format!(
                        "All literals in a union must have the same type. Expected {}, found {}",
                        acc.as_ref().unwrap(),
                        t
                    ))
                } else {
                    acc
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
                .collect::<Result<Vec<Type>, String>>()?;

            Ok(Type::TypeAlias(TypeAlias {
                type_identifier: type_identifier.clone(),
                types,
            }))
        }
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
                identifier: Some(type_identifier.clone()),
                param,
                return_type: Box::new(check_type_annotation(
                    return_type_annotation,
                    discovered_types,
                    type_environment,
                )?),
            }))
        }
        None => Type::from_string(&type_annotation.to_string())
            .ok_or(format!("Unknown type {}", type_annotation)),
    }
}
