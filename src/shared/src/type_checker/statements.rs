use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    parser::{self, Impl, Statement, UnionDeclaration},
    types::{GenericType, TypeAnnotation, TypeIdentifier},
};

use super::{
    ast::{self, Typed, TypedStatement},
    expressions,
    scope::ScopeType,
    type_checker::DiscoveredType,
    type_environment::TypeEnvironment,
    Enum, EnumMember, EnumMemberField, Function, Rcrc, Struct, StructField, Trait, Type, Union,
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
            members,
        }) => Ok(vec![DiscoveredType::Enum(
            type_identifier.clone(),
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
        Statement::TraitDeclaration(parser::TraitDeclaration {
            access_modifier: _,
            type_identifier,
            associated_types,
            functions,
        }) => {
            let mut discovered_types = vec![];

            let trait_type = DiscoveredType::Trait(
                type_identifier.clone(),
                associated_types.iter().cloned().collect(),
                functions.iter().map(|f| f.identifier.clone()).collect(),
            );

            discovered_types.push(trait_type);
            Ok(discovered_types)
        }
        // Statement::FlagsDeclaration(parser::FlagsDeclaration {
        //     access_modifier: _,
        //     type_name,
        //     members
        // }) => Ok(vec![]),
        Statement::Impl(Impl {
            functions: methods, ..
        }) => {
            let mut discovered_types = vec![];
            for method in methods {
                discovered_types.append(&mut discover_user_defined_types(method)?);
            }
            Ok(discovered_types)
        }
        Statement::FunctionDeclaration(parser::FunctionDeclaration {
            access_modifier: _,
            identifier,
            parameters,
            return_type,
            body: _,
        }) => Ok(vec![DiscoveredType::Function(
            identifier.clone(),
            parameters
                .iter()
                .map(|parameter| {
                    (
                        parameter.identifier.clone(),
                        parameter.type_annotation.clone(),
                    )
                })
                .collect(),
            return_type
                .clone()
                .unwrap_or(TypeAnnotation::Type(Type::Void.to_string())),
        )]),
        Statement::Semi(_) => Ok(vec![]),
        Statement::Break(_) => Ok(vec![]),
        Statement::Continue => Ok(vec![]),
        Statement::Return(_) => Ok(vec![]),
        Statement::Expression(_) => Ok(vec![]),
        Statement::Print(_) => Ok(vec![]),
    }
}

pub fn check_type<'a>(
    statement: &Statement,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<TypedStatement, String> {
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
                .map(|f: &ast::StructField| {
                    let struct_field = Type::StructField(StructField {
                        struct_name: type_identifier.clone(),
                        field_name: f.identifier.clone(),
                        field_type: Box::new(f.type_.clone()),
                    });

                    match type_environment.borrow_mut().add_type(struct_field.clone()) {
                        Ok(_) => Ok((f.identifier.clone(), struct_field)),
                        Err(e) => Err(e),
                    }
                })
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
                        .map(|f| {
                            let field_name = f.identifier.clone();

                            let enum_member_field = Type::EnumMemberField(EnumMemberField {
                                enum_name: type_identifier.clone(),
                                discriminant_name: member.identifier.clone(),
                                field_name: field_name.clone(),
                                field_type: Box::new(f.type_.clone()),
                            });

                            match type_environment
                                .borrow_mut()
                                .add_type(enum_member_field.clone())
                            {
                                Ok(_) => Ok((field_name, enum_member_field)),
                                Err(e) => Err(e),
                            }
                        })
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
                members: members
                    .clone()?
                    .iter()
                    .map(|m| (m.discriminant_name.clone(), m.type_.clone()))
                    .collect(),
            });

            type_environment.borrow_mut().add_type(enum_type.clone())?;

            Ok(TypedStatement::EnumDeclaration {
                type_identifier: type_identifier.clone(),
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

                if acc.clone()? == Type::Void {
                    Ok(*type_.clone())
                } else if acc.clone()? != *type_.clone() {
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
        Statement::TraitDeclaration(parser::TraitDeclaration {
            access_modifier: _,
            type_identifier,
            associated_types,
            functions: _,
        }) => {
            let trait_type_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
                type_environment.clone(),
            )));

            for associated_type in associated_types {
                trait_type_environment
                    .borrow_mut()
                    .add_type(Type::Generic(GenericType {
                        type_name: associated_type.name().to_string(),
                    }))?;
            }

            let associated_types = associated_types
                .iter()
                .map(|ti| {
                    let check_type_identifier =
                        check_type_identifier(ti, discovered_types, type_environment.clone());
                    (ti.name().to_string(), check_type_identifier)
                })
                .collect::<HashMap<String, Result<Type, String>>>();

            let values: Result<Vec<Type>, String> =
                associated_types.clone().into_values().map(|r| r).collect();

            let values = values?;

            let associated_types = associated_types
                .into_iter()
                .zip(values)
                .map(|((k, _), v)| (k, v.clone()))
                .collect::<HashMap<String, Type>>();

            let type_ = Type::Trait(Trait {
                type_identifier: type_identifier.clone(),
                associated_types,
                functions: Vec::new(),
            });

            type_environment.borrow_mut().add_type(type_.clone())?;

            Ok(TypedStatement::None)
        }
        // Statement::FlagsDeclaration(parser::FlagsDeclaration {
        //     access_modifier: _,
        //     type_name,
        //     members
        // }) => {
        //     todo!("Flags declaration")
        //     // let ms: Result<Vec<ast::FlagsMember>, String> = members.iter()
        //     //     .map(|member| {
        //     //         Ok(ast::FlagsMember {
        //     //             identifier: member.identifier.clone(),
        //     //             value: ast::FlagsValue::Default
        //     //         })
        //     //     })
        //     //     .collect();

        //     // let flags = Type::Flags(Enum {
        //     //     name: type_name.clone(),
        //     //     members: ms.clone()?
        //     //         .iter()
        //     //         .map(|m| (m.identifier.clone(), Type::FlagsMember(EnumMember {
        //     //             enum_name: type_name.clone(),
        //     //             discriminant_name: m.identifier.clone(),
        //     //             fields: HashMap::new()
        //     //         })))
        //     //         .collect()
        //     // });

        //     // type_environment.add_type(flags.clone())?;

        //     // Ok(TypedStatement::FlagsDeclaration {
        //     //     type_name: type_name.clone(),
        //     //     members: ms?,
        //     //     type_: flags
        //     // })
        // },
        Statement::Impl(parser::Impl {
            type_annotation,
            functions,
        }) => {
            let mut typed_functions = vec![];

            for function in functions {
                let typed_function =
                    check_type(function, discovered_types, type_environment.clone())?;

                let TypedStatement::FunctionDeclaration {
                    identifier,
                    parameters,
                    return_type,
                    body: _,
                    type_: _,
                } = typed_function.clone()
                else {
                    return Err("Impl method must be a function".to_string());
                };

                typed_functions.push(typed_function);

                let function_type = Type::Function(Function {
                    identifier: identifier.clone(),
                    parameters: parameters
                        .iter()
                        .map(|p| (p.identifier.clone(), p.type_.clone()))
                        .collect(),
                    return_type: Box::new(return_type.clone()),
                });

                type_environment.borrow_mut().add_impl_function(
                    type_annotation.clone(),
                    identifier.clone(),
                    function_type.clone(),
                )?;
            }

            Ok(TypedStatement::Impl {
                type_annotation: type_annotation.clone(),
                functions: typed_functions,
            })
        }
        Statement::FunctionDeclaration(parser::FunctionDeclaration {
            access_modifier: _,
            identifier,
            parameters,
            return_type,
            body,
        }) => {
            let return_type = check_type_annotation(
                &return_type
                    .clone()
                    .unwrap_or(TypeAnnotation::Type(Type::Void.to_string())),
                &discovered_types,
                type_environment.clone(),
            )?;

            let block_environment = Rc::new(RefCell::new(TypeEnvironment::new_scope(
                type_environment.clone(),
                ScopeType::Return,
            )));

            let parameters: Result<Vec<ast::Parameter>, String> = parameters
                .iter()
                .map(|parameter| {
                    match check_type_annotation(
                        &parameter.type_annotation,
                        &discovered_types,
                        block_environment.clone(),
                    ) {
                        Ok(t) => {
                            block_environment
                                .borrow_mut()
                                .add_variable(parameter.identifier.clone(), t.clone());

                            Ok(ast::Parameter {
                                identifier: parameter.identifier.clone(),
                                type_annotation: parameter.type_annotation.clone(),
                                type_: t,
                            })
                        }
                        Err(e) => Err(e),
                    }
                })
                .collect();

            let body_typed_statement: Result<Vec<TypedStatement>, String> = body
                .iter()
                .map(|s| check_type(s, discovered_types, block_environment.clone()))
                .collect();

            let body_typed_statement = body_typed_statement?;

            let return_scope = block_environment.borrow().get_scope(&ScopeType::Return);

            let body_type = return_scope.map(|s| s.fold()).unwrap_or_else(|| {
                Ok(body_typed_statement
                    .iter()
                    .last()
                    .map(|ts| ts.get_deep_type())
                    .unwrap_or(Type::Void))
            })?;

            if return_type != body_type {
                return Err(format!(
                    "Function body's return type {} does not match function return type {}",
                    body_type, return_type
                ));
            }

            let type_ = Type::Function(Function {
                identifier: identifier.clone(),
                parameters: parameters
                    .clone()?
                    .iter()
                    .map(|p| (p.identifier.clone(), p.type_.clone()))
                    .collect(),
                return_type: Box::new(return_type.clone()),
            });

            type_environment.borrow_mut().add_type(type_.clone())?;

            Ok(TypedStatement::FunctionDeclaration {
                identifier: identifier.clone(),
                parameters: parameters?,
                return_type,
                body: body_typed_statement,
                type_,
            })
        }
        Statement::Semi(s) => Ok(TypedStatement::Semi(Box::new(check_type(
            s,
            discovered_types,
            type_environment,
        )?))),
        Statement::Break(e) => match e {
            Some(e) => {
                let typed_expression =
                    expressions::check_type(e, discovered_types, type_environment.clone())?;

                let break_type = typed_expression.get_type();
                type_environment
                    .borrow_mut()
                    .activate_scope(ScopeType::Break, break_type)?;
                Ok(TypedStatement::Break(Some(typed_expression)))
            }
            None => {
                type_environment
                    .borrow_mut()
                    .activate_scope(ScopeType::Break, Type::Void)?;
                Ok(TypedStatement::Break(None))
            }
        },
        Statement::Continue => Ok(TypedStatement::Continue),
        Statement::Return(e) => match e {
            Some(e) => {
                let typed_expression =
                    expressions::check_type(e, discovered_types, type_environment.clone())?;

                let return_type = typed_expression.get_type();
                type_environment
                    .borrow_mut()
                    .activate_scope(ScopeType::Return, return_type)?;
                Ok(TypedStatement::Return(Some(typed_expression)))
            }
            None => {
                type_environment
                    .borrow_mut()
                    .activate_scope(ScopeType::Return, Type::Void)?;
                Ok(TypedStatement::Return(None))
            }
        },
        Statement::Expression(e) => Ok(TypedStatement::Expression(expressions::check_type(
            e,
            discovered_types,
            type_environment,
        )?)),
        Statement::Print(e) => Ok(TypedStatement::Print(expressions::check_type(
            e,
            discovered_types,
            type_environment,
        )?)),
    }
}

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
            DiscoveredType::Trait(name, ..) => name == type_identifier,
            DiscoveredType::Function(name, ..) => name == type_identifier,
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
        Some(DiscoveredType::Enum(type_identifier, members)) => Ok(Type::Enum(Enum {
            type_identifier: type_identifier.clone(),
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
        })),
        Some(DiscoveredType::Union(type_identifier, literals)) => {
            let literal_types = literals
                .iter()
                .map(|literal| {
                    check_type_annotation(literal, discovered_types, type_environment.clone())
                })
                .collect::<Result<Vec<Type>, String>>()?;

            let literal_type = literal_types.iter().fold(Ok(Type::Void), |acc, t| {
                if acc == Ok(Type::Void) {
                    Ok(t.clone())
                } else if acc != Ok(t.clone()) {
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
        Some(DiscoveredType::Trait(type_identifier, associated_types, functions)) => {
            let associated_types = associated_types
                .iter()
                .map(|ti| {
                    (
                        ti.name().to_string(),
                        check_type_identifier(ti, discovered_types, type_environment.clone()),
                    )
                })
                .collect::<HashMap<String, Result<Type, String>>>();

            let values: Result<Vec<Type>, String> =
                associated_types.clone().into_values().map(|r| r).collect();

            let values = values?;

            let associated_types = associated_types
                .into_iter()
                .zip(values)
                .map(|((k, _), v)| (k, v.clone()))
                .collect::<HashMap<String, Type>>();

            Ok(Type::Trait(Trait {
                type_identifier: type_identifier.clone(),
                associated_types,
                functions: functions.clone(),
            }))
        }
        Some(DiscoveredType::Function(type_identifier, parameters, return_type)) => {
            Ok(Type::Function(Function {
                identifier: type_identifier.clone(),
                parameters: {
                    let mut map = HashMap::new();

                    for (identifier, type_annotation) in parameters {
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
                return_type: Box::new(check_type_annotation(
                    return_type,
                    discovered_types,
                    type_environment,
                )?),
            }))
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
        .get_type_from_annotation(type_annotation, type_environment.clone())
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
            DiscoveredType::Trait(type_identifier, ..) => {
                type_identifier.name() == type_annotation.name()
            }
            DiscoveredType::Function(type_identifier, ..) => {
                type_identifier.name() == type_annotation.name()
            }
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
        Some(DiscoveredType::Enum(type_identifier, members)) => Ok(Type::Enum(Enum {
            type_identifier: type_identifier.clone(),
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
        })),
        Some(DiscoveredType::Union(type_identifier, literals)) => {
            let literal_types = literals
                .iter()
                .map(|literal| {
                    check_type_annotation(literal, discovered_types, type_environment.clone())
                })
                .collect::<Result<Vec<Type>, String>>()?;

            let literal_type = literal_types.iter().fold(Ok(Type::Void), |acc, t| {
                if acc == Ok(Type::Void) {
                    Ok(t.clone())
                } else if acc != Ok(t.clone()) {
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
        Some(DiscoveredType::Trait(type_identifier, ..)) => Ok(Type::Trait(Trait {
            type_identifier: type_identifier.clone(),
            associated_types: HashMap::new(),
            functions: Vec::new(),
        })),
        Some(DiscoveredType::Function(type_identifier, parameters, return_type)) => {
            Ok(Type::Function(Function {
                identifier: type_identifier.clone(),
                parameters: {
                    let mut map = HashMap::new();

                    for (identifier, type_annotation) in parameters {
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
                return_type: Box::new(check_type_annotation(
                    return_type,
                    discovered_types,
                    type_environment,
                )?),
            }))
        }
        None => Type::from_string(&type_annotation.to_string())
            .ok_or(format!("Unknown type {}", type_annotation)),
    }
}
