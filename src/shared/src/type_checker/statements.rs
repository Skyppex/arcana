use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{parser::{self, Impl, Statement}, types::{TypeAnnotation, TypeIdentifier}};

use super::{
    ast::{self, Typed, TypedStatement}, expressions, scope::ScopeType, type_checker::DiscoveredType, type_environment::TypeEnvironment, Function, Rcrc, Struct, StructField, Type, Union, UnionMember, UnionMemberField
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
            fields,
        }) => Ok(vec![DiscoveredType::Struct(
            type_identifier.clone(),
            fields
                .iter()
                .map(|field| (field.identifier.clone(), field.type_annotation.clone()))
                .collect(),
        )]),
        Statement::UnionDeclaration(parser::UnionDeclaration {
            access_modifier: _,
            type_identifier,
            members,
        }) => Ok(vec![DiscoveredType::Union(
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
                .map(|parameter| (parameter.identifier.clone(), parameter.type_annotation.clone()))
                .collect(),
            return_type.clone().unwrap_or(TypeAnnotation::Type(Type::Void.to_string())),
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
            fields,
        }) => {
            let struct_type_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
                type_environment.clone(),
            )));

            if let TypeIdentifier::GenericType(_, generics) = type_identifier {
                for generic in generics {
                    struct_type_environment.borrow_mut().add_type(Type::Generic(generic.clone()))?;
                }
            }

            let fs: Result<Vec<ast::StructField>, String> = fields
                .iter()
                .map(|field| {
                    match check_type_annotation(&field.type_annotation, &discovered_types, struct_type_environment.clone()) {
                        Ok(t) => Ok(ast::StructField {
                            mutable: field.mutable,
                            identifier: field.identifier.clone(),
                            type_: t,
                        }),
                        Err(e) => Err(e),
                    }
                })
                .collect();

            let fields: Result<HashMap<String, Type>, String> = fs
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
                fields: fields?,
            });

            type_environment.borrow_mut().add_type(type_.clone())?;

            Ok(TypedStatement::StructDeclaration {
                type_identifier: type_identifier.clone(),
                fields: fs?,
                type_,
            })
        }
        Statement::UnionDeclaration(parser::UnionDeclaration {
            access_modifier: _,
            type_identifier,
            members,
        }) => {
            let union_type_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
                type_environment.clone(),
            )));

            if let TypeIdentifier::GenericType(_, generics) = type_identifier {
                for generic in generics {
                    union_type_environment.borrow_mut().add_type(Type::Generic(generic.clone()))?;
                }
            }

            let ms: Result<Vec<ast::UnionMember>, String> = members
                .iter()
                .map(|member| {
                    let fs: Result<Vec<ast::UnionMemberField>, String> = member
                        .fields
                        .iter()
                        .map(|field| {
                            match check_type_annotation(
                                &field.type_annotation,
                                &discovered_types,
                                union_type_environment.clone(),
                            ) {
                                Ok(t) => Ok(ast::UnionMemberField {
                                    union_name: type_identifier.clone(),
                                    discriminant_name: member.identifier.clone(),
                                    identifier: field.identifier.clone(),
                                    type_: t,
                                }),
                                Err(e) => Err(e),
                            }
                        })
                        .collect();

                    let fields: Result<HashMap<String, Type>, String> = fs
                        .clone()?
                        .iter()
                        .map(|f| {
                            let field_name = f.identifier.clone();

                            let union_member_field = Type::UnionMemberField(UnionMemberField {
                                union_name: type_identifier.clone(),
                                discriminant_name: member.identifier.clone(),
                                field_name: field_name.clone(),
                                field_type: Box::new(f.type_.clone()),
                            });

                            match type_environment.borrow_mut().add_type(union_member_field.clone()) {
                                Ok(_) => Ok((field_name, union_member_field)),
                                Err(e) => Err(e),
                            }
                        })
                        .collect();

                    let union_member = Type::UnionMember(UnionMember {
                        union_name: type_identifier.clone(),
                        discriminant_name: member.identifier.clone(),
                        fields: fields?,
                    });

                    type_environment.borrow_mut().add_type(union_member.clone())?;

                    Ok(ast::UnionMember {
                        union_name: type_identifier.clone(),
                        discriminant_name: member.identifier.clone(),
                        fields: fs.clone()?,
                        type_: union_member,
                    })
                })
                .collect();

            let union = Type::Union(Union {
                type_identifier: type_identifier.clone(),
                members: ms
                    .clone()?
                    .iter()
                    .map(|m| (m.discriminant_name.clone(), m.type_.clone()))
                    .collect(),
            });

            type_environment.borrow_mut().add_type(union.clone())?;

            Ok(TypedStatement::UnionDeclaration {
                type_identifier: type_identifier.clone(),
                members: ms?,
                type_: union,
            })
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

        //     // let flags = Type::Flags(Union {
        //     //     name: type_name.clone(),
        //     //     members: ms.clone()?
        //     //         .iter()
        //     //         .map(|m| (m.identifier.clone(), Type::FlagsMember(UnionMember {
        //     //             union_name: type_name.clone(),
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
            let impl_type = check_type_annotation(type_annotation, discovered_types, type_environment.clone())?;

            let mut typed_functions = vec![];

            for function in functions {
                let typed_function = check_type(function, discovered_types, type_environment.clone())?;

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
                    impl_type.clone(),
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
                &return_type.clone().unwrap_or(TypeAnnotation::Type(Type::Void.to_string())),
                &discovered_types,
                type_environment.clone(),
            )?;

            let block_environment =
                Rc::new(RefCell::new(TypeEnvironment::new_scope(
                    type_environment.clone(),
                    ScopeType::Return)));

            let parameters: Result<Vec<ast::Parameter>, String> = parameters
                .iter()
                .map(|parameter| {
                    match check_type_annotation(
                        &parameter.type_annotation,
                        &discovered_types,
                        block_environment.clone(),
                    ) {
                        Ok(t) => {
                            block_environment.borrow_mut().add_variable(parameter.identifier.clone(), t.clone());

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
                .map(|s| {
                    check_type(s, discovered_types, block_environment.clone())
                })
                .collect();

            let body_typed_statement = body_typed_statement?;

            let return_scope = block_environment.borrow().get_scope(&ScopeType::Return);

            let body_type = return_scope.map(|s| s.fold())
                .unwrap_or(Ok(Type::Void))?;

            if return_type != body_type {
                return Err(format!("Function body's return type {} does not match function return type {}", body_type, return_type));
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
                let typed_expression = expressions::check_type(
                    e,
                    discovered_types,
                    type_environment.clone(),
                )?;

                let break_type = typed_expression.get_type();
                type_environment.borrow_mut().activate_scope(ScopeType::Break, break_type)?;
                Ok(TypedStatement::Break(Some(typed_expression)))
            },
            None =>
            {
                type_environment.borrow_mut().activate_scope(ScopeType::Break, Type::Void)?;
                Ok(TypedStatement::Break(None))
            },
        },
        Statement::Continue => Ok(TypedStatement::Continue),
        Statement::Return(e) => match e {
            Some(e) => {
                let typed_expression = expressions::check_type(
                    e,
                    discovered_types,
                    type_environment.clone(),
                )?;

                let return_type = typed_expression.get_type();
                type_environment.borrow_mut().activate_scope(ScopeType::Return, return_type)?;
                Ok(TypedStatement::Return(Some(typed_expression)))
            },
            None => {
                type_environment.borrow_mut().activate_scope(ScopeType::Return, Type::Void)?;
                Ok(TypedStatement::Return(None))
            },
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
    if let Some(type_) = type_environment.borrow().get_type_from_identifier(type_identifier) {
        return Ok(type_.clone());
    }

    match discovered_types
        .iter()
        .find(|discovered_type| match discovered_type {
            DiscoveredType::Struct(name, ..) => name == type_identifier,
            DiscoveredType::Union(name, ..) => name == type_identifier,
            DiscoveredType::Function(name, ..) => name == type_identifier,
        }) {
        Some(DiscoveredType::Struct(type_identifier, fields)) => Ok(Type::Struct(Struct {
            type_identifier: type_identifier.clone(),
            fields: {
                let mut map = HashMap::new();

                for (identifier, type_annotation) in fields {
                    map.insert(
                        identifier.clone(),
                        check_type_annotation(type_annotation, discovered_types, type_environment.clone())?,
                    );
                }

                map
            },
        })),
        Some(DiscoveredType::Union(type_identifier, members)) => Ok(Type::Union(Union {
            type_identifier: type_identifier.clone(),
            members: {
                let mut map = HashMap::new();

                for (identifier, fields) in members {
                    let mut fields_map = HashMap::new();

                    for (identifier, type_annotation) in fields {
                        fields_map.insert(
                            identifier.clone(),
                            check_type_annotation(type_annotation, discovered_types, type_environment.clone())?,
                        );
                    }

                    map.insert(identifier.clone(), fields_map);
                }

                map.iter()
                    .map(|(k, v)| {
                        (
                            k.clone(),
                            Type::UnionMember(UnionMember {
                                union_name: type_identifier.clone(),
                                discriminant_name: k.clone(),
                                fields: v.clone(),
                            }),
                        )
                    })
                    .collect()
            },
        })),
        Some(DiscoveredType::Function(type_identifier, parameters, return_type)) => {
            Ok(Type::Function(Function {
                identifier: type_identifier.clone(),
                parameters: {
                    let mut map = HashMap::new();

                    for (identifier, type_annotation) in parameters {
                        map.insert(
                            identifier.clone(),
                            check_type_annotation(type_annotation, discovered_types, type_environment.clone())?,
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
        None => Type::from_string(&type_identifier.to_string()).ok_or(format!("Unknown type {}", type_identifier)),
    }
}

pub fn check_type_annotation(
    type_annotation: &TypeAnnotation,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<Type, String> {
    if let Ok(type_) = type_environment.borrow().get_type_from_annotation(type_annotation, type_environment.clone()) {
         return Ok(type_);
    }

    match discovered_types
        .iter()
        .find(|discovered_type| match discovered_type {
            DiscoveredType::Struct(type_identifier, ..) => type_identifier.name() == type_annotation.name(),
            DiscoveredType::Union(type_identifier, ..) => type_identifier.name() == type_annotation.name(),
            DiscoveredType::Function(type_identifier, ..) => type_identifier.name() == type_annotation.name(),
        }) {
        Some(DiscoveredType::Struct(type_identifier, fields)) => Ok(Type::Struct(Struct {
            type_identifier: type_identifier.clone(),
            fields: {
                let mut map = HashMap::new();

                for (identifier, type_annotation) in fields {
                    map.insert(
                        identifier.clone(),
                        check_type_annotation(type_annotation, discovered_types, type_environment.clone())?,
                    );
                }

                map
            },
        })),
        Some(DiscoveredType::Union(type_identifier, members)) => Ok(Type::Union(Union {
            type_identifier: type_identifier.clone(),
            members: {
                let mut map = HashMap::new();

                for (identifier, fields) in members {
                    let mut fields_map = HashMap::new();

                    for (identifier, type_annotation) in fields {
                        fields_map.insert(
                            identifier.clone(),
                            check_type_annotation(type_annotation, discovered_types, type_environment.clone())?,
                        );
                    }

                    map.insert(identifier.clone(), fields_map);
                }

                map.iter()
                    .map(|(k, v)| {
                        (
                            k.clone(),
                            Type::UnionMember(UnionMember {
                                union_name: type_identifier.clone(),
                                discriminant_name: k.clone(),
                                fields: v.clone(),
                            }),
                        )
                    })
                    .collect()
            },
        })),
        Some(DiscoveredType::Function(type_identifier, parameters, return_type)) => {
            Ok(Type::Function(Function {
                identifier: type_identifier.clone(),
                parameters: {
                    let mut map = HashMap::new();

                    for (identifier, type_annotation) in parameters {
                        map.insert(
                            identifier.clone(),
                            check_type_annotation(type_annotation, discovered_types, type_environment.clone())?,
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
        None => Type::from_string(&type_annotation.to_string()).ok_or(format!("Unknown type {}", type_annotation)),
    }
}