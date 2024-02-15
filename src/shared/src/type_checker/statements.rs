use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::parser::{self, Impl, Statement};

use super::{
    ast::{self, Typed, TypedStatement}, expressions, scope::Scope, type_checker::DiscoveredType, type_environment::TypeEnvironment, Function, Rcrc, Struct, StructField, Type, Union, UnionMember, UnionMemberField
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
            type_name,
            fields,
        }) => Ok(vec![DiscoveredType::Struct(
            type_name.clone(),
            fields
                .iter()
                .map(|field| (field.identifier.clone(), field.type_name.clone()))
                .collect(),
        )]),
        Statement::UnionDeclaration(parser::UnionDeclaration {
            access_modifier: _,
            type_name,
            members,
        }) => Ok(vec![DiscoveredType::Union(
            type_name.clone(),
            members
                .iter()
                .map(|member| {
                    (
                        member.identifier.clone(),
                        member
                            .fields
                            .iter()
                            .map(|field| (field.identifier.clone(), field.type_name.clone()))
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
                .map(|parameter| (parameter.identifier.clone(), parameter.type_name.clone()))
                .collect(),
            return_type.clone().unwrap_or(Type::Void.to_string()),
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
            type_name,
            fields,
        }) => {
            let fs: Result<Vec<ast::StructField>, String> = fields
                .iter()
                .map(|field| {
                    match check_type_name(&field.type_name, &discovered_types, type_environment.clone()) {
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
                        struct_name: type_name.clone(),
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
                name: type_name.clone(),
                fields: fields?,
            });

            type_environment.borrow_mut().add_type(type_.clone())?;

            Ok(TypedStatement::StructDeclaration {
                type_name: type_name.clone(),
                fields: fs?,
                type_,
            })
        }
        Statement::UnionDeclaration(parser::UnionDeclaration {
            access_modifier: _,
            type_name,
            members,
        }) => {
            let ms: Result<Vec<ast::UnionMember>, String> = members
                .iter()
                .map(|member| {
                    let fs: Result<Vec<ast::UnionMemberField>, String> = member
                        .fields
                        .iter()
                        .map(|field| {
                            match check_type_name(
                                &field.type_name,
                                &discovered_types,
                                type_environment.clone(),
                            ) {
                                Ok(t) => Ok(ast::UnionMemberField {
                                    union_name: type_name.clone(),
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
                                union_name: type_name.clone(),
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
                        union_name: type_name.clone(),
                        discriminant_name: member.identifier.clone(),
                        fields: fields?,
                    });

                    type_environment.borrow_mut().add_type(union_member.clone())?;

                    Ok(ast::UnionMember {
                        union_name: type_name.clone(),
                        discriminant_name: member.identifier.clone(),
                        fields: fs.clone()?,
                        type_: union_member,
                    })
                })
                .collect();

            let union = Type::Union(Union {
                name: type_name.clone(),
                members: ms
                    .clone()?
                    .iter()
                    .map(|m| (m.discriminant_name.clone(), m.type_.clone()))
                    .collect(),
            });

            type_environment.borrow_mut().add_type(union.clone())?;

            Ok(TypedStatement::UnionDeclaration {
                type_name: type_name.clone(),
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
            type_name,
            functions,
        }) => {
            let impl_type = check_type_name(type_name, discovered_types, type_environment.clone())?;

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
                    name: identifier.clone(),
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
                type_name: type_name.clone(),
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
            let return_type = check_type_name(
                &return_type.clone().unwrap_or(Type::Void.to_string()),
                &discovered_types,
                type_environment.clone(),
            )?;

            let block_environment =
                Rc::new(RefCell::new(TypeEnvironment::new_parent(type_environment.clone())));

            let ps: Result<Vec<ast::Parameter>, String> = parameters
                .iter()
                .map(|parameter| {
                    match check_type_name(
                        &parameter.type_name,
                        &discovered_types,
                        block_environment.clone(),
                    ) {
                        Ok(t) => {
                            block_environment.borrow_mut().add_variable(parameter.identifier.clone(), t.clone());

                            Ok(ast::Parameter {
                                identifier: parameter.identifier.clone(),
                                type_name: parameter.type_name.clone(),
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
                    let typed_statement = &check_type(s, discovered_types, block_environment.clone());

                    typed_statement.clone()
                })
                .collect();

            let body_typed_statement = body_typed_statement?;

            for s in body_typed_statement.clone() {
                let t = s.get_type();

                if t == Type::Void {
                    continue;
                }

                if t != return_type {
                    return Err(format!(
                        "Function {} has return type {} but body has type {}",
                        identifier,
                        return_type,
                        s.get_type()
                    ));
                }
            }

            let type_ = Type::Function(Function {
                name: identifier.clone(),
                parameters: ps
                    .clone()?
                    .iter()
                    .map(|p| (p.identifier.clone(), p.type_.clone()))
                    .collect(),
                return_type: Box::new(return_type.clone()),
            });

            type_environment.borrow_mut().add_type(type_.clone())?;

            Ok(TypedStatement::FunctionDeclaration {
                identifier: identifier.clone(),
                parameters: ps?,
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
                type_environment.borrow_mut().activate_scope(Scope::Break(Some(break_type)))?;
                Ok(TypedStatement::Break(Some(typed_expression)))
            },
            None =>
            {
                type_environment.borrow_mut().activate_scope(Scope::Break(None))?;
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
                type_environment.borrow_mut().activate_scope(Scope::Return(Some(return_type)))?;
                Ok(TypedStatement::Return(Some(typed_expression)))
            },
            None => {
                type_environment.borrow_mut().activate_scope(Scope::Return(None))?;
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

fn check_type_name<'a>(
    type_name: &str,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<Type, String> {
    if let Some(type_) = type_environment.borrow().get_type(type_name) {
        return Ok(type_.clone());
    }

    match discovered_types
        .iter()
        .find(|discovered_type| match discovered_type {
            DiscoveredType::Struct(name, ..) => name == type_name,
            DiscoveredType::Union(name, ..) => name == type_name,
            DiscoveredType::Function(name, ..) => name == type_name,
        }) {
        Some(DiscoveredType::Struct(name, fields)) => Ok(Type::Struct(Struct {
            name: name.clone(),
            fields: {
                let mut map = HashMap::new();

                for (identifier, type_name) in fields {
                    map.insert(
                        identifier.clone(),
                        check_type_name(type_name, discovered_types, type_environment.clone())?,
                    );
                }

                map
            },
        })),
        Some(DiscoveredType::Union(name, members)) => Ok(Type::Union(Union {
            name: name.clone(),
            members: {
                let mut map = HashMap::new();

                for (identifier, fields) in members {
                    let mut fields_map = HashMap::new();

                    for (identifier, type_name) in fields {
                        fields_map.insert(
                            identifier.clone(),
                            check_type_name(type_name, discovered_types, type_environment.clone())?,
                        );
                    }

                    map.insert(identifier.clone(), fields_map);
                }

                map.iter()
                    .map(|(k, v)| {
                        (
                            k.clone(),
                            Type::UnionMember(UnionMember {
                                union_name: name.clone(),
                                discriminant_name: k.clone(),
                                fields: v.clone(),
                            }),
                        )
                    })
                    .collect()
            },
        })),
        Some(DiscoveredType::Function(name, parameters, return_type)) => {
            Ok(Type::Function(Function {
                name: name.clone(),
                parameters: {
                    let mut map = HashMap::new();

                    for (identifier, type_name) in parameters {
                        map.insert(
                            identifier.clone(),
                            check_type_name(type_name, discovered_types, type_environment.clone())?,
                        );
                    }

                    map
                },
                return_type: Box::new(check_type_name(
                    return_type,
                    discovered_types,
                    type_environment,
                )?),
            }))
        }
        None => Type::from_string(type_name).ok_or(format!("Unknown type {}", type_name)),
    }
}
