use std::collections::HashMap;

use crate::parser::{Statement, self};

use super::{
    Type,
    Struct,
    StructField,
    Union,
    UnionMember,
    UnionMemberField,
    Function,
    expressions,
    ast::{TypedStatement, self},
    type_checker::DiscoveredType,
    type_environment::TypeEnvironment
};

pub fn discover_user_defined_types(statement: &Statement) -> Result<Vec<DiscoveredType>, String> {
    match statement {
        Statement::Program {
            statements
        } => {
            let mut discovered_types = vec![];
            for statement in statements {
                discovered_types.append(&mut discover_user_defined_types(statement)?);
            }
            Ok(discovered_types)
        },
        Statement::StructDeclaration(parser::StructDeclaration {
            access_modifier: _,
            type_name,
            fields
        }) => Ok(vec![DiscoveredType::Struct(
            type_name.clone(),
            fields.iter().map(|field| (field.identifier.clone(), field.type_name.clone())).collect())]),
        Statement::UnionDeclaration(parser::UnionDeclaration {
            access_modifier: _,
            type_name,
            members: fields
        }) => Ok(vec![DiscoveredType::Union(
            type_name.clone(),
            fields.iter().map(|member| 
                (member.identifier.clone(), member.fields
                    .iter().map(|field|
                        (field.identifier.clone(), field.type_name.clone()))
                    .collect()))
            .collect())]),
        Statement::FunctionDeclaration(parser::FunctionDeclaration {
            access_modifier: _,
            identifier,
            parameters,
            return_type,
            body: _
        }) => Ok(vec![DiscoveredType::Function(
            identifier.clone(),
            parameters.iter().map(|parameter| (parameter.identifier.clone(), parameter.type_name.clone())).collect(),
            return_type.clone().unwrap_or(Type::Void.to_string()))]),
        Statement::Expression(_) => Ok(vec![])
    }
}

pub fn check_type<'a>(statement: &Statement, discovered_types: &Vec<DiscoveredType>, type_environment: &mut TypeEnvironment<'a>) -> Result<TypedStatement, String> {
    match statement {
        Statement::Program {
            statements
        } => {
            let mut typed_statements = vec![];

            for statement in statements {
                typed_statements.push(check_type(statement, discovered_types, type_environment)?);
            }

            let statements: Result<Vec<TypedStatement>, String> = statements.iter().map(|s| check_type(s, discovered_types, type_environment)).collect();

            Ok(TypedStatement::Program {
                statements: statements?
            })
        },
        Statement::StructDeclaration(parser::StructDeclaration {
            access_modifier: _,
            type_name,
            fields
        }) => {
            let fs: Result<Vec<ast::StructField>, String> = fields.iter()
                .map(|field| {
                    match check_type_name(&field.type_name, &discovered_types) {
                        Ok(t) => Ok(ast::StructField {
                            mutable: field.mutable,
                            identifier: field.identifier.clone(),
                            type_: t
                        }),
                        Err(e) => Err(e)
                    }
                })
                .collect();

            let type_ = Type::Struct(Struct {
                name: type_name.clone(),
                fields: fs.clone()?.iter().map(|f| {
                    let struct_field = Type::StructField(StructField {
                        struct_name: type_name.clone(),
                        field_name: f.identifier.clone(),
                        field_type: Box::new(f.type_.clone())
                    });

                    type_environment.add_type(f.identifier.clone(), struct_field.clone());

                    (f.identifier.clone(), struct_field)
                }).collect()
            });

            type_environment.add_type(type_name.clone(), type_.clone());

            Ok(TypedStatement::StructDeclaration {
                type_name: type_name.clone(), 
                fields: fs?,
                type_
            })
        },
        Statement::UnionDeclaration(
            parser::UnionDeclaration {
                access_modifier: _,
                type_name,
                members
            }
        ) => {
            let ms: Result<Vec<ast::UnionMember>, String> = members.iter()
                .map(|member| {
                    let fs: Result<Vec<ast::UnionMemberField>, String> = member.fields.iter()
                        .map(|field| {
                            match check_type_name(&field.type_name, &discovered_types) {
                                Ok(t) => Ok(ast::UnionMemberField {
                                    union_name: type_name.clone(),
                                    discriminant_name: member.identifier.clone(),
                                    field_position: field.field_position,
                                    identifier: field.identifier.clone(),
                                    type_: t
                                }),
                                Err(e) => Err(e)
                            }
                        })
                        .collect();

                    let type_ = Type::UnionMember(UnionMember {
                        union_name: type_name.clone(),
                        discriminant_name: member.identifier.clone(),
                        fields: fs.clone()?.iter().map(|f| {
                            let union_member_field = Type::UnionMemberField(UnionMemberField {
                                union_name: type_name.clone(),
                                discriminant_name: member.identifier.clone(),
                                field_position: f.field_position,
                                field_name: f.identifier.clone(),
                                field_type: Box::new(f.type_.clone())
                            });

                            type_environment.add_type(
                                f.identifier
                                    .clone()
                                    .unwrap_or(f.field_position.to_string()),
                                union_member_field.clone());

                            (f.identifier.clone(), union_member_field)
                        }).collect()
                    });

                    type_environment.add_type(member.identifier.clone(), type_.clone());

                    Ok(ast::UnionMember {
                        union_name: type_name.clone(),
                        discriminant_name: member.identifier.clone(),
                        fields: fs.clone()?,
                        type_
                    })
                })
                .collect();

            let type_ = Type::Union(Union {
                name: type_name.clone(),
                members: ms.clone()?
                    .iter()
                    .map(|m| (m.discriminant_name.clone(), m.type_.clone()))
                    .collect()
            });

            type_environment.add_type(type_name.clone(), type_.clone());

            Ok(TypedStatement::UnionDeclaration {
                type_name: type_name.clone(),
                members: ms?,
                type_
            })
        },
        Statement::FunctionDeclaration(parser::FunctionDeclaration {
            access_modifier: _,
            identifier,
            parameters,
            return_type,
            body
        }) => {
            let ps: Result<Vec<ast::Parameter>, String> = parameters.iter()
                .map(|parameter| {
                    match check_type_name(&parameter.type_name, &discovered_types) {
                        Ok(t) => Ok(ast::Parameter {
                            identifier: parameter.identifier.clone(),
                            type_name: parameter.type_name.clone(),
                            type_: t
                        }),
                        Err(e) => Err(e)
                    }
                })
                .collect();

            let type_ = Type::Function(Function {
                name: identifier.clone(),
                parameters: ps.clone()?.iter().map(|p| (p.identifier.clone(), p.type_.clone())).collect(),
                return_type: Box::new(check_type_name(&return_type.clone().unwrap_or(Type::Void.to_string()), &discovered_types)?)
            });

            type_environment.add_type(identifier.clone(), type_.clone());

            Ok(TypedStatement::FunctionDeclaration {
                identifier: identifier.clone(),
                parameters: ps?,
                return_type: return_type.clone().unwrap_or(Type::Void.to_string()),
                body: Box::new(check_type(&Statement::Expression(body.clone()), discovered_types, type_environment)?.as_expression().unwrap().clone()),
                type_
            })
        
        },
        Statement::Expression(e) => Ok(TypedStatement::Expression(expressions::check_type(e, type_environment)?)),
    }
}

fn check_type_name(type_name: &String, discovered_types: &Vec<DiscoveredType>) -> Result<Type, String> {
    match discovered_types.iter().find(|discovered_type| match discovered_type {
        DiscoveredType::Struct(name, ..) => name == type_name,
        DiscoveredType::Union(name, ..) => name == type_name,
        DiscoveredType::Function(name, ..) => name == type_name
    }) {
        Some(DiscoveredType::Struct(name, fields)) => Ok(Type::Struct(Struct {
            name: name.clone(),
            fields: {
                let mut map = HashMap::new();

                for (identifier, type_name) in fields {
                    map.insert(identifier.clone(), check_type_name(type_name, discovered_types)?);
                }

                map
            }
        })),
        Some(DiscoveredType::Union(name, members)) => Ok(Type::Union(Union {
            name: name.clone(),
            members: {
                let mut map = HashMap::new();

                for (identifier, fields) in members {
                    let mut fields_map = HashMap::new();

                    for (identifier, type_name) in fields {
                        fields_map.insert(identifier.clone(), check_type_name(type_name, discovered_types)?);
                    }

                    map.insert(identifier.clone(), fields_map);
                }

                map.iter().map(|(k, v)| (k.clone(), Type::UnionMember(UnionMember {
                    union_name: name.clone(),
                    discriminant_name: k.clone(),
                    fields: v.clone()
                }))).collect()
            }
        })),
        Some(DiscoveredType::Function(name, parameters, return_type)) => Ok(Type::Function(Function {
            name: name.clone(),
            parameters: {
                let mut map = HashMap::new();

                for (identifier, type_name) in parameters {
                    map.insert(identifier.clone(), check_type_name(type_name, discovered_types)?);
                }

                map
            },
            return_type: Box::new(check_type_name(return_type, discovered_types)?)
        
        })),
        None => Type::from_string(type_name).ok_or(format!("Unknown type {}", type_name))
    }
}