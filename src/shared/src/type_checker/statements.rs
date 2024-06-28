use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    parser::{self, Statement, UnionDeclaration},
    types::{TypeAnnotation, TypeIdentifier},
};

use super::{
    ast::{self, Typed, TypedParameter, TypedStatement},
    expressions,
    scope::ScopeType,
    type_checker::DiscoveredType,
    type_environment::TypeEnvironment,
    Enum, EnumMember, Function, Parameter, Rcrc, Struct, Type, Union,
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
        Statement::FunctionDeclaration(parser::FunctionDeclaration {
            access_modifier: _,
            identifier,
            param,
            return_type_annotation,
            body,
        }) => {
            let return_type = check_type_annotation(
                &return_type_annotation
                    .clone()
                    .unwrap_or(TypeAnnotation::Type(Type::Void.to_string())),
                &discovered_types,
                type_environment.clone(),
            )?;

            let block_environment = Rc::new(RefCell::new(TypeEnvironment::new_scope(
                type_environment.clone(),
                ScopeType::Return,
            )));

            if let Some(param) = param {
                match check_type_annotation(
                    &param.type_annotation,
                    &discovered_types,
                    block_environment.clone(),
                ) {
                    Ok(t) => {
                        block_environment
                            .borrow_mut()
                            .add_variable(param.name.clone(), t.clone());

                        Ok(())
                    }
                    Err(e) => Err(e),
                }?;
            }

            let param: Option<Parameter> = match param {
                Some(p) => {
                    let param_name = p.name.clone();
                    let param_type_annotation = p.type_annotation.clone();

                    if param_name.is_empty() {
                        return Err("Parameter must have a name".to_string());
                    }

                    Some(Parameter {
                        name: param_name,
                        type_: Box::new(check_type_annotation(
                            &param_type_annotation,
                            &discovered_types,
                            block_environment.clone(),
                        )?),
                    })
                }
                None => None,
            };

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
                param: param.clone(),
                return_type: Box::new(return_type.clone()),
            });

            type_environment.borrow_mut().add_type(type_.clone())?;

            Ok(TypedStatement::FunctionDeclaration {
                identifier: identifier.clone(),
                param: param.map(|p| TypedParameter {
                    name: p.name,
                    type_annotation: p.type_.type_annotation(),
                    type_: p.type_,
                }),
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
        Some(DiscoveredType::Function {
            type_identifier,
            param,
            return_type_annotation,
        }) => {
            let param = match param {
                Some(param) => Some(Parameter {
                    name: param.name.clone(),
                    type_: Box::new(check_type_annotation(
                        &param.type_annotation,
                        discovered_types,
                        type_environment.clone(),
                    )?),
                }),
                None => None,
            };

            return Ok(Type::Function(Function {
                identifier: type_identifier.clone(),
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
        Some(DiscoveredType::Function {
            type_identifier,
            param,
            return_type_annotation,
        }) => {
            let param = match param {
                Some(param) => Some(Parameter {
                    name: param.name.clone(),
                    type_: Box::new(check_type_annotation(
                        &param.type_annotation,
                        discovered_types,
                        type_environment.clone(),
                    )?),
                }),
                None => None,
            };

            Ok(Type::Function(Function {
                identifier: type_identifier.clone(),
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
