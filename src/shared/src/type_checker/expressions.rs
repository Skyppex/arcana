use crate::{parser::{self, Expression, VariableDeclaration, If, Assignment, Call, Unary, Binary, Ternary}, type_checker::ast::Literal};

use super::{ast::{TypedExpression, Typed, ConditionBlock, Member, FieldInitializer, TypedStatement}, TypeEnvironment, Type, statements, DiscoveredType};

pub fn check_type<'a>(
    expression: &Expression,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: &mut TypeEnvironment<'a>
) -> Result<TypedExpression, String> {
    match expression {
        Expression::None => Ok(TypedExpression::None),
        Expression::VariableDeclaration(VariableDeclaration {
            mutable,
            type_name,
            identifier,
            initializer
        }) => {
            let type_ = type_environment.get_type(type_name)
                .ok_or_else(|| format!("Unexpected type: {}", type_name))?.clone();

            let initializer = if let Some(initializer) = initializer {
                let initializer = check_type(initializer, discovered_types, type_environment)?;
                Some(Box::new(initializer))
            } else {
                None
            };

            Ok(TypedExpression::VariableDeclaration {
                mutable: *mutable,
                identifier: identifier.clone(),
                initializer,
                type_
            })
        },
        Expression::If(If {
            r#if,
            else_ifs,
            r#else
        }) => {
            if let Type::Bool = check_type(&r#if.condition, discovered_types, type_environment)?.get_type() {
                return Err(format!("If condition must be of type bool"));
            }

            let if_block_type = check_type(&r#if.block, discovered_types, type_environment)?.get_type();

            if let Some(else_ifs) = else_ifs {
                for else_if in else_ifs {
                    if let Type::Bool = check_type(&else_if.condition, discovered_types, type_environment)?.get_type() {
                        return Err(format!("Else if condition must be of type bool"));
                    }

                    let else_if_block_type = check_type(&else_if.block, discovered_types, type_environment)?.get_type();

                    if if_block_type != else_if_block_type {
                        return Err(format!("If block type {:?} does not match else if block type {:?}", if_block_type, else_if_block_type));
                    }
                }
            }

            let else_type = if let Some(r#else) = r#else {
                Some(check_type(r#else, discovered_types, type_environment)?.get_type())
            } else {
                None
            };

            if let Some(else_type) = else_type {
                if if_block_type != else_type {
                    return Err(format!("If block type {:?} does not match else block type {:?}", if_block_type, else_type));
                }
            }

            Ok(TypedExpression::If {
                r#if: ConditionBlock {
                    condition: Box::new(check_type(&r#if.condition, discovered_types, type_environment)?),
                    block: Box::new(check_type(&r#if.block, discovered_types, type_environment)?),
                },
                else_ifs: if let Some(else_ifs) = else_ifs {
                    let mut elifs: Vec<ConditionBlock> = vec![];
                    
                    for elif in else_ifs {
                        elifs.push(ConditionBlock {
                            condition: Box::new(check_type(&elif.condition, discovered_types, type_environment)?),
                            block: Box::new(check_type(&elif.block, discovered_types, type_environment)?),
                        })
                    }

                    Some(elifs)
                } else {
                    None
                },
                r#else: if let Some(r#else) = r#else {
                    Some(Box::new(check_type(r#else, discovered_types, type_environment)?))
                } else {
                    None
                },
                type_: if_block_type.clone()
            })
        },
        Expression::Assignment(Assignment {
            member,
            initializer
        }) => {
            let member = check_type(member, discovered_types, type_environment)?;
            let initializer = check_type(initializer, discovered_types, type_environment)?;

            Ok(TypedExpression::Assignment {
                member: Box::new(member),
                initializer: Box::new(initializer.clone()),
                type_: initializer.get_type()
            })
        },
        Expression::Member(member) => {
            match member {
                crate::parser::Member::Identifier { symbol } => {
                    let type_ = type_environment.get_variable(symbol)
                        .ok_or_else(|| format!("Unexpected variable: {}", symbol))?.clone();

                    Ok(TypedExpression::Member(Member::Identifier {
                        symbol: symbol.clone(),
                        type_
                    }))
                },
                crate::parser::Member::MemberAccess {
                    object,
                    member,
                    symbol
                } => {
                    todo!("Member access")
                },
            }
        },
        Expression::Literal(l) => {
            let literal = match l {
                parser::Literal::I8(v) => Literal::I8(*v),
                parser::Literal::I16(v) => Literal::I16(*v),
                parser::Literal::I32(v) => Literal::I32(*v),
                parser::Literal::I64(v) => Literal::I64(*v),
                parser::Literal::I128(v) => Literal::I128(*v),
                parser::Literal::U8(v) => Literal::U8(*v),
                parser::Literal::U16(v) => Literal::U16(*v),
                parser::Literal::U32(v) => Literal::U32(*v),
                parser::Literal::U64(v) => Literal::U64(*v),
                parser::Literal::U128(v) => Literal::U128(*v),
                parser::Literal::F32(v) => Literal::F32(*v),
                parser::Literal::F64(v) => Literal::F64(*v),
                parser::Literal::String(v) => Literal::String(v.clone()),
                parser::Literal::Char(v) => Literal::Char(*v),
                parser::Literal::Bool(v) => Literal::Bool(*v),
                parser::Literal::Struct {
                    type_name,
                    field_initializers
                } => {
                    let field_initializers: Result<Option<Vec<FieldInitializer>>, String> = field_initializers.as_ref().map(|field_initializers| {
                        let mut field_initializers_: Vec<FieldInitializer> = vec![];
                        for field_initializer in field_initializers {
                            let field_initializer = FieldInitializer {
                                identifier: field_initializer.identifier.clone(),
                                initializer: check_type(&field_initializer.initializer, discovered_types, type_environment)?
                            };
                            field_initializers_.push(field_initializer);
                        }
                        Ok(field_initializers_)
                    }).transpose();

                    Literal::Struct {
                        type_name: type_name.clone(),
                        field_initializers: field_initializers?,
                        type_: type_environment.get_type(type_name)
                            .ok_or_else(|| format!("Unexpected type: {}", type_name))?.clone()
                    }
                },
                parser::Literal::Union {
                    type_name,
                    member,
                    field_initializers
                } => {
                    let field_initializers: Result<Option<Vec<FieldInitializer>>, String> = field_initializers.as_ref().map(|field_initializers| {
                        let mut field_initializers_: Vec<FieldInitializer> = vec![];
                        for field_initializer in field_initializers {
                            let field_initializer = FieldInitializer {
                                identifier: field_initializer.identifier.clone(),
                                initializer: check_type(&field_initializer.initializer, discovered_types, type_environment)?
                            };
                            field_initializers_.push(field_initializer);
                        }
                        Ok(field_initializers_)
                    }).transpose();

                    Literal::Union {
                        type_name: type_name.clone(),
                        member: member.clone(),
                        field_initializers: field_initializers?,
                        type_: type_environment.get_type(type_name)
                            .ok_or_else(|| format!("Unexpected type: {}", type_name))?.clone()
                    }
                
                },
            };

            Ok(TypedExpression::Literal(literal))
        },
        Expression::Call(call) => todo!(),
        Expression::Unary(unary) => todo!(),
        Expression::Binary(binary) => todo!(),
        Expression::Ternary(ternary) => todo!(),
        Expression::Block(statements) => {
            let mut statements_: Vec<TypedStatement> = vec![];
            
            for statement in statements {
                statements_.push(statements::check_type(statement, discovered_types, type_environment)?);
            }
            
            Ok(TypedExpression::Block {
                statements: statements_,
                type_: Type::Void
            })
        },
        Expression::Drop(symbol) => todo!(),
    }
}