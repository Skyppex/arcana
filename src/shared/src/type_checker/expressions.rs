use crate::parser::{Expression, VariableDeclaration, If, Assignment, Call, Unary, Binary, Ternary};

use super::{ast::{TypedExpression, Typed, ConditionBlock}, TypeEnvironment, Type, type_checker};

pub fn check_type<'a>(
    expression: &Expression,
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
                let initializer = check_type(initializer, type_environment)?;
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
            if let Type::Bool = check_type(&r#if.condition, type_environment)?.get_type() {
                return Err(format!("If condition must be of type bool"));
            }

            let if_block_type = check_type(&r#if.block, type_environment)?.get_type();

            if let Some(else_ifs) = else_ifs {
                for else_if in else_ifs {
                    if let Type::Bool = check_type(&else_if.condition, type_environment)?.get_type() {
                        return Err(format!("Else if condition must be of type bool"));
                    }

                    let else_if_block_type = check_type(&else_if.block, type_environment)?.get_type();

                    if if_block_type != else_if_block_type {
                        return Err(format!("If block type {:?} does not match else if block type {:?}", if_block_type, else_if_block_type));
                    }
                }
            }

            let else_type = if let Some(r#else) = r#else {
                Some(check_type(r#else, type_environment)?.get_type())
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
                    condition: Box::new(check_type(&r#if.condition, type_environment)?),
                    block: Box::new(check_type(&r#if.block, type_environment)?),
                },
                else_ifs: if let Some(else_ifs) = else_ifs {
                    let mut elifs: Vec<ConditionBlock> = vec![];
                    
                    for elif in else_ifs {
                        elifs.push(ConditionBlock {
                            condition: Box::new(check_type(&elif.condition, type_environment)?),
                            block: Box::new(check_type(&elif.block, type_environment)?),
                        })
                    }

                    Some(elifs)
                } else {
                    None
                },
                r#else: if let Some(r#else) = r#else {
                    Some(Box::new(check_type(r#else, type_environment)?))
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
            let member = check_type(member, type_environment)?;
            let initializer = check_type(initializer, type_environment)?;

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

                    Ok(TypedExpression::Member(type_checker::Member::Identifier {
                        symbol: symbol.clone()
                    }))
                },
                crate::parser::Member::MemberAccess { object, member, symbol } => todo!(),
            }
        },
        Expression::Literal(l) => todo!(),
        Expression::Call(Call {
            caller,
            arguments
        }) => todo!(),
        Expression::Unary(Unary {
            operator,
            expression
        }) => todo!(),
        Expression::Binary(Binary {
            left,
            operator,
            right
        }) => todo!(),
        Expression::Ternary(Ternary {
            condition,
            true_expression,
            false_expression
        }) => todo!(),
        Expression::Block(statements) => todo!(),
        Expression::Drop(identifier) => todo!(),
    }
}