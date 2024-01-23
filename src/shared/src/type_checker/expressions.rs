use std::collections::HashMap;

use crate::{parser::{self, Assignment, Expression, If, VariableDeclaration}, type_checker::ast::Literal};

use super::{ast::{TypedExpression,
        Typed,
        ConditionBlock,
        Member,
        FieldInitializer,
        TypedStatement,
        UnaryOperator,
        BinaryOperator,
        UnionMemberFieldInitializers
    },
    TypeEnvironment,
    Type,
    statements,
    DiscoveredType,
    FullName
};

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

            type_environment.add_variable(identifier.clone(), type_.clone());

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
            let if_else_environment = &mut type_environment.new_child();
            let if_condition = check_type(&r#if.condition, discovered_types, if_else_environment)?;
            if let Type::Bool = if_condition.get_type() {
                return Err(format!("If condition must be of type bool"));
            }

            let if_block = check_type(&r#if.block, discovered_types, if_else_environment)?;
            let if_block_type = if_block.get_type();

            let mut else_if_condition_blocks = vec![];

            if let Some(else_ifs) = else_ifs {
                for else_if in else_ifs {
                    let else_if_environment = &mut type_environment.new_child();
                    let else_if_condition = check_type(&else_if.condition, discovered_types, else_if_environment)?;
                    if let Type::Bool = else_if_condition.get_type() {
                        return Err(format!("Else if condition must be of type bool"));
                    }

                    let else_if_block = check_type(&else_if.block, discovered_types, else_if_environment)?;
                    let else_if_block_type = else_if_block.get_type();

                    if if_block_type != else_if_block_type {
                        return Err(format!("If block type {:?} does not match else if block type {:?}", if_block_type, else_if_block_type));
                    }

                    else_if_condition_blocks.push(ConditionBlock {
                        condition: Box::new(else_if_condition),
                        block: Box::new(else_if_block),
                    });
                }
            }

            let else_block = if let Some(r#else) = r#else {
                Some(check_type(r#else, discovered_types, if_else_environment)?)
            } else {
                None
            };

            let else_type = else_block.clone().map(|e| e.get_type());

            if let Some(else_type) = else_type {
                if if_block_type != else_type {
                    return Err(format!("If block type {:?} does not match else block type {:?}", if_block_type, else_type));
                }
            }

            Ok(TypedExpression::If {
                r#if: ConditionBlock {
                    condition: Box::new(if_condition.clone()),
                    block: Box::new(if_block.clone()),
                },
                else_ifs: else_if_condition_blocks,
                r#else: else_block.map(|e| Box::new(e.clone())),
                type_: if_block_type.clone()
            })
        },
        Expression::Assignment(Assignment {
            member,
            initializer
        }) => {
            let member = check_type(&Expression::Member(*member.clone()), discovered_types, type_environment)?;
            let initializer = check_type(initializer, discovered_types, type_environment)?;

            if member.get_type() != initializer.get_type() {
                return Err(format!("Member type {:?} does not match initializer type {:?}", member.get_type(), initializer.get_type()));
            }

            let TypedExpression::Member(member) = member else {
                return Err("Expected member expression".to_string());
            };

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
                        .or(type_environment.get_type(symbol))
                        .ok_or_else(|| format!("Unexpected variable: {}", symbol))?.clone();

                    Ok(TypedExpression::Member(Member::Identifier {
                        symbol: symbol.clone(),
                        type_
                    }))
                },
                crate::parser::Member::MemberAccess {
                    object,
                    member,
                    symbol: _
                } => {
                    check_type_member_access(object, discovered_types, type_environment, member)
                },
            }
        },
        Expression::Literal(l) => {
            let literal = match l {
                parser::Literal::Unit => Literal::Unit,
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
                    let field_initializers: Result<Vec<FieldInitializer>, String> = {
                        let mut field_initializers_: Vec<FieldInitializer> = vec![];
                        for field_initializer in field_initializers {
                            let field_initializer = FieldInitializer {
                                identifier: field_initializer.identifier.clone(),
                                initializer: check_type(
                                    &field_initializer.initializer,
                                    discovered_types, type_environment)?
                            };
                            field_initializers_.push(field_initializer);
                        }
                        Ok(field_initializers_)
                    };

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
                    let field_initializers: Result<UnionMemberFieldInitializers, String> = {
                        let mut field_initializers_: UnionMemberFieldInitializers = UnionMemberFieldInitializers::None;
                        match field_initializers {
                            parser::UnionMemberFieldInitializers::None => (),
                            parser::UnionMemberFieldInitializers::Named(field_initializers) => {
                                let mut field_initializers__: HashMap<String, TypedExpression> = HashMap::new();
                                for (identifier, initializer) in field_initializers {
                                    field_initializers__.insert(identifier.clone(), check_type(
                                        &initializer,
                                        discovered_types, type_environment)?
                                    );
                                }
                                field_initializers_ = UnionMemberFieldInitializers::Named(field_initializers__);
                            },
                            parser::UnionMemberFieldInitializers::Unnamed(field_initializers) => {
                                let mut field_initializers__: Vec<TypedExpression> = vec![];
                                for initializer in field_initializers {
                                    field_initializers__.push(check_type(
                                        &initializer,
                                        discovered_types, type_environment)?
                                    );
                                }
                                field_initializers_ = UnionMemberFieldInitializers::Unnamed(field_initializers__);
                            },
                        }
                        Ok(field_initializers_)
                    };

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
        Expression::Call(call) => {
            let Expression::Member(member) = *call.caller.clone() else {
                Err("Function must be a member".to_string())?
            };

            let caller = check_type(&call.caller, discovered_types, type_environment)?;

            match member {
                parser::Member::Identifier { symbol } => {
                    let function_type = type_environment.get_type(&symbol)
                        .ok_or_else(|| format!("Unexpected type: {}", symbol))?;

                    let Type::Function(function) = function_type.clone() else {
                        Err(format!("Expected function type, found {}", function_type.full_name()))?
                    };

                    let mut args = vec![];
                    for (i, (.., type_)) in function.clone().parameters.iter().enumerate() {
                        let Some(arg) = call.arguments.get(i) else {
                            Err("Not enough arguments")?
                        };
                        
                        let arg_typed_exression = check_type(arg, discovered_types, type_environment)?;

                        if arg_typed_exression.get_deep_type() != type_.clone() {
                            Err(format!("Argument {} type {} does not match parameter type {}", i, arg_typed_exression.get_type(), type_))?
                        }

                        args.push(arg_typed_exression);
                    }

                    Ok(TypedExpression::Call {
                        caller: Box::new(caller),
                        arguments: args,
                        type_: *function.return_type
                    })
                },
                parser::Member::MemberAccess {
                    object,
                    member,
                    symbol
                } => todo!(),
            }
        },
        Expression::Unary(unary) => {
            let expression = check_type(&unary.expression, discovered_types, type_environment)?;
            let type_ = expression.get_type();

            Ok(TypedExpression::Unary {
                operator: match unary.operator {
                    parser::UnaryOperator::Identity => UnaryOperator::Identity,
                    parser::UnaryOperator::Negate => UnaryOperator::Negate,
                    parser::UnaryOperator::LogicalNot => UnaryOperator::LogicalNot,
                    parser::UnaryOperator::BitwiseNot => UnaryOperator::BitwiseNot,
                },
                expression: Box::new(expression),
                type_
            })
        
        },
        Expression::Binary(binary) => {
            let left = check_type(&binary.left, discovered_types, type_environment)?;
            let right = check_type(&binary.right, discovered_types, type_environment)?;
            let left_type = left.get_type();
            let right_type = right.get_type();

            let operator = match binary.operator {
                    parser::BinaryOperator::Add => BinaryOperator::Add,
                    parser::BinaryOperator::Subtract => BinaryOperator::Subtract,
                    parser::BinaryOperator::Multiply => BinaryOperator::Multiply,
                    parser::BinaryOperator::Divide => BinaryOperator::Divide,
                    parser::BinaryOperator::Modulo => BinaryOperator::Modulo,
                    parser::BinaryOperator::BitwiseAnd => BinaryOperator::BitwiseAnd,
                    parser::BinaryOperator::BitwiseOr => BinaryOperator::BitwiseOr,
                    parser::BinaryOperator::BitwiseXor => BinaryOperator::BitwiseXor,
                    parser::BinaryOperator::BitwiseLeftShift => BinaryOperator::BitwiseLeftShift,
                    parser::BinaryOperator::BitwiseRightShift => BinaryOperator::BitwiseRightShift,
                    parser::BinaryOperator::BooleanLogicalAnd => BinaryOperator::BooleanLogicalAnd,
                    parser::BinaryOperator::BooleanLogicalOr => BinaryOperator::BooleanLogicalOr,
                    parser::BinaryOperator::Equal => BinaryOperator::Equal,
                    parser::BinaryOperator::NotEqual => BinaryOperator::NotEqual,
                    parser::BinaryOperator::LessThan => BinaryOperator::LessThan,
                    parser::BinaryOperator::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
                    parser::BinaryOperator::GreaterThan => BinaryOperator::GreaterThan,
                    parser::BinaryOperator::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
                };

            let type_ = get_binop_type(&left_type, &operator, &right_type);

            Ok(TypedExpression::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
                type_
            })
        
        },
        Expression::Ternary(ternary) => {
            let ternary_environment = &mut type_environment.new_child();
            let condition = check_type(&ternary.condition, discovered_types, ternary_environment)?;
            let true_expression = check_type(&ternary.true_expression, discovered_types, ternary_environment)?;
            let false_expression = check_type(&ternary.false_expression, discovered_types, ternary_environment)?;

            let Type::Bool = condition.get_type() else {
                return Err(format!("Ternary condition must be of type bool"));
            };

            if true_expression.get_type() != false_expression.get_type() {
                return Err(format!("Ternary true expression type {:?} does not match false expression type {:?}", true_expression.get_type(), false_expression.get_type()));
            }

            Ok(TypedExpression::Ternary {
                condition: Box::new(condition),
                true_expression: Box::new(true_expression.clone()),
                false_expression: Box::new(false_expression),
                type_: true_expression.get_type()
            })
        
        },
        Expression::Block(statements) => {
            let mut statements_: Vec<TypedStatement> = vec![];
            
            for statement in statements {
                statements_.push(statements::check_type(statement, discovered_types, type_environment)?);
            }

            let mut type_ = Type::Void;
            for statement in statements_.clone() {
                match statement {
                    TypedStatement::Expression(e) => {
                        type_ = e.get_type();
                    },
                    _ => continue
                }
            }
            
            Ok(TypedExpression::Block {
                statements: statements_,
                type_
            })
        },
        Expression::Drop(symbol) => {
            let type_ = type_environment.get_variable(symbol)
                .ok_or_else(|| format!("Unexpected variable: {}", symbol))?.clone();

            Ok(TypedExpression::Drop {
                identifier: symbol.clone(),
                type_
            })
        },
    }
}

fn check_type_member_access(object: &Box<Expression>, discovered_types: &Vec<DiscoveredType>, type_environment: &mut TypeEnvironment<'_>, member: &Box<parser::Member>) -> Result<TypedExpression, String> {
    let object_type_expression = check_type(object, discovered_types, type_environment)?;
    let object_type  = object_type_expression.get_type();
    check_type_member_access_recurse(object_type, member, type_environment, object_type_expression, discovered_types)
}

fn check_type_member_access_recurse(
    object_type: Type,
    member: &Box<parser::Member>,
    type_environment: &mut TypeEnvironment<'_>,
    object_type_expression: TypedExpression,
    discovered_types: &Vec<DiscoveredType>) -> Result<TypedExpression, String> {
    match object_type {
        Type::Struct(struct_) => {
            match *member.clone() {
                parser::Member::Identifier { symbol } => {
                    let field_type = struct_.fields.get(&symbol)
                        .ok_or(format!("Struct {} does not have a field called '{}'", struct_.name, symbol))?;

                    if !type_environment.lookup_type(&field_type) {
                        return Err(format!("Unexpected type: {}", field_type.full_name()));
                    }

                    let identifier_type = match field_type {
                        Type::StructField(struct_field) => *struct_field.field_type.clone(),
                        _ => field_type.clone()
                    };

                    Ok(TypedExpression::Member(Member::MemberAccess {
                        object: Box::new(object_type_expression),
                        member: Box::new(Member::Identifier {
                            symbol: symbol.clone(),
                            type_: identifier_type.clone()
                        }),
                        symbol: symbol.clone(),
                        type_: field_type.clone()
                    }))
                },
                parser::Member::MemberAccess {
                    object,
                    member,
                    symbol: _
                } => {
                    check_type_member_access(&object, discovered_types, type_environment, &member)
                },
            }
        },
        Type::StructField(struct_field) => {
            check_type_member_access_recurse(
                *struct_field.field_type,
                member, type_environment, object_type_expression, discovered_types)
        },
        _ => Err("Member access is only supported on structs".to_string())
    }
}

fn get_binop_type(left_type: &Type, operator: &BinaryOperator, right_type: &Type) -> Type {
    match (left_type, operator, right_type) {
        (Type::I8, BinaryOperator::Add, Type::I8) => Type::I8,
        (Type::I16, BinaryOperator::Add, Type::I16) => Type::I16,
        (Type::I32, BinaryOperator::Add, Type::I32) => Type::I32,
        (Type::I64, BinaryOperator::Add, Type::I64) => Type::I64,
        (Type::I128, BinaryOperator::Add, Type::I128) => Type::I128,
        (Type::U8, BinaryOperator::Add, Type::U8) => Type::U8,
        (Type::U16, BinaryOperator::Add, Type::U16) => Type::U16,
        (Type::U32, BinaryOperator::Add, Type::U32) => Type::U32,
        (Type::U64, BinaryOperator::Add, Type::U64) => Type::U64,
        (Type::U128, BinaryOperator::Add, Type::U128) => Type::U128,
        (Type::F32, BinaryOperator::Add, Type::F32) => Type::F32,
        (Type::F64, BinaryOperator::Add, Type::F64) => Type::F64,
        (Type::String, BinaryOperator::Add, Type::String) => Type::String,
        (Type::Char, BinaryOperator::Add, Type::Char) => Type::String,
        (Type::I8, BinaryOperator::Subtract, Type::I8) => Type::I8,
        (Type::I16, BinaryOperator::Subtract, Type::I16) => Type::I16,
        (Type::I32, BinaryOperator::Subtract, Type::I32) => Type::I32,
        (Type::I64, BinaryOperator::Subtract, Type::I64) => Type::I64,
        (Type::I128, BinaryOperator::Subtract, Type::I128) => Type::I128,
        (Type::U8, BinaryOperator::Subtract, Type::U8) => Type::U8,
        (Type::U16, BinaryOperator::Subtract, Type::U16) => Type::U16,
        (Type::U32, BinaryOperator::Subtract, Type::U32) => Type::U32,
        (Type::U64, BinaryOperator::Subtract, Type::U64) => Type::U64,
        (Type::U128, BinaryOperator::Subtract, Type::U128) => Type::U128,
        (Type::F32, BinaryOperator::Subtract, Type::F32) => Type::F32,
        (Type::F64, BinaryOperator::Subtract, Type::F64) => Type::F64,
        (Type::I8, BinaryOperator::Multiply, Type::I8) => Type::I8,
        (Type::I16, BinaryOperator::Multiply, Type::I16) => Type::I16,
        (Type::I32, BinaryOperator::Multiply, Type::I32) => Type::I32,
        (Type::I64, BinaryOperator::Multiply, Type::I64) => Type::I64,
        (Type::I128, BinaryOperator::Multiply, Type::I128) => Type::I128,
        (Type::U8, BinaryOperator::Multiply, Type::U8) => Type::U8,
        (Type::U16, BinaryOperator::Multiply, Type::U16) => Type::U16,
        (Type::U32, BinaryOperator::Multiply, Type::U32) => Type::U32,
        (Type::U64, BinaryOperator::Multiply, Type::U64) => Type::U64,
        (Type::U128, BinaryOperator::Multiply, Type::U128) => Type::U128,
        (Type::F32, BinaryOperator::Multiply, Type::F32) => Type::F32,
        (Type::F64, BinaryOperator::Multiply, Type::F64) => Type::F64,
        (Type::I8, BinaryOperator::Divide, Type::I8) => Type::I8,
        (Type::I16, BinaryOperator::Divide, Type::I16) => Type::I16,
        (Type::I32, BinaryOperator::Divide, Type::I32) => Type::I32,
        (Type::I64, BinaryOperator::Divide, Type::I64) => Type::I64,
        (Type::I128, BinaryOperator::Divide, Type::I128) => Type::I128,
        (Type::U8, BinaryOperator::Divide, Type::U8) => Type::U8,
        (Type::U16, BinaryOperator::Divide, Type::U16) => Type::U16,
        (Type::U32, BinaryOperator::Divide, Type::U32) => Type::U32,
        (Type::U64, BinaryOperator::Divide, Type::U64) => Type::U64,
        (Type::U128, BinaryOperator::Divide, Type::U128) => Type::U128,
        (Type::F32, BinaryOperator::Divide, Type::F32) => Type::F32,
        (Type::F64, BinaryOperator::Divide, Type::F64) => Type::F64,
        (Type::I8, BinaryOperator::Modulo, Type::I8) => Type::I8,
        (Type::I16, BinaryOperator::Modulo, Type::I16) => Type::I16,
        (Type::I32, BinaryOperator::Modulo, Type::I32) => Type::I32,
        (Type::I64, BinaryOperator::Modulo, Type::I64) => Type::I64,
        (Type::I128, BinaryOperator::Modulo, Type::I128) => Type::I128,
        (Type::U8, BinaryOperator::Modulo, Type::U8) => Type::U8,
        (Type::U16, BinaryOperator::Modulo, Type::U16) => Type::U16,
        (Type::U32, BinaryOperator::Modulo, Type::U32) => Type::U32,
        (Type::U64, BinaryOperator::Modulo, Type::U64) => Type::U64,
        (Type::U128, BinaryOperator::Modulo, Type::U128) => Type::U128,
        (Type::F32, BinaryOperator::Modulo, Type::F32) => Type::F32,
        (Type::F64, BinaryOperator::Modulo, Type::F64) => Type::F64,
        (Type::I8, BinaryOperator::BitwiseAnd, Type::I8) => Type::I8,
        (Type::I16, BinaryOperator::BitwiseAnd, Type::I16) => Type::I16,
        (Type::I32, BinaryOperator::BitwiseAnd, Type::I32) => Type::I32,
        (Type::I64, BinaryOperator::BitwiseAnd, Type::I64) => Type::I64,
        (Type::I128, BinaryOperator::BitwiseAnd, Type::I128) => Type::I128,
        (Type::U8, BinaryOperator::BitwiseAnd, Type::U8) => Type::U8,
        (Type::U16, BinaryOperator::BitwiseAnd, Type::U16) => Type::U16,
        (Type::U32, BinaryOperator::BitwiseAnd, Type::U32) => Type::U32,
        (Type::U64, BinaryOperator::BitwiseAnd, Type::U64) => Type::U64,
        (Type::U128, BinaryOperator::BitwiseAnd, Type::U128) => Type::U128,
        (Type::I8, BinaryOperator::BitwiseOr, Type::I8) => Type::I8,
        (Type::I16, BinaryOperator::BitwiseOr, Type::I16) => Type::I16,
        (Type::I32, BinaryOperator::BitwiseOr, Type::I32) => Type::I32,
        (Type::I64, BinaryOperator::BitwiseOr, Type::I64) => Type::I64,
        (Type::I128, BinaryOperator::BitwiseOr, Type::I128) => Type::I128,
        (Type::U8, BinaryOperator::BitwiseOr, Type::U8) => Type::U8,
        (Type::U16, BinaryOperator::BitwiseOr, Type::U16) => Type::U16,
        (Type::U32, BinaryOperator::BitwiseOr, Type::U32) => Type::U32,
        (Type::U64, BinaryOperator::BitwiseOr, Type::U64) => Type::U64,
        (Type::U128, BinaryOperator::BitwiseOr, Type::U128) => Type::U128,
        (Type::I8, BinaryOperator::BitwiseXor, Type::I8) => Type::I8,
        (Type::I16, BinaryOperator::BitwiseXor, Type::I16) => Type::I16,
        (Type::I32, BinaryOperator::BitwiseXor, Type::I32) => Type::I32,
        (Type::I64, BinaryOperator::BitwiseXor, Type::I64) => Type::I64,
        (Type::I128, BinaryOperator::BitwiseXor, Type::I128) => Type::I128,
        (Type::U8, BinaryOperator::BitwiseXor, Type::U8) => Type::U8,
        (Type::U16, BinaryOperator::BitwiseXor, Type::U16) => Type::U16,
        (Type::U32, BinaryOperator::BitwiseXor, Type::U32) => Type::U32,
        (Type::U64, BinaryOperator::BitwiseXor, Type::U64) => Type::U64,
        (Type::U128, BinaryOperator::BitwiseXor, Type::U128) => Type::U128,
        (Type::I8, BinaryOperator::BitwiseLeftShift, Type::I8) => Type::I8,
        (Type::I16, BinaryOperator::BitwiseLeftShift, Type::I16) => Type::I16,
        (Type::I32, BinaryOperator::BitwiseLeftShift, Type::I32) => Type::I32,
        (Type::I64, BinaryOperator::BitwiseLeftShift, Type::I64) => Type::I64,
        (Type::I128, BinaryOperator::BitwiseLeftShift, Type::I128) => Type::I128,
        (Type::U8, BinaryOperator::BitwiseLeftShift, Type::U8) => Type::U8,
        (Type::U16, BinaryOperator::BitwiseLeftShift, Type::U16) => Type::U16,
        (Type::U32, BinaryOperator::BitwiseLeftShift, Type::U32) => Type::U32,
        (Type::U64, BinaryOperator::BitwiseLeftShift, Type::U64) => Type::U64,
        (Type::U128, BinaryOperator::BitwiseLeftShift, Type::U128) => Type::U128,
        (Type::I8, BinaryOperator::BitwiseRightShift, Type::I8) => Type::I8,
        (Type::I16, BinaryOperator::BitwiseRightShift, Type::I16) => Type::I16,
        (Type::I32, BinaryOperator::BitwiseRightShift, Type::I32) => Type::I32,
        (Type::I64, BinaryOperator::BitwiseRightShift, Type::I64) => Type::I64,
        (Type::I128, BinaryOperator::BitwiseRightShift, Type::I128) => Type::I128,
        (Type::U8, BinaryOperator::BitwiseRightShift, Type::U8) => Type::U8,
        (Type::U16, BinaryOperator::BitwiseRightShift, Type::U16) => Type::U16,
        (Type::U32, BinaryOperator::BitwiseRightShift, Type::U32) => Type::U32,
        (Type::U64, BinaryOperator::BitwiseRightShift, Type::U64) => Type::U64,
        (Type::U128, BinaryOperator::BitwiseRightShift, Type::U128) => Type::U128,
        (Type::I8, BinaryOperator::Equal, Type::I8) => Type::Bool,
        (Type::I16, BinaryOperator::Equal, Type::I16) => Type::Bool,
        (Type::I32, BinaryOperator::Equal, Type::I32) => Type::Bool,
        (Type::I64, BinaryOperator::Equal, Type::I64) => Type::Bool,
        (Type::I128, BinaryOperator::Equal, Type::I128) => Type::Bool,
        (Type::U8, BinaryOperator::Equal, Type::U8) => Type::Bool,
        (Type::U16, BinaryOperator::Equal, Type::U16) => Type::Bool,
        (Type::U32, BinaryOperator::Equal, Type::U32) => Type::Bool,
        (Type::U64, BinaryOperator::Equal, Type::U64) => Type::Bool,
        (Type::U128, BinaryOperator::Equal, Type::U128) => Type::Bool,
        (Type::F32, BinaryOperator::Equal, Type::F32) => Type::Bool,
        (Type::F64, BinaryOperator::Equal, Type::F64) => Type::Bool,
        (Type::String, BinaryOperator::Equal, Type::String) => Type::Bool,
        (Type::Char, BinaryOperator::Equal, Type::Char) => Type::Bool,
        (Type::Bool, BinaryOperator::Equal, Type::Bool) => Type::Bool,
        (Type::I8, BinaryOperator::NotEqual, Type::I8) => Type::Bool,
        (Type::I16, BinaryOperator::NotEqual, Type::I16) => Type::Bool,
        (Type::I32, BinaryOperator::NotEqual, Type::I32) => Type::Bool,
        (Type::I64, BinaryOperator::NotEqual, Type::I64) => Type::Bool,
        (Type::I128, BinaryOperator::NotEqual, Type::I128) => Type::Bool,
        (Type::U8, BinaryOperator::NotEqual, Type::U8) => Type::Bool,
        (Type::U16, BinaryOperator::NotEqual, Type::U16) => Type::Bool,
        (Type::U32, BinaryOperator::NotEqual, Type::U32) => Type::Bool,
        (Type::U64, BinaryOperator::NotEqual, Type::U64) => Type::Bool,
        (Type::U128, BinaryOperator::NotEqual, Type::U128) => Type::Bool,
        (Type::F32, BinaryOperator::NotEqual, Type::F32) => Type::Bool,
        (Type::F64, BinaryOperator::NotEqual, Type::F64) => Type::Bool,
        (Type::String, BinaryOperator::NotEqual, Type::String) => Type::Bool,
        (Type::Char, BinaryOperator::NotEqual, Type::Char) => Type::Bool,
        (Type::Bool, BinaryOperator::NotEqual, Type::Bool) => Type::Bool,
        (Type::I8, BinaryOperator::LessThan, Type::I8) => Type::Bool,
        (Type::I16, BinaryOperator::LessThan, Type::I16) => Type::Bool,
        (Type::I32, BinaryOperator::LessThan, Type::I32) => Type::Bool,
        (Type::I64, BinaryOperator::LessThan, Type::I64) => Type::Bool,
        (Type::I128, BinaryOperator::LessThan, Type::I128) => Type::Bool,
        (Type::U8, BinaryOperator::LessThan, Type::U8) => Type::Bool,
        (Type::U16, BinaryOperator::LessThan, Type::U16) => Type::Bool,
        (Type::U32, BinaryOperator::LessThan, Type::U32) => Type::Bool,
        (Type::U64, BinaryOperator::LessThan, Type::U64) => Type::Bool,
        (Type::U128, BinaryOperator::LessThan, Type::U128) => Type::Bool,
        (Type::F32, BinaryOperator::LessThan, Type::F32) => Type::Bool,
        (Type::F64, BinaryOperator::LessThan, Type::F64) => Type::Bool,
        (Type::I8, BinaryOperator::LessThanOrEqual, Type::I8) => Type::Bool,
        (Type::I16, BinaryOperator::LessThanOrEqual, Type::I16) => Type::Bool,
        (Type::I32, BinaryOperator::LessThanOrEqual, Type::I32) => Type::Bool,
        (Type::I64, BinaryOperator::LessThanOrEqual, Type::I64) => Type::Bool,
        (Type::I128, BinaryOperator::LessThanOrEqual, Type::I128) => Type::Bool,
        (Type::U8, BinaryOperator::LessThanOrEqual, Type::U8) => Type::Bool,
        (Type::U16, BinaryOperator::LessThanOrEqual, Type::U16) => Type::Bool,
        (Type::U32, BinaryOperator::LessThanOrEqual, Type::U32) => Type::Bool,
        (Type::U64, BinaryOperator::LessThanOrEqual, Type::U64) => Type::Bool,
        (Type::U128, BinaryOperator::LessThanOrEqual, Type::U128) => Type::Bool,
        (Type::F32, BinaryOperator::LessThanOrEqual, Type::F32) => Type::Bool,
        (Type::F64, BinaryOperator::LessThanOrEqual, Type::F64) => Type::Bool,
        (Type::I8, BinaryOperator::GreaterThan, Type::I8) => Type::Bool,
        (Type::I16, BinaryOperator::GreaterThan, Type::I16) => Type::Bool,
        (Type::I32, BinaryOperator::GreaterThan, Type::I32) => Type::Bool,
        (Type::I64, BinaryOperator::GreaterThan, Type::I64) => Type::Bool,
        (Type::I128, BinaryOperator::GreaterThan, Type::I128) => Type::Bool,
        (Type::U8, BinaryOperator::GreaterThan, Type::U8) => Type::Bool,
        (Type::U16, BinaryOperator::GreaterThan, Type::U16) => Type::Bool,
        (Type::U32, BinaryOperator::GreaterThan, Type::U32) => Type::Bool,
        (Type::U64, BinaryOperator::GreaterThan, Type::U64) => Type::Bool,
        (Type::U128, BinaryOperator::GreaterThan, Type::U128) => Type::Bool,
        (Type::F32, BinaryOperator::GreaterThan, Type::F32) => Type::Bool,
        (Type::F64, BinaryOperator::GreaterThan, Type::F64) => Type::Bool,
        (Type::I8, BinaryOperator::GreaterThanOrEqual, Type::I8) => Type::Bool,
        (Type::I16, BinaryOperator::GreaterThanOrEqual, Type::I16) => Type::Bool,
        (Type::I32, BinaryOperator::GreaterThanOrEqual, Type::I32) => Type::Bool,
        (Type::I64, BinaryOperator::GreaterThanOrEqual, Type::I64) => Type::Bool,
        (Type::I128, BinaryOperator::GreaterThanOrEqual, Type::I128) => Type::Bool,
        (Type::U8, BinaryOperator::GreaterThanOrEqual, Type::U8) => Type::Bool,
        (Type::U16, BinaryOperator::GreaterThanOrEqual, Type::U16) => Type::Bool,
        (Type::U32, BinaryOperator::GreaterThanOrEqual, Type::U32) => Type::Bool,
        (Type::U64, BinaryOperator::GreaterThanOrEqual, Type::U64) => Type::Bool,
        (Type::U128, BinaryOperator::GreaterThanOrEqual, Type::U128) => Type::Bool,
        (Type::F32, BinaryOperator::GreaterThanOrEqual, Type::F32) => Type::Bool,
        (Type::F64, BinaryOperator::GreaterThanOrEqual, Type::F64) => Type::Bool,
        (Type::Bool, BinaryOperator::BooleanLogicalAnd, Type::Bool) => Type::Bool,
        (Type::Bool, BinaryOperator::BooleanLogicalOr, Type::Bool) => Type::Bool,
        (Type::Literal { name: _, type_ }, operator, right_type) => get_binop_type(type_, operator, right_type),
        (left_type, operator, Type::Literal { name: _, type_ }) => get_binop_type(left_type, operator, type_),
        _ => panic!("Unexpected binary operator {:?} for types {:?} and {:?}", operator, left_type, right_type)
    }
}