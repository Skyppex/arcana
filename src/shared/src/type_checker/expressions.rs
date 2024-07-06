use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    parser::{self, Assignment, Expression, If, VariableDeclaration, While},
    type_checker::ast::Literal,
    types::TypeIdentifier,
};

use super::{
    ast::{
        BinaryOperator, Block, EnumMemberFieldInitializers, FieldInitializer, Member, Typed,
        TypedExpression, TypedParameter, TypedStatement, UnaryOperator,
    },
    scope::ScopeType,
    statements, type_equals, DiscoveredType, Enum, EnumMember, FullName, Function, Rcrc, Struct,
    Type, TypeEnvironment,
};

pub fn check_type<'a>(
    expression: &Expression,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rc<RefCell<TypeEnvironment>>,
) -> Result<TypedExpression, String> {
    match expression {
        Expression::None => Ok(TypedExpression::None),
        Expression::VariableDeclaration(VariableDeclaration {
            mutable,
            type_annotation,
            identifier,
            initializer,
        }) => {
            let type_ = type_environment
                .borrow()
                .get_type_from_annotation(type_annotation, type_environment.clone())?;

            let initializer = if let Some(initializer) = initializer {
                let initializer =
                    check_type(initializer, discovered_types, type_environment.clone())?;
                Some(Box::new(initializer))
            } else {
                None
            };

            if let Some(initializer) = &initializer {
                if !type_equals(&initializer.get_type(), &type_) {
                    return Err(format!(
                        "Initializer type {} does not match variable type {}",
                        initializer.get_type(),
                        type_
                    ));
                }
            }

            type_environment
                .borrow_mut()
                .add_variable(identifier.clone(), type_.clone());

            Ok(TypedExpression::VariableDeclaration {
                mutable: *mutable,
                identifier: identifier.clone(),
                initializer,
                type_,
            })
        }
        Expression::If(If {
            condition,
            true_expression,
            false_expression,
        }) => {
            let if_else_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
                type_environment.clone(),
            )));

            let if_condition =
                check_type(&condition, discovered_types, if_else_environment.clone())?;

            if !type_equals(&if_condition.get_type(), &Type::Bool) {
                return Err(format!(
                    "If condition must be of type bool but found {}",
                    if_condition.get_type()
                ));
            };

            let if_block = check_type(
                &true_expression,
                discovered_types,
                if_else_environment.clone(),
            )?;
            let if_block_type = if_block.get_deep_type();

            let else_block = if let Some(false_expression) = false_expression {
                Some(check_type(
                    false_expression,
                    discovered_types,
                    if_else_environment,
                )?)
            } else {
                None
            };

            let else_type = else_block.clone().map(|e| e.get_deep_type());

            let type_ = if !is_option(&else_type) {
                if let Some(else_type) = else_type {
                    if !type_equals(&if_block_type, &else_type) {
                        return Err(format!(
                            "If block type {:?} does not match else block type {:?}",
                            if_block_type, else_type
                        ));
                    }

                    if_block_type.clone()
                } else {
                    Type::option_of(if_block_type.clone())
                }
            } else {
                Type::option_of(if_block_type.clone())
            };

            Ok(TypedExpression::If {
                condition: Box::new(if_condition.clone()),
                true_expression: Box::new(if_block.clone()),
                false_expression: else_block.map(|e| Box::new(e.clone())),
                type_,
            })
        }
        Expression::Assignment(Assignment {
            member,
            initializer,
        }) => {
            let member = check_type(
                &Expression::Member(*member.clone()),
                discovered_types,
                type_environment.clone(),
            )?;
            let initializer = check_type(initializer, discovered_types, type_environment)?;

            if !type_equals(&member.get_type(), &initializer.get_type()) {
                return Err(format!(
                    "Member type {} does not match initializer type {}",
                    member.get_type(),
                    initializer.get_type()
                ));
            }

            let TypedExpression::Member(member) = member else {
                return Err("Expected member expression".to_string());
            };

            Ok(TypedExpression::Assignment {
                member: Box::new(member),
                initializer: Box::new(initializer.clone()),
                type_: initializer.get_type(),
            })
        }
        Expression::Member(member) => match member {
            crate::parser::Member::Identifier { symbol } => {
                let type_ = type_environment
                    .borrow()
                    .get_variable(symbol)
                    .or_else(|| type_environment.borrow().get_type_from_str(symbol))
                    .ok_or_else(|| format!("Unexpected variable: {}", symbol))?
                    .clone();

                Ok(TypedExpression::Member(Member::Identifier {
                    symbol: symbol.clone(),
                    type_,
                }))
            }
            crate::parser::Member::MemberAccess { object, member, .. } => {
                check_type_member_access(object, discovered_types, type_environment, member, false)
            }
            crate::parser::Member::ParamPropagation { object, member, .. } => {
                check_type_member_access(object, discovered_types, type_environment, member, true)
            }
        },
        Expression::Literal(l) => {
            let literal = match l {
                parser::Literal::Unit => Literal::Unit,
                parser::Literal::Int(v) => Literal::Int(*v),
                parser::Literal::UInt(v) => Literal::UInt(*v),
                parser::Literal::Float(v) => Literal::Float(*v),
                parser::Literal::String(v) => Literal::String(v.clone()),
                parser::Literal::Char(v) => Literal::Char(*v),
                parser::Literal::Bool(v) => Literal::Bool(*v),
                parser::Literal::Array(v) => {
                    let v: Result<(Vec<TypedExpression>, Type), String> = {
                        let mut v_: Vec<TypedExpression> = vec![];
                        let mut previous_type = Type::Void;

                        for e in v {
                            let value = check_type(&e, discovered_types, type_environment.clone())?;
                            let type_ = value.get_deep_type();

                            if !type_equals(&previous_type, &Type::Void)
                                && !type_equals(&type_, &previous_type)
                            {
                                return Err(format!("Array element type {:?} does not match previous element type {:?}", type_, previous_type));
                            }

                            previous_type = type_.clone();
                            v_.push(value);
                        }

                        Ok((v_, previous_type.clone()))
                    };

                    let v = v?;
                    Literal::Array {
                        values: v.0,
                        type_: v.1,
                    }
                }
                parser::Literal::Struct {
                    type_annotation,
                    field_initializers,
                } => {
                    let field_initializers: Result<Vec<FieldInitializer>, String> = {
                        let mut field_initializers_: Vec<FieldInitializer> = vec![];
                        for field_initializer in field_initializers {
                            let field_initializer = FieldInitializer {
                                identifier: field_initializer.identifier.clone(),
                                initializer: check_type(
                                    &field_initializer.initializer,
                                    discovered_types,
                                    type_environment.clone(),
                                )?,
                            };
                            field_initializers_.push(field_initializer);
                        }
                        Ok(field_initializers_)
                    };

                    let type_ = type_environment
                        .borrow()
                        .get_type_from_annotation(type_annotation, type_environment.clone())?;

                    let Type::Struct(Struct { fields, .. }) = type_.clone() else {
                        Err(format!("{} is not a struct", type_.full_name()))?
                    };

                    let field_initializers = field_initializers?;

                    for (field, initializer) in fields.iter().zip(field_initializers.iter()) {
                        let field_type = field.1;
                        let initializer_type = initializer.initializer.get_type();

                        if !type_equals(field_type, &initializer_type) {
                            return Err(format!(
                                "Field type {} does not match initializer type {}",
                                field_type, initializer_type
                            ));
                        }
                    }

                    Literal::Struct {
                        type_annotation: type_annotation.clone(),
                        field_initializers,
                        type_,
                    }
                }
                parser::Literal::Enum {
                    type_annotation,
                    member,
                    field_initializers,
                } => {
                    let field_initializers: Result<EnumMemberFieldInitializers, String> = {
                        let field_initializers = match field_initializers {
                            parser::EnumMemberFieldInitializers::None => {
                                EnumMemberFieldInitializers::None
                            }
                            parser::EnumMemberFieldInitializers::Named(field_initializers) => {
                                let mut fis: HashMap<String, TypedExpression> = HashMap::new();

                                for (identifier, initializer) in field_initializers {
                                    fis.insert(
                                        identifier.clone(),
                                        check_type(
                                            &initializer,
                                            discovered_types,
                                            type_environment.clone(),
                                        )?,
                                    );
                                }

                                EnumMemberFieldInitializers::Named(fis)
                            }
                        };

                        Ok(field_initializers)
                    };

                    let type_ = type_environment
                        .borrow()
                        .get_type_from_annotation(type_annotation, type_environment.clone())?;

                    let Type::Enum(Enum { members, .. }) = type_.clone() else {
                        Err(format!("{} is not an enum", type_.full_name()))?
                    };

                    let Some(Type::EnumMember(EnumMember { fields, .. })) = members.get(member)
                    else {
                        Err(format!(
                            "{} is not a member of {}",
                            member,
                            type_.full_name()
                        ))?
                    };

                    let field_initializers = field_initializers?;

                    match field_initializers {
                        EnumMemberFieldInitializers::None => (),
                        EnumMemberFieldInitializers::Named(ref field_initializers) => {
                            for ((_, field_type), (_, initializer)) in
                                fields.iter().zip(field_initializers.iter())
                            {
                                let initializer_type = initializer.get_type();

                                if !type_equals(field_type, &initializer_type) {
                                    return Err(format!(
                                        "Field type {} does not match initializer type {}",
                                        field_type, initializer_type
                                    ));
                                }
                            }
                        }
                    }

                    Literal::Enum {
                        type_annotation: type_annotation.clone(),
                        member: member.clone(),
                        field_initializers,
                        type_,
                    }
                }
            };

            Ok(TypedExpression::Literal(literal))
        }
        Expression::Closure(closure) => {
            let closure_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
                type_environment.clone(),
            )));

            let param = closure.param.clone();
            let return_type_annotation = closure.return_type_annotation.clone();
            let body = closure.body.clone();

            let param = match param {
                Some(param) => {
                    let type_ = type_environment.borrow().get_type_from_annotation(
                        &param.type_annotation,
                        type_environment.clone(),
                    )?;

                    closure_environment
                        .borrow_mut()
                        .add_variable(param.identifier.clone(), type_.clone());

                    Some(TypedParameter {
                        identifier: param.identifier.clone(),
                        type_annotation: param.type_annotation.clone(),
                        type_: Box::new(type_),
                    })
                }
                None => None,
            };

            let return_type = match return_type_annotation {
                Some(return_type) => {
                    let type_ = type_environment
                        .borrow()
                        .get_type_from_annotation(&return_type, type_environment.clone())?;

                    type_
                }
                None => Type::Void,
            };

            let body = check_type(&body, discovered_types, closure_environment.clone())?;

            let type_ = Type::Function(Function {
                identifier: None,
                param: param.clone().map(|p| super::Parameter {
                    identifier: p.identifier,
                    type_: p.type_,
                }),
                return_type: Box::new(return_type.clone()),
            });

            Ok(TypedExpression::Closure {
                param,
                return_type,
                body: Box::new(body),
                type_,
            })
        }
        Expression::Call(call) => {
            let callee = check_type(&call.callee, discovered_types, type_environment.clone())?;

            let callee_type = callee.get_type();
            if !matches!(&callee_type, &Type::Function(_)) {
                return Err(format!(
                    "Expected function type, found {}",
                    callee.get_type()
                ));
            }

            let type_ = match callee_type.clone() {
                Type::Function(Function { return_type, .. }) => *return_type,
                _ => {
                    return Err(format!(
                        "Expected function type, found {}",
                        callee.get_type()
                    ))
                }
            };

            let arg_typed_expression = call
                .argument
                .clone()
                .map(|arg| check_type(&arg, discovered_types, type_environment.clone()))
                .transpose()?;

            let mut callee = callee;
            let mut type_ = type_;

            if let Some(arg) = arg_typed_expression.clone() {
                if let Type::Function(Function {
                    param: Some(param), ..
                }) = callee.get_type()
                {
                    if !type_equals(&arg.get_type(), &param.type_) {
                        return Err(format!(
                            "Argument type {} does not match parameter type {}",
                            arg.get_type(),
                            param.type_
                        ));
                    }
                } else {
                    if let Type::Function(Function { param: None, .. }) = callee_type {
                        callee = TypedExpression::Call {
                            callee: Box::new(callee),
                            argument: None,
                            type_: type_.clone(),
                        };
                        type_ = match type_ {
                            Type::Function(Function { return_type, .. }) => *return_type,
                            _ => {
                                return Err(format!(
                                    "Expected function type with a return type, found {}",
                                    callee.get_type()
                                ));
                            }
                        };
                    } else {
                        return Err(format!(
                            "Expected function type with a parameter, found {}",
                            callee.get_type()
                        ));
                    }
                };
            }

            let arg = arg_typed_expression.clone().map(|arg| Box::new(arg));

            Ok(TypedExpression::Call {
                callee: Box::new(callee),
                argument: arg,
                type_,
            })
        }
        Expression::Unary(unary) => {
            let expression = check_type(&unary.expression, discovered_types, type_environment)?;
            let type_ = expression.get_deep_type();

            Ok(TypedExpression::Unary {
                operator: match unary.operator {
                    parser::UnaryOperator::Identity => UnaryOperator::Identity,
                    parser::UnaryOperator::Negate => UnaryOperator::Negate,
                    parser::UnaryOperator::LogicalNot => UnaryOperator::LogicalNot,
                    parser::UnaryOperator::BitwiseNot => UnaryOperator::BitwiseNot,
                },
                expression: Box::new(expression),
                type_: type_.clone(),
            })
        }
        Expression::Binary(binary) => {
            let left = check_type(&binary.left, discovered_types, type_environment.clone())?;
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
                parser::BinaryOperator::LogicalAnd => BinaryOperator::LogicalAnd,
                parser::BinaryOperator::LogicalOr => BinaryOperator::LogicalOr,
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
                type_,
            })
        }
        Expression::Block(statements) => {
            let mut statements_: Vec<TypedStatement> = vec![];

            for statement in statements {
                statements_.push(statements::check_type(
                    statement,
                    discovered_types,
                    type_environment.clone(),
                )?);
            }

            let mut type_ = Type::Void;
            for statement in statements_.clone() {
                match statement {
                    TypedStatement::Expression(e) => {
                        type_ = e.get_deep_type();
                    }
                    _ => continue,
                }
            }

            Ok(TypedExpression::Block(Block {
                statements: statements_,
                type_,
            }))
        }
        Expression::Drop(symbol) => {
            let type_ = type_environment
                .borrow()
                .get_variable(symbol)
                .ok_or_else(|| format!("Unexpected variable: {}", symbol))?
                .clone();

            Ok(TypedExpression::Drop {
                identifier: symbol.clone(),
                type_,
            })
        }
        Expression::Loop(block) => {
            let loop_environment = Rc::new(RefCell::new(TypeEnvironment::new_scope(
                type_environment,
                ScopeType::Break,
            )));

            let block = check_type(
                &Expression::Block(block.clone()),
                discovered_types,
                loop_environment.clone(),
            )?;

            let TypedExpression::Block(block) = block else {
                return Err("Loop must have a block".to_string());
            };

            let scope = loop_environment.borrow().get_scope(&ScopeType::Break);
            match scope {
                Some(scope) => {
                    let type_ = scope.fold()?;

                    Ok(TypedExpression::Loop(Block {
                        statements: block.statements,
                        type_,
                    }))
                }
                _ => Ok(TypedExpression::Loop(Block {
                    statements: block.statements,
                    type_: Type::Void,
                })),
            }
        }
        Expression::While(While {
            condition,
            statements: block,
            else_statements: else_block,
        }) => {
            let while_and_else_environment =
                Rc::new(RefCell::new(TypeEnvironment::new_parent(type_environment)));

            let while_environment = Rc::new(RefCell::new(TypeEnvironment::new_scope(
                while_and_else_environment.clone(),
                ScopeType::Break,
            )));

            let condition = check_type(
                condition,
                discovered_types,
                while_and_else_environment.clone(),
            )?;

            let Type::Bool = condition.get_type() else {
                return Err(format!("While condition must be of type bool"));
            };

            let block = check_type(
                &Expression::Block(block.clone()),
                discovered_types,
                while_environment.clone(),
            )?;

            let TypedExpression::Block(block) = block else {
                return Err("While block must be a block".to_string());
            };

            let else_block = match else_block {
                Some(else_block) => Some(check_type(
                    &Expression::Block(else_block.clone()),
                    discovered_types,
                    while_and_else_environment,
                )?),
                None => None,
            };

            let type_ = while_environment
                .borrow()
                .get_scope(&ScopeType::Break)
                .map(|scope| scope.fold())
                .unwrap_or(Ok(Type::Void))?;

            let else_block = match else_block {
                Some(TypedExpression::Block(block)) => {
                    if !type_equals(&type_, &Type::Void) && !type_equals(&type_, &block.type_) {
                        return Err(format!("While block breaks with value of type {} which does not match else blocks type {}", type_, block.type_));
                    }

                    Some(block.statements)
                }
                None => {
                    if !type_equals(&type_, &Type::Void) {
                        return Err(format!(
                            "Must have an else block if the while block breaks with a value"
                        ));
                    }

                    None
                }
                _ => return Err("Else block must be a block".to_string()),
            };

            Ok(TypedExpression::While {
                condition: Box::new(condition),
                block: block.statements,
                else_block,
                type_: type_.clone(),
            })
        }
    }
}

fn synthesize_type(
    expression: &Expression,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rc<RefCell<TypeEnvironment>>,
) -> Result<Option<TypedExpression>, String> {
    match expression {
        Expression::None => Ok(Some(TypedExpression::None)),
        Expression::VariableDeclaration(_) => todo!(),
        Expression::If(_) => todo!(),
        Expression::Assignment(_) => todo!(),
        Expression::Member(_) => todo!(),
        Expression::Literal(l) => match l {
            parser::Literal::Unit => Ok(Some(TypedExpression::Literal(Literal::Unit))),
            parser::Literal::Int(v) => Ok(Some(TypedExpression::Literal(Literal::Int(*v)))),
            parser::Literal::UInt(v) => Ok(Some(TypedExpression::Literal(Literal::UInt(*v)))),
            parser::Literal::Float(v) => Ok(Some(TypedExpression::Literal(Literal::Float(*v)))),
            parser::Literal::String(v) => {
                Ok(Some(TypedExpression::Literal(Literal::String(v.clone()))))
            }
            parser::Literal::Char(v) => Ok(Some(TypedExpression::Literal(Literal::Char(*v)))),
            parser::Literal::Bool(v) => Ok(Some(TypedExpression::Literal(Literal::Bool(*v)))),
            parser::Literal::Array(v) => todo!("Synthesize array type"),
            parser::Literal::Struct { .. } => todo!("Synthesize struct type"),
            parser::Literal::Enum { .. } => todo!("Synthesize enum type"),
        },
        Expression::Closure(_) => todo!(),
        Expression::Call(_) => todo!(),
        Expression::Unary(_) => todo!(),
        Expression::Binary(_) => todo!(),
        Expression::Block(_) => todo!(),
        Expression::Loop(_) => todo!(),
        Expression::While(_) => todo!(),
        Expression::Drop(_) => todo!(),
    }
}

fn is_option(type_: &Option<Type>) -> bool {
    let Some(type_) = type_ else {
        return false;
    };

    let Type::Enum(Enum {
        type_identifier,
        members,
    }) = type_
    else {
        return false;
    };

    let TypeIdentifier::ConcreteType(name, ..) = type_identifier else {
        return false;
    };

    if name != "Option" {
        return false;
    }

    return members.get("Some").map_or(false, |member| {
        let Type::EnumMember(EnumMember { fields, .. }) = member else {
            return false;
        };

        return fields.get("f0").map_or(false, |_| {
            return true;
        });
    });
}

fn check_type_member_access(
    object: &Box<Expression>,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
    member: &Box<parser::Member>,
    function_propagation: bool,
) -> Result<TypedExpression, String> {
    let object_type_expression = check_type(object, discovered_types, type_environment.clone())?;
    let object_type = object_type_expression.get_type();
    check_type_member_access_recurse(
        object_type.clone(),
        member,
        type_environment,
        object_type_expression,
        discovered_types,
        function_propagation,
    )
}

fn check_type_member_access_recurse(
    object_type: Type,
    member: &Box<parser::Member>,
    type_environment: Rcrc<TypeEnvironment>,
    object_typed_expression: TypedExpression,
    _discovered_types: &Vec<DiscoveredType>,
    function_propagation: bool,
) -> Result<TypedExpression, String> {
    match *member.clone() {
        parser::Member::Identifier { symbol } => {
            if !function_propagation {
                return Err(
                    "Currently, only parameter propagation is supported for member access"
                        .to_string(),
                );
            }

            let type_ = type_environment
                .borrow()
                .get_variable(&symbol)
                .or_else(|| type_environment.borrow().get_type_from_str(&symbol))
                .ok_or_else(|| {
                    format!(
                        "Unexpected member access: {} on type {}",
                        symbol,
                        object_type.full_name()
                    )
                })?;

            let Type::Function(Function {
                param, return_type, ..
            }) = type_.clone()
            else {
                return Err(format!("{} is not a function", symbol));
            };

            let Some(param) = param else {
                Err(format!(
                    "Function {} must have at least one parameter",
                    symbol
                ))?
            };

            if !type_equals(&object_type, &param.type_) {
                Err(format!(
                    "Function '{}' must be called on type {}. Found {}",
                    symbol, param.type_, object_type
                ))?
            }

            Ok(TypedExpression::Closure {
                param: None,
                return_type: *return_type.clone(),
                body: Box::new(TypedExpression::Call {
                    callee: Box::new(TypedExpression::Member(Member::Identifier {
                        symbol: symbol.clone(),
                        type_: type_.clone(),
                    })),
                    argument: Some(Box::new(object_typed_expression)),
                    type_: type_.clone(),
                }),
                type_: Type::Function(Function {
                    identifier: None,
                    param: None,
                    return_type,
                }),
            })
        }
        parser::Member::MemberAccess { .. } => todo!("Member access"),
        parser::Member::ParamPropagation { .. } => todo!("Param propagation"),
    }
}

fn get_binop_type(left_type: &Type, operator: &BinaryOperator, right_type: &Type) -> Type {
    match (left_type, operator, right_type) {
        (Type::Int, BinaryOperator::Add, Type::Int) => Type::Int,
        (Type::UInt, BinaryOperator::Add, Type::UInt) => Type::UInt,
        (Type::Float, BinaryOperator::Add, Type::Float) => Type::Float,
        (Type::String, BinaryOperator::Add, Type::String) => Type::String,
        (Type::Char, BinaryOperator::Add, Type::Char) => Type::String,
        (Type::Int, BinaryOperator::Subtract, Type::Int) => Type::Int,
        (Type::UInt, BinaryOperator::Subtract, Type::UInt) => Type::UInt,
        (Type::Float, BinaryOperator::Subtract, Type::Float) => Type::Float,
        (Type::Int, BinaryOperator::Multiply, Type::Int) => Type::Int,
        (Type::UInt, BinaryOperator::Multiply, Type::UInt) => Type::UInt,
        (Type::Float, BinaryOperator::Multiply, Type::Float) => Type::Float,
        (Type::Int, BinaryOperator::Divide, Type::Int) => Type::Int,
        (Type::UInt, BinaryOperator::Divide, Type::UInt) => Type::UInt,
        (Type::Float, BinaryOperator::Divide, Type::Float) => Type::Float,
        (Type::Int, BinaryOperator::Modulo, Type::Int) => Type::Int,
        (Type::UInt, BinaryOperator::Modulo, Type::UInt) => Type::UInt,
        (Type::Float, BinaryOperator::Modulo, Type::Float) => Type::Float,
        (Type::Int, BinaryOperator::BitwiseAnd, Type::Int) => Type::Int,
        (Type::UInt, BinaryOperator::BitwiseAnd, Type::UInt) => Type::UInt,
        (Type::Int, BinaryOperator::BitwiseOr, Type::Int) => Type::Int,
        (Type::UInt, BinaryOperator::BitwiseOr, Type::UInt) => Type::UInt,
        (Type::Int, BinaryOperator::BitwiseXor, Type::Int) => Type::Int,
        (Type::UInt, BinaryOperator::BitwiseXor, Type::UInt) => Type::UInt,
        (Type::Int, BinaryOperator::BitwiseLeftShift, Type::Int) => Type::Int,
        (Type::UInt, BinaryOperator::BitwiseLeftShift, Type::UInt) => Type::UInt,
        (Type::Int, BinaryOperator::BitwiseRightShift, Type::Int) => Type::Int,
        (Type::UInt, BinaryOperator::BitwiseRightShift, Type::UInt) => Type::UInt,
        (Type::Int, BinaryOperator::Equal, Type::Int) => Type::Bool,
        (Type::UInt, BinaryOperator::Equal, Type::UInt) => Type::Bool,
        (Type::Float, BinaryOperator::Equal, Type::Float) => Type::Bool,
        (Type::String, BinaryOperator::Equal, Type::String) => Type::Bool,
        (Type::Char, BinaryOperator::Equal, Type::Char) => Type::Bool,
        (Type::Bool, BinaryOperator::Equal, Type::Bool) => Type::Bool,
        (Type::Unit, BinaryOperator::Equal, Type::Unit) => Type::Bool,
        (Type::Int, BinaryOperator::NotEqual, Type::Int) => Type::Bool,
        (Type::UInt, BinaryOperator::NotEqual, Type::UInt) => Type::Bool,
        (Type::Float, BinaryOperator::NotEqual, Type::Float) => Type::Bool,
        (Type::String, BinaryOperator::NotEqual, Type::String) => Type::Bool,
        (Type::Char, BinaryOperator::NotEqual, Type::Char) => Type::Bool,
        (Type::Bool, BinaryOperator::NotEqual, Type::Bool) => Type::Bool,
        (Type::Unit, BinaryOperator::NotEqual, Type::Unit) => Type::Bool,
        (Type::Int, BinaryOperator::LessThan, Type::Int) => Type::Bool,
        (Type::UInt, BinaryOperator::LessThan, Type::UInt) => Type::Bool,
        (Type::Float, BinaryOperator::LessThan, Type::Float) => Type::Bool,
        (Type::Int, BinaryOperator::LessThanOrEqual, Type::Int) => Type::Bool,
        (Type::UInt, BinaryOperator::LessThanOrEqual, Type::UInt) => Type::Bool,
        (Type::Float, BinaryOperator::LessThanOrEqual, Type::Float) => Type::Bool,
        (Type::Int, BinaryOperator::GreaterThan, Type::Int) => Type::Bool,
        (Type::UInt, BinaryOperator::GreaterThan, Type::UInt) => Type::Bool,
        (Type::Float, BinaryOperator::GreaterThan, Type::Float) => Type::Bool,
        (Type::Int, BinaryOperator::GreaterThanOrEqual, Type::Int) => Type::Bool,
        (Type::UInt, BinaryOperator::GreaterThanOrEqual, Type::UInt) => Type::Bool,
        (Type::Float, BinaryOperator::GreaterThanOrEqual, Type::Float) => Type::Bool,
        (Type::Bool, BinaryOperator::LogicalAnd, Type::Bool) => Type::Bool,
        (Type::Bool, BinaryOperator::LogicalOr, Type::Bool) => Type::Bool,
        (Type::Literal { name: _, type_ }, operator, right_type) => {
            get_binop_type(type_, operator, right_type)
        }
        (left_type, operator, Type::Literal { name: _, type_ }) => {
            get_binop_type(left_type, operator, type_)
        }
        _ => panic!(
            "Unexpected binary operator {:?} for types {:?} and {:?}",
            operator, left_type, right_type
        ),
    }
}
