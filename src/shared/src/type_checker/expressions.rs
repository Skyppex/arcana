use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    parser::{self, Assignment, Binary, Expression, If, VariableDeclaration, While},
    type_checker::ast::Literal,
    types::TypeIdentifier,
};

use super::{
    ast::{
        BinaryOperator, Block, EnumMemberFieldInitializers, FieldInitializer, Member, Typed,
        TypedClosureParameter, TypedExpression, TypedStatement, UnaryOperator,
    },
    scope::ScopeType,
    statements, type_equals, DiscoveredType, Enum, EnumMember, FullName, Function, Rcrc, Struct,
    Type, TypeEnvironment,
};

pub fn check_type<'a>(
    expression: &Expression,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rc<RefCell<TypeEnvironment>>,
    context: Option<Type>,
) -> Result<TypedExpression, String> {
    match expression {
        // Expression::None => Ok(TypedExpression::None),
        Expression::Closure(closure) => {
            let closure_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
                type_environment.clone(),
            )));

            let param = closure.param.clone();
            let return_type_annotation = closure.return_type_annotation.clone();
            let body = closure.body.clone();

            let param = match param {
                Some(param) => {
                    let type_ = &param
                        .type_annotation
                        .clone()
                        .map(|ta| type_environment.borrow().get_type_from_annotation(&ta))
                        .transpose()?;

                    let type_ = type_.clone().or_else(|| {
                        if let Some(Type::Function(Function { param: Some(t), .. })) =
                            context.clone()
                        {
                            return Some(*t.type_);
                        } else {
                            return None;
                        };
                    });

                    let Some(type_) = type_ else {
                        return Err("Could not infer type of closure parameter".to_string());
                    };

                    closure_environment
                        .borrow_mut()
                        .add_variable(param.identifier.clone(), type_.clone());

                    Some(TypedClosureParameter {
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
                        .get_type_from_annotation(&return_type)?;

                    type_
                }
                None => {
                    if let Some(Type::Function(Function { return_type, .. })) = context {
                        *return_type
                    } else {
                        Type::Void
                    }
                }
            };

            let body = check_type(&body, discovered_types, closure_environment.clone(), None)?;

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
            let callee = check_type(
                &call.callee,
                discovered_types,
                type_environment.clone(),
                None,
            )?;

            let callee_type = callee.get_type();
            if !matches!(&callee_type, &Type::Function(_)) {
                return Err(format!(
                    "Expected function type, found {}",
                    callee.get_type()
                ));
            }

            let return_type = match callee_type.clone() {
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
                .map(|arg| {
                    let context_type =
                        if let Type::Function(Function { param, .. }) = callee_type.clone() {
                            param.map(|p| *p.type_)
                        } else {
                            None
                        };

                    check_type(
                        &arg,
                        discovered_types,
                        type_environment.clone(),
                        context_type,
                    )
                })
                .transpose()?;

            let mut callee = callee;
            let mut return_type = return_type;

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
                            type_: return_type.clone(),
                        };
                        return_type = match return_type {
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
                type_: return_type,
            })
        }
        other => synthesize_type(other, discovered_types, type_environment),
    }
}

fn synthesize_type(
    expression: &Expression,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rc<RefCell<TypeEnvironment>>,
) -> Result<TypedExpression, String> {
    match expression {
        // Expression::None => Ok(TypedExpression::None),
        Expression::VariableDeclaration(VariableDeclaration {
            mutable,
            type_annotation,
            identifier,
            initializer,
        }) => {
            let mut type_ = Type::Unknown;

            let initializer = match (&initializer, type_annotation) {
                (Some(initializer), Some(type_annotation)) => {
                    let context_type = type_environment
                        .borrow()
                        .get_type_from_annotation(type_annotation)?;

                    let initializer = check_type(
                        initializer,
                        discovered_types,
                        type_environment.clone(),
                        Some(context_type),
                    )?;

                    type_ = type_environment
                        .borrow()
                        .get_type_from_annotation(type_annotation)?;

                    if !type_equals(&initializer.get_type(), &type_) {
                        return Err(format!(
                            "Initializer type {} does not match variable type",
                            initializer.get_type(),
                        ));
                    }

                    Some(initializer)
                }
                (Some(initializer), None) => {
                    let initializer = check_type(
                        initializer,
                        discovered_types,
                        type_environment.clone(),
                        None,
                    )?;

                    type_ = initializer.get_type();
                    Some(initializer)
                }
                (None, Some(type_annotation)) => {
                    type_ = type_environment
                        .borrow()
                        .get_type_from_annotation(type_annotation)?;

                    None
                }
                _ => None,
            };

            type_environment
                .borrow_mut()
                .add_variable(identifier.clone(), type_.clone());

            Ok(TypedExpression::VariableDeclaration {
                mutable: *mutable,
                identifier: identifier.clone(),
                initializer: initializer.map(|i| Box::new(i)),
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

            let if_condition = check_type(
                &condition,
                discovered_types,
                if_else_environment.clone(),
                None,
            )?;

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
                None,
            )?;
            let if_block_type = if_block.get_deep_type();

            let else_block = if let Some(false_expression) = false_expression {
                Some(check_type(
                    false_expression,
                    discovered_types,
                    if_else_environment,
                    None,
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
            let identifier = member.get_symbol();

            let mut member = check_type(
                &Expression::Member(*member.clone()),
                discovered_types,
                type_environment.clone(),
                None,
            )?;

            let initializer = check_type(
                initializer,
                discovered_types,
                type_environment.clone(),
                None,
            )?;

            let mut member_type = member.get_type();

            if member_type == Type::Unknown {
                member_type = initializer.get_type();

                type_environment
                    .borrow_mut()
                    .add_variable(identifier.clone(), member_type.clone());

                let TypedExpression::Member(mem) = member else {
                    return Err("Expected member expression".to_string());
                };

                let mem = match mem {
                    Member::Identifier { symbol, .. } => Member::Identifier {
                        symbol,
                        type_: member_type.clone(),
                    },
                    ma => ma,
                };

                member = TypedExpression::Member(mem);
            }

            if !type_equals(&member_type, &initializer.get_type()) {
                return Err(format!(
                    "Member type {} does not match initializer type {}",
                    member.get_type(),
                    initializer.get_type()
                ));
            }

            let TypedExpression::Member(member) = member else {
                unreachable!("Member should always be a member expression here");
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
        Expression::Literal(l) => match l {
            parser::Literal::Unit => Ok(TypedExpression::Literal(Literal::Unit)),
            parser::Literal::Int(v) => Ok(TypedExpression::Literal(Literal::Int(*v))),
            parser::Literal::UInt(v) => Ok(TypedExpression::Literal(Literal::UInt(*v))),
            parser::Literal::Float(v) => Ok(TypedExpression::Literal(Literal::Float(*v))),
            parser::Literal::String(v) => Ok(TypedExpression::Literal(Literal::String(v.clone()))),
            parser::Literal::Char(v) => Ok(TypedExpression::Literal(Literal::Char(*v))),
            parser::Literal::Bool(v) => Ok(TypedExpression::Literal(Literal::Bool(*v))),
            parser::Literal::Array(v) => {
                let v: Result<(Vec<TypedExpression>, Type), String> = {
                    let mut v_: Vec<TypedExpression> = vec![];
                    let mut previous_type = Type::Void;

                    for e in v {
                        let value =
                            check_type(&e, discovered_types, type_environment.clone(), None)?;
                        let type_ = value.get_deep_type();

                        if !type_equals(&previous_type, &Type::Void)
                            && !type_equals(&type_, &previous_type)
                        {
                            return Err(format!(
                                "Array element type {:?} does not match previous element type {:?}",
                                type_, previous_type
                            ));
                        }

                        previous_type = type_.clone();
                        v_.push(value);
                    }

                    Ok((v_, previous_type.clone()))
                };

                let v = v?;
                Ok(TypedExpression::Literal(Literal::Array {
                    values: v.0,
                    type_: v.1,
                }))
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
                                None,
                            )?,
                        };
                        field_initializers_.push(field_initializer);
                    }
                    Ok(field_initializers_)
                };

                let type_ = type_environment
                    .borrow()
                    .get_type_from_annotation(type_annotation)?;

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

                Ok(TypedExpression::Literal(Literal::Struct {
                    type_annotation: type_annotation.clone(),
                    field_initializers,
                    type_,
                }))
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
                                        None,
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
                    .get_type_from_annotation(type_annotation)?;

                let Type::Enum(Enum { members, .. }) = type_.clone() else {
                    Err(format!("{} is not an enum", type_.full_name()))?
                };

                let Some(Type::EnumMember(EnumMember { fields, .. })) = members.get(member) else {
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

                Ok(TypedExpression::Literal(Literal::Enum {
                    type_annotation: type_annotation.clone(),
                    member: member.clone(),
                    field_initializers,
                    type_,
                }))
            }
        },
        Expression::Closure(_) => todo!(),
        Expression::Unary(unary) => {
            let expression =
                check_type(&unary.expression, discovered_types, type_environment, None)?;
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
        Expression::Binary(Binary {
            left,
            operator,
            right,
        }) => {
            let left = check_type(left, discovered_types, type_environment.clone(), None)?;
            let right = check_type(right, discovered_types, type_environment, None)?;

            let operator: BinaryOperator = operator.clone().into();
            let type_ = get_binop_type(&left.get_type(), &operator, &right.get_type())?;

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
                None,
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
                None,
            )?;

            let Type::Bool = condition.get_type() else {
                return Err(format!("While condition must be of type bool"));
            };

            let block = check_type(
                &Expression::Block(block.clone()),
                discovered_types,
                while_environment.clone(),
                None,
            )?;

            let TypedExpression::Block(block) = block else {
                return Err("While block must be a block".to_string());
            };

            let else_block = match else_block {
                Some(else_block) => Some(check_type(
                    &Expression::Block(else_block.clone()),
                    discovered_types,
                    while_and_else_environment,
                    None,
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
        _ => Err(format!("Unexpected expression: {:?}", expression)),
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
    let object_type_expression =
        check_type(object, discovered_types, type_environment.clone(), None)?;
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

fn get_binop_type(
    left_type: &Type,
    operator: &BinaryOperator,
    right_type: &Type,
) -> Result<Type, String> {
    match (left_type, operator, right_type) {
        (Type::Int, BinaryOperator::Add, Type::Int) => Ok(Type::Int),
        (Type::UInt, BinaryOperator::Add, Type::UInt) => Ok(Type::UInt),
        (Type::Float, BinaryOperator::Add, Type::Float) => Ok(Type::Float),
        (Type::String, BinaryOperator::Add, Type::String) => Ok(Type::String),
        (Type::Char, BinaryOperator::Add, Type::Char) => Ok(Type::String),
        (Type::Int, BinaryOperator::Subtract, Type::Int) => Ok(Type::Int),
        (Type::UInt, BinaryOperator::Subtract, Type::UInt) => Ok(Type::UInt),
        (Type::Float, BinaryOperator::Subtract, Type::Float) => Ok(Type::Float),
        (Type::Int, BinaryOperator::Multiply, Type::Int) => Ok(Type::Int),
        (Type::UInt, BinaryOperator::Multiply, Type::UInt) => Ok(Type::UInt),
        (Type::Float, BinaryOperator::Multiply, Type::Float) => Ok(Type::Float),
        (Type::Int, BinaryOperator::Divide, Type::Int) => Ok(Type::Int),
        (Type::UInt, BinaryOperator::Divide, Type::UInt) => Ok(Type::UInt),
        (Type::Float, BinaryOperator::Divide, Type::Float) => Ok(Type::Float),
        (Type::Int, BinaryOperator::Modulo, Type::Int) => Ok(Type::Int),
        (Type::UInt, BinaryOperator::Modulo, Type::UInt) => Ok(Type::UInt),
        (Type::Float, BinaryOperator::Modulo, Type::Float) => Ok(Type::Float),
        (Type::Int, BinaryOperator::BitwiseAnd, Type::Int) => Ok(Type::Int),
        (Type::UInt, BinaryOperator::BitwiseAnd, Type::UInt) => Ok(Type::UInt),
        (Type::Int, BinaryOperator::BitwiseOr, Type::Int) => Ok(Type::Int),
        (Type::UInt, BinaryOperator::BitwiseOr, Type::UInt) => Ok(Type::UInt),
        (Type::Int, BinaryOperator::BitwiseXor, Type::Int) => Ok(Type::Int),
        (Type::UInt, BinaryOperator::BitwiseXor, Type::UInt) => Ok(Type::UInt),
        (Type::Int, BinaryOperator::BitwiseLeftShift, Type::Int) => Ok(Type::Int),
        (Type::UInt, BinaryOperator::BitwiseLeftShift, Type::UInt) => Ok(Type::UInt),
        (Type::Int, BinaryOperator::BitwiseRightShift, Type::Int) => Ok(Type::Int),
        (Type::UInt, BinaryOperator::BitwiseRightShift, Type::UInt) => Ok(Type::UInt),
        (Type::Int, BinaryOperator::Equal, Type::Int) => Ok(Type::Bool),
        (Type::UInt, BinaryOperator::Equal, Type::UInt) => Ok(Type::Bool),
        (Type::Float, BinaryOperator::Equal, Type::Float) => Ok(Type::Bool),
        (Type::String, BinaryOperator::Equal, Type::String) => Ok(Type::Bool),
        (Type::Char, BinaryOperator::Equal, Type::Char) => Ok(Type::Bool),
        (Type::Bool, BinaryOperator::Equal, Type::Bool) => Ok(Type::Bool),
        (Type::Unit, BinaryOperator::Equal, Type::Unit) => Ok(Type::Bool),
        (Type::Int, BinaryOperator::NotEqual, Type::Int) => Ok(Type::Bool),
        (Type::UInt, BinaryOperator::NotEqual, Type::UInt) => Ok(Type::Bool),
        (Type::Float, BinaryOperator::NotEqual, Type::Float) => Ok(Type::Bool),
        (Type::String, BinaryOperator::NotEqual, Type::String) => Ok(Type::Bool),
        (Type::Char, BinaryOperator::NotEqual, Type::Char) => Ok(Type::Bool),
        (Type::Bool, BinaryOperator::NotEqual, Type::Bool) => Ok(Type::Bool),
        (Type::Unit, BinaryOperator::NotEqual, Type::Unit) => Ok(Type::Bool),
        (Type::Int, BinaryOperator::LessThan, Type::Int) => Ok(Type::Bool),
        (Type::UInt, BinaryOperator::LessThan, Type::UInt) => Ok(Type::Bool),
        (Type::Float, BinaryOperator::LessThan, Type::Float) => Ok(Type::Bool),
        (Type::Int, BinaryOperator::LessThanOrEqual, Type::Int) => Ok(Type::Bool),
        (Type::UInt, BinaryOperator::LessThanOrEqual, Type::UInt) => Ok(Type::Bool),
        (Type::Float, BinaryOperator::LessThanOrEqual, Type::Float) => Ok(Type::Bool),
        (Type::Int, BinaryOperator::GreaterThan, Type::Int) => Ok(Type::Bool),
        (Type::UInt, BinaryOperator::GreaterThan, Type::UInt) => Ok(Type::Bool),
        (Type::Float, BinaryOperator::GreaterThan, Type::Float) => Ok(Type::Bool),
        (Type::Int, BinaryOperator::GreaterThanOrEqual, Type::Int) => Ok(Type::Bool),
        (Type::UInt, BinaryOperator::GreaterThanOrEqual, Type::UInt) => Ok(Type::Bool),
        (Type::Float, BinaryOperator::GreaterThanOrEqual, Type::Float) => Ok(Type::Bool),
        (Type::Bool, BinaryOperator::LogicalAnd, Type::Bool) => Ok(Type::Bool),
        (Type::Bool, BinaryOperator::LogicalOr, Type::Bool) => Ok(Type::Bool),
        (Type::Literal { type_, .. }, operator, right_type) => {
            get_binop_type(type_, operator, right_type)
        }
        (left_type, operator, Type::Literal { type_, .. }) => {
            get_binop_type(left_type, operator, type_)
        }
        _ => Err(format!(
            "Unexpected binary operator {:?} for types {:?} and {:?}",
            operator, left_type, right_type,
        )),
    }
}
