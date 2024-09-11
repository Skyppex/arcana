use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    parser::{self, Assignment, Binary, Expression, For, If, Match, VariableDeclaration, While},
    type_checker::ast::Literal,
    types::{TypeAnnotation, TypeIdentifier},
};

use super::{
    ast::{
        BinaryOperator, Block, EnumMemberFieldInitializers, FieldInitializer, Member, Typed,
        TypedClosureParameter, TypedExpression, TypedMatchArm, TypedStatement, UnaryOperator,
    },
    decision_tree::create_decision_tree,
    scope::ScopeType,
    statements::{self, check_type_annotation},
    type_equals, type_equals_coerce, DiscoveredType, Enum, EnumMember, FullName, Function, Rcrc,
    Struct, Type, TypeAlias, TypeEnvironment, Union,
};

pub fn check_type<'a>(
    expression: &Expression,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rc<RefCell<TypeEnvironment>>,
    context: Option<Type>,
) -> Result<TypedExpression, String> {
    match expression {
        // Expression::None => Ok(TypedExpression::None),
        Expression::Break(e) => match e {
            Some(e) => {
                let typed_expression =
                    check_type(e, discovered_types, type_environment.clone(), None)?;

                let break_type = typed_expression.get_type();
                type_environment
                    .borrow_mut()
                    .activate_scope(ScopeType::Break, break_type)?;
                Ok(TypedExpression::Break(Some(Box::new(typed_expression))))
            }
            None => {
                type_environment
                    .borrow_mut()
                    .activate_scope(ScopeType::Break, Type::Void)?;
                Ok(TypedExpression::Break(None))
            }
        },
        Expression::Continue => Ok(TypedExpression::Continue),
        Expression::Return(e) => match e {
            Some(e) => {
                let typed_expression =
                    check_type(e, discovered_types, type_environment.clone(), None)?;

                let return_type = typed_expression.get_type();
                type_environment
                    .borrow_mut()
                    .activate_scope(ScopeType::Return, return_type)?;
                Ok(TypedExpression::Return(Some(Box::new(typed_expression))))
            }
            None => {
                type_environment
                    .borrow_mut()
                    .activate_scope(ScopeType::Return, Type::Void)?;
                Ok(TypedExpression::Return(None))
            }
        },
        Expression::Closure(closure) => {
            println!("CLOSURE__CONTEXT: {:?}", context);
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
                        type_annotation: param
                            .type_annotation
                            .clone()
                            .or_else(|| Some(type_.clone().into())),
                        type_: Box::new(type_),
                    })
                }
                None => None,
            };

            let new_context = match context.clone() {
                Some(Type::Function(Function { return_type, .. })) => Some(*return_type),
                _ => None,
            };

            let body = check_type(
                &body,
                discovered_types,
                closure_environment.clone(),
                new_context.clone(),
            )?;

            let return_type = match return_type_annotation.clone() {
                Some(rta) => type_environment.borrow().get_type_from_annotation(&rta)?,
                None => {
                    if let Some(Type::Function(Function { return_type, .. })) = context.clone() {
                        *return_type
                    } else if let Some(return_type) = new_context {
                        return_type
                    } else {
                        body.get_type()
                    }
                }
            };

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
                context.clone(),
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
                    if !type_equals(&param.type_, &arg.get_type()) {
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

                    if !type_equals(&type_, &initializer.get_type()) {
                        return Err(format!(
                            "Initializer type {} does not match variable type {}",
                            initializer.get_type(),
                            type_
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

            if *mutable {
                if let Type::Literal {
                    type_: literal_type,
                    ..
                } = type_
                {
                    type_ = *literal_type;
                }
            }

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

            if !type_equals(&Type::Bool, &if_condition.get_type()) {
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
        Expression::Match(Match { expression, arms }) => {
            let match_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
                type_environment.clone(),
            )));

            let expression = check_type(
                expression,
                discovered_types,
                match_environment.clone(),
                None,
            )?;

            let mut typed_arms: Vec<TypedMatchArm> = vec![];

            for arm in arms {
                let arm_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
                    match_environment.clone(),
                )));

                let pattern = arm.pattern.clone();
                let expression = arm.expression.clone();

                typed_arms.push(TypedMatchArm {
                    pattern: pattern.into(),
                    expression: *expression,
                    type_environment: arm_environment.clone(),
                });
            }

            let decision_tree = create_decision_tree(
                expression.clone(),
                typed_arms.clone(),
                discovered_types,
                None,
            )?;

            let type_ = decision_tree.get_type();

            Ok(TypedExpression::Match {
                expression: Box::new(expression),
                arms: typed_arms,
                decision_tree,
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
                member_type = initializer.get_deep_type();

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
            crate::parser::Member::Identifier { symbol, .. } => {
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
            crate::parser::Member::StaticMemberAccess {
                type_annotation,
                member,
                ..
            } => check_type_static_member_access(
                type_annotation,
                discovered_types,
                type_environment,
                member,
                context,
            ),
            crate::parser::Member::MemberAccess { object, member, .. } => check_type_member_access(
                object,
                discovered_types,
                type_environment,
                member,
                context,
            ),
            crate::parser::Member::ParamPropagation { object, member, .. } => {
                println!("KASHDLKJASHD: {:?}", context);

                check_type_param_propagation(
                    object,
                    member,
                    discovered_types,
                    type_environment,
                    context,
                )
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
            parser::Literal::Array(values) => {
                let v: Result<(Vec<TypedExpression>, Type), String> = {
                    let mut v_: Vec<TypedExpression> = vec![];
                    let mut previous_type = Type::Void;

                    for value in values {
                        let value =
                            check_type(&value, discovered_types, type_environment.clone(), None)?;

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
                let mut target_type = v.1.clone();

                if type_equals(&v.1, &Type::Void) {
                    if let Some(Type::Array(inner)) = context {
                        target_type = *inner;
                    }
                }

                Ok(TypedExpression::Literal(Literal::Array {
                    values: v.0,
                    type_: target_type,
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

                let Type::EnumMember(EnumMember { fields, .. }) = &type_ else {
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
                        for ((field_name, field_type), (initializer_field_name, initializer)) in
                            fields.iter().zip(field_initializers.iter())
                        {
                            if field_name != initializer_field_name {
                                return Err(format!(
                                    "Field '{}' does not match initializer field '{}'",
                                    field_name, initializer_field_name
                                ));
                            }

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
        Expression::Unary(unary) => {
            let expression =
                check_type(&unary.expression, discovered_types, type_environment, None)?;
            let type_ = expression.get_deep_type();

            let operator = match unary.operator {
                parser::UnaryOperator::Identity => UnaryOperator::Identity,
                parser::UnaryOperator::Negate => UnaryOperator::Negate,
                parser::UnaryOperator::LogicalNot => UnaryOperator::LogicalNot,
                parser::UnaryOperator::BitwiseNot => UnaryOperator::BitwiseNot,
            };

            let type_ = get_unop_type(&operator, &type_)?;

            Ok(TypedExpression::Unary {
                operator,
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

            if matches!(
                operator,
                BinaryOperator::Range | BinaryOperator::RangeInclusive
            ) {
                if !type_equals_coerce(&right.get_type(), &left.get_type()) {
                    return Err(format!(
                        "Range operator requires both sides to be of the same type, found {} and {}",
                        left.get_type(),
                        right.get_type()
                    ));
                }
            }

            Ok(TypedExpression::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
                type_,
            })
        }
        Expression::Block(statements) => {
            let mut typed_statements: Vec<TypedStatement> = vec![];

            for statement in statements {
                typed_statements.push(statements::check_type(
                    statement,
                    discovered_types,
                    type_environment.clone(),
                )?);
            }

            let mut type_ = Type::Void;
            for statement in typed_statements.clone() {
                match statement {
                    TypedStatement::Expression(e) => {
                        type_ = e.get_deep_type();
                    }
                    _ => continue,
                }
            }

            Ok(TypedExpression::Block(Block {
                statements: typed_statements,
                type_,
            }))
        }
        Expression::Print(e) => Ok(TypedExpression::Print {
            value: Box::new(check_type(e, discovered_types, type_environment, None)?),
        }),
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
        Expression::Loop(body) => {
            let loop_environment = Rc::new(RefCell::new(TypeEnvironment::new_scope(
                type_environment,
                ScopeType::Break,
            )));

            let body = check_type(body, discovered_types, loop_environment.clone(), None)?;

            let scope = loop_environment.borrow().get_scope(&ScopeType::Break);
            match scope {
                Some(scope) => {
                    let type_ = scope.fold()?;

                    Ok(TypedExpression::Loop {
                        body: Box::new(body),
                        type_,
                    })
                }
                _ => Ok(TypedExpression::Loop {
                    body: Box::new(body),
                    type_: Type::Void,
                }),
            }
        }
        Expression::While(While {
            condition,
            body,
            else_body,
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

            if !type_equals(&Type::Bool, &condition.get_type()) {
                return Err(format!("While condition must be of type bool"));
            };

            let body = check_type(body, discovered_types, while_environment.clone(), None)?;

            let else_body = match else_body {
                Some(else_block) => Some(check_type(
                    else_block,
                    discovered_types,
                    while_and_else_environment,
                    None,
                )?),
                None => None,
            };

            let mut type_ = while_environment
                .borrow()
                .get_scope(&ScopeType::Break)
                .map(|scope| scope.fold())
                .unwrap_or(Ok(Type::Void))?;

            match &else_body {
                Some(else_body) => {
                    let else_type = else_body.get_type();

                    if !type_equals(&type_, &Type::Void) && !type_equals(&type_, &else_type) {
                        return Err(format!("While block breaks with value of type {} which does not match else blocks type {}", type_, else_body.get_type()));
                    }

                    type_ = else_type
                }
                None => {
                    if !type_equals(&type_, &Type::Void) {
                        return Err(format!(
                            "Must have an else block if the while block breaks with a value"
                        ));
                    }
                }
            };

            Ok(TypedExpression::While {
                condition: Box::new(condition),
                body: Box::new(body),
                else_body: else_body.map(Box::new),
                type_: type_.clone(),
            })
        }
        Expression::For(For {
            identifier,
            iterable,
            body,
            else_body,
        }) => {
            let for_and_else_environment =
                Rc::new(RefCell::new(TypeEnvironment::new_parent(type_environment)));

            let for_environment = Rc::new(RefCell::new(TypeEnvironment::new_scope(
                for_and_else_environment.clone(),
                ScopeType::Break,
            )));

            let iterable = check_type(
                iterable,
                discovered_types,
                for_and_else_environment.clone(),
                None,
            )?;

            let Type::Array(inner_type) = iterable.get_type() else {
                return Err(format!(
                    "For iterable must be of type array, found {}",
                    iterable.get_type()
                ));
            };

            for_environment
                .borrow_mut()
                .add_variable(identifier.clone(), *inner_type);

            let body = check_type(body, discovered_types, for_environment.clone(), None)?;

            let else_body = match else_body {
                Some(else_body) => Some(check_type(
                    else_body,
                    discovered_types,
                    for_and_else_environment,
                    None,
                )?),
                None => None,
            };

            let mut type_ = for_environment
                .borrow()
                .get_scope(&ScopeType::Break)
                .map(|scope| scope.fold())
                .unwrap_or(Ok(Type::Void))?;

            match &else_body {
                Some(else_body) => {
                    let else_type = else_body.get_type();

                    if !type_equals(&type_, &Type::Void)
                        && !type_equals(&type_, &else_body.get_type())
                    {
                        return Err(format!("For block breaks with value of type {} which does not match else blocks type {}", type_, else_body.get_type()));
                    }

                    type_ = else_type
                }
                None => {
                    if !type_equals(&type_, &Type::Void) {
                        return Err(format!(
                            "Must have an else block if the for block breaks with a value"
                        ));
                    }
                }
            };

            Ok(TypedExpression::For {
                identifier: identifier.clone(),
                iterable: Box::new(iterable),
                body: Box::new(body),
                else_body: else_body.map(Box::new),
                type_: type_.clone(),
            })
        }
    }
}

fn is_option(type_: &Option<Type>) -> bool {
    let Some(type_) = type_ else {
        return false;
    };

    let Type::Enum(Enum {
        type_identifier,
        shared_fields,
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

    if shared_fields.len() != 0 {
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

fn check_type_static_member_access(
    type_annotation: &TypeAnnotation,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
    member: &Box<parser::Member>,
    context: Option<Type>,
) -> Result<TypedExpression, String> {
    let object_type =
        check_type_annotation(type_annotation, discovered_types, type_environment.clone())?;

    return match *member.clone() {
        parser::Member::Identifier { symbol, .. } => match object_type {
            Type::Struct(struct_) => {
                let Some(static_member_type) = type_environment
                    .borrow()
                    .get_static_member(struct_.type_annotation(), &symbol)
                else {
                    return Err(format!(
                        "Struct '{}' does not have a static member called '{}'",
                        struct_.type_identifier, symbol
                    ));
                };

                let identifier_type = static_member_type.clone();

                Ok(TypedExpression::Member(Member::StaticMemberAccess {
                    type_annotation: type_annotation.clone(),
                    member: Box::new(Member::Identifier {
                        symbol: symbol.clone(),
                        type_: identifier_type.clone(),
                    }),
                    symbol: symbol.clone(),
                    type_: static_member_type.clone(),
                }))
            }
            Type::EnumMember(enum_member) => {
                let Some(static_member_type) = type_environment
                    .borrow()
                    .get_static_member(enum_member.type_annotation(), &symbol)
                else {
                    return Err(format!(
                        "EnumMember '{}' does not have a static member called '{}'",
                        enum_member.enum_name, symbol
                    ));
                };

                if !type_environment.borrow().lookup_type(&static_member_type) {
                    return Err(format!(
                        "Unexpected type: {}",
                        static_member_type.full_name()
                    ));
                }

                let identifier_type = static_member_type.clone();

                Ok(TypedExpression::Member(Member::StaticMemberAccess {
                    type_annotation: type_annotation.clone(),
                    member: Box::new(Member::Identifier {
                        symbol: symbol.clone(),
                        type_: identifier_type.clone(),
                    }),
                    symbol: symbol.clone(),
                    type_: static_member_type.clone(),
                }))
            }
            Type::Enum(enum_) => {
                let Some(static_member_type) = type_environment
                    .borrow()
                    .get_static_member(enum_.type_annotation(), &symbol)
                else {
                    return Err(format!(
                        "EnumMember '{}' does not have a static member called '{}'",
                        enum_.type_identifier, symbol
                    ));
                };

                if !type_environment.borrow().lookup_type(&static_member_type) {
                    return Err(format!(
                        "Unexpected type: {}",
                        static_member_type.full_name()
                    ));
                }

                let identifier_type = static_member_type.clone();

                Ok(TypedExpression::Member(Member::StaticMemberAccess {
                    type_annotation: type_annotation.clone(),
                    member: Box::new(Member::Identifier {
                        symbol: symbol.clone(),
                        type_: identifier_type.clone(),
                    }),
                    symbol: symbol.clone(),
                    type_: static_member_type.clone(),
                }))
            }
            _ => Err(format!(
                "Unexpected member access: {} on type {}",
                symbol,
                object_type.full_name()
            )),
        },
        parser::Member::StaticMemberAccess {
            type_annotation,
            member,
            ..
        } => check_type_static_member_access(
            &type_annotation,
            discovered_types,
            type_environment,
            &member,
            context,
        ),
        parser::Member::MemberAccess { object, member, .. } => check_type_member_access(
            &object,
            discovered_types,
            type_environment,
            &member,
            context,
        ),
        parser::Member::ParamPropagation { object, member, .. } => check_type_param_propagation(
            &object,
            &member,
            discovered_types,
            type_environment,
            context,
        ),
    };
}

fn check_type_member_access(
    object: &Box<Expression>,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
    member: &Box<parser::Member>,
    context: Option<Type>,
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
        context,
    )
}

fn check_type_member_access_recurse(
    object_type: Type,
    member: &Box<parser::Member>,
    type_environment: Rcrc<TypeEnvironment>,
    object_typed_expression: TypedExpression,
    discovered_types: &Vec<DiscoveredType>,
    context: Option<Type>,
) -> Result<TypedExpression, String> {
    return match *member.clone() {
        parser::Member::Identifier { symbol, .. } => match object_type {
            Type::Struct(struct_) => {
                let field_type = struct_.fields.get(&symbol).ok_or(format!(
                    "Struct '{}' does not have a field called '{}'",
                    struct_.type_identifier, symbol
                ))?;

                if !type_environment.borrow().lookup_type(&field_type) {
                    return Err(format!("Unexpected type: {}", field_type.full_name()));
                }

                let identifier_type = field_type.clone();

                Ok(TypedExpression::Member(Member::MemberAccess {
                    object: Box::new(object_typed_expression),
                    member: Box::new(Member::Identifier {
                        symbol: symbol.clone(),
                        type_: identifier_type.clone(),
                    }),
                    symbol: symbol.clone(),
                    type_: field_type.clone(),
                }))
            }
            Type::EnumMember(EnumMember {
                enum_name, fields, ..
            }) => {
                let field_type = fields.get(&symbol).ok_or(format!(
                    "EnumMember '{}' does not have a field called '{}'",
                    enum_name, symbol
                ))?;

                if !type_environment.borrow().lookup_type(&field_type) {
                    return Err(format!("Unexpected type: {}", field_type.full_name()));
                }

                let identifier_type = field_type.clone();

                Ok(TypedExpression::Member(Member::MemberAccess {
                    object: Box::new(object_typed_expression),
                    member: Box::new(Member::Identifier {
                        symbol: symbol.clone(),
                        type_: identifier_type.clone(),
                    }),
                    symbol: symbol.clone(),
                    type_: field_type.clone(),
                }))
            }
            Type::Enum(Enum {
                type_identifier,
                shared_fields,
                ..
            }) => {
                let field_type = shared_fields.get(&symbol).ok_or(format!(
                    "Enum '{}' does not have a shared field called '{}'",
                    type_identifier, symbol
                ))?;

                if !type_environment.borrow().lookup_type(&field_type) {
                    return Err(format!("Unexpected type: {}", field_type.full_name()));
                }

                let identifier_type = field_type.clone();

                Ok(TypedExpression::Member(Member::MemberAccess {
                    object: Box::new(object_typed_expression),
                    member: Box::new(Member::Identifier {
                        symbol: symbol.clone(),
                        type_: identifier_type.clone(),
                    }),
                    symbol: symbol.clone(),
                    type_: field_type.clone(),
                }))
            }
            _ => Err(format!(
                "Unexpected member access: {} on type {}",
                symbol,
                object_type.full_name()
            )),
        },
        parser::Member::StaticMemberAccess {
            type_annotation,
            member,
            ..
        } => check_type_static_member_access(
            &type_annotation,
            discovered_types,
            type_environment,
            &member,
            context,
        ),
        parser::Member::MemberAccess { object, member, .. } => check_type_member_access(
            &object,
            discovered_types,
            type_environment,
            &member,
            context,
        ),
        parser::Member::ParamPropagation { object, member, .. } => check_type_param_propagation(
            &object,
            &member,
            discovered_types,
            type_environment,
            context,
        ),
    };
}

fn check_type_param_propagation(
    object: &Box<Expression>,
    member: &Box<parser::Member>,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
    context: Option<Type>,
) -> Result<TypedExpression, String> {
    let parser::Member::Identifier { symbol, .. } = *member.clone() else {
        return Err("Param propagation must be followed by a member access".to_string());
    };

    let object_type_expression =
        check_type(object, discovered_types, type_environment.clone(), context)?;

    let object_type = object_type_expression.get_type();

    let member_type = type_environment
        .borrow()
        .get_type_from_str(&symbol)
        .or_else(|| {
            type_environment
                .borrow()
                .get_static_member(object_type.type_annotation(), &symbol)
        });

    let Some(Type::Function(Function { param, .. })) = member_type else {
        return Err(format!("{} is not a function", symbol));
    };

    let Some(param) = param else {
        Err(format!(
            "Function {} must have at least one parameter",
            symbol
        ))?
    };

    if !type_equals(&param.type_, &object_type) {
        Err(format!(
            "Function '{}' must be called on type {}. Found {}",
            symbol, param.type_, object_type
        ))?
    }

    check_type_param_propagation_recurse(
        object_type.clone(),
        member,
        type_environment,
        object_type_expression,
    )
}

fn check_type_param_propagation_recurse(
    object_type: Type,
    member: &Box<parser::Member>,
    type_environment: Rcrc<TypeEnvironment>,
    object_typed_expression: TypedExpression,
) -> Result<TypedExpression, String> {
    match *member.clone() {
        parser::Member::Identifier { symbol, .. } => {
            let type_ = type_environment
                .borrow()
                .get_variable(&symbol)
                .or_else(|| type_environment.borrow().get_type_from_str(&symbol))
                .or_else(|| {
                    type_environment
                        .borrow()
                        .get_static_member(object_type.type_annotation(), &symbol)
                })
                .ok_or_else(|| {
                    format!(
                        "Unexpected member access: {} on type {}",
                        symbol,
                        object_type.full_name()
                    )
                })?;

            let Type::Function(Function { param, .. }) = type_.clone() else {
                return Err(format!("{} is not a function", symbol));
            };

            let Some(param) = param else {
                Err(format!(
                    "Function {} must have at least one parameter",
                    symbol
                ))?
            };

            if !type_equals(&param.type_, &object_type) {
                Err(format!(
                    "Function '{}' must be called on type {}. Found {}",
                    symbol, param.type_, object_type
                ))?
            }

            let Type::Function(Function { return_type, .. }) = type_.clone() else {
                return Err(format!("{} is not a function", symbol));
            };

            Ok(TypedExpression::Call {
                callee: Box::new(TypedExpression::Member(Member::Identifier {
                    symbol: symbol.clone(),
                    type_: type_.clone(),
                })),
                argument: Some(Box::new(object_typed_expression)),
                type_: *return_type,
            })
        }
        parser::Member::StaticMemberAccess { .. } => todo!("Static member access"),
        parser::Member::MemberAccess { .. } => todo!("Member access"),
        parser::Member::ParamPropagation { .. } => todo!("Param propagation"),
    }
}

fn get_unop_type(operator: &UnaryOperator, operand: &Type) -> Result<Type, String> {
    match (operator, operand) {
        (UnaryOperator::Identity, Type::Int) => Ok(Type::Int),
        (UnaryOperator::Identity, Type::UInt) => Ok(Type::UInt),
        (UnaryOperator::Identity, Type::Float) => Ok(Type::Float),
        (UnaryOperator::Negate, Type::Int) => Ok(Type::Int),
        (UnaryOperator::Negate, Type::UInt) => Ok(Type::UInt),
        (UnaryOperator::Negate, Type::Float) => Ok(Type::Float),
        (UnaryOperator::Negate, Type::Literal { name, type_ })
            if matches!(**type_, Type::Int | Type::UInt | Type::Float) =>
        {
            let mut buf = String::new();
            buf.push_str("-");
            buf.push_str(name);

            Ok(Type::Literal {
                name: buf,
                type_: type_.clone(),
            })
        }
        (UnaryOperator::LogicalNot, Type::Bool) => Ok(Type::Bool),
        (UnaryOperator::BitwiseNot, Type::Int) => Ok(Type::Int),
        (UnaryOperator::BitwiseNot, Type::UInt) => Ok(Type::UInt),
        _ => Err(format!(
            "Invalid unary operator {:?} for type {}",
            operator, operand
        )),
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
        (Type::Int, BinaryOperator::Range, Type::Int) => Ok(Type::Array(Box::new(Type::Int))),
        (Type::UInt, BinaryOperator::Range, Type::UInt) => Ok(Type::Array(Box::new(Type::UInt))),
        (Type::Char, BinaryOperator::Range, Type::Char) => Ok(Type::Array(Box::new(Type::Char))),
        (Type::TypeAlias(TypeAlias { types, .. }), operator, right_type) => {
            let mut acc = Type::Unknown;

            for type_ in types {
                let t = get_binop_type(&type_, operator, right_type)?;

                if acc == Type::Unknown {
                    acc = t;
                    continue;
                }

                if !type_equals(&acc, &t) {
                    return Err(format!(
                        "Binary operator {:?} is not supported for types {:?} and {:?}",
                        operator, acc, t
                    ));
                }
            }

            Ok(acc)
        }
        (left_type, operator, Type::TypeAlias(TypeAlias { types, .. })) => {
            let mut acc = Type::Unknown;

            for type_ in types {
                let t = get_binop_type(left_type, operator, &type_)?;

                if acc == Type::Unknown {
                    acc = t;
                    continue;
                }

                if !type_equals(&acc, &t) {
                    return Err(format!(
                        "Binary operator {:?} is not supported for types {:?} and {:?}",
                        operator, acc, t
                    ));
                }
            }

            Ok(acc)
        }
        // (Type::Literal { type_, name }, BinaryOperator::Range, Type::UInt)
        //     if **type_ == Type::Int && name.parse::<u64>().is_ok() =>
        // {
        //     Ok(Type::Array(Box::new(Type::UInt)))
        // }
        // (Type::Literal { type_, name }, BinaryOperator::Range, Type::Int)
        //     if **type_ == Type::UInt && name.parse::<i64>().is_ok() =>
        // {
        //     Ok(Type::Array(Box::new(Type::Int)))
        // }
        // (Type::Int, BinaryOperator::RangeInclusive, Type::Int) => {
        //     Ok(Type::Array(Box::new(Type::Int)))
        // }
        // (Type::UInt, BinaryOperator::RangeInclusive, Type::UInt) => {
        //     Ok(Type::Array(Box::new(Type::UInt)))
        // }
        // (Type::Char, BinaryOperator::RangeInclusive, Type::Char) => {
        //     Ok(Type::Array(Box::new(Type::Char)))
        // }
        // (Type::Literal { type_, name }, BinaryOperator::RangeInclusive, Type::Int)
        //     if **type_ == Type::UInt && name.parse::<i64>().is_ok() =>
        // {
        //     Ok(Type::Array(Box::new(Type::Int)))
        // }
        // (Type::Literal { type_, name }, BinaryOperator::RangeInclusive, Type::UInt)
        //     if **type_ == Type::Int && name.parse::<u64>().is_ok() =>
        // {
        //     Ok(Type::Array(Box::new(Type::UInt)))
        // }
        (Type::Literal { name, type_ }, operator, Type::Int) if **type_ == Type::UInt => {
            if name.parse::<i64>().is_ok() {
                get_binop_type(&Type::Int, operator, &Type::Int)
            } else {
                Err(format!("{} is not a valid uint", name))
            }
        }
        (Type::Literal { name, type_ }, operator, Type::UInt) if **type_ == Type::Int => {
            if name.parse::<u64>().is_ok() {
                get_binop_type(&Type::UInt, operator, &Type::UInt)
            } else {
                Err(format!("{} is not a valid int", name))
            }
        }
        (Type::Int, operator, Type::Literal { name, type_ }) if **type_ == Type::UInt => {
            if name.parse::<i64>().is_ok() {
                get_binop_type(&Type::Int, operator, &Type::Int)
            } else {
                Err(format!("{} is not a valid uint", name))
            }
        }
        (Type::UInt, operator, Type::Literal { name, type_ }) if **type_ == Type::Int => {
            if name.parse::<u64>().is_ok() {
                get_binop_type(&Type::UInt, operator, &Type::UInt)
            } else {
                Err(format!("{} is not a valid int", name))
            }
        }
        (Type::Literal { type_, .. }, operator, right_type) => {
            get_binop_type(type_, operator, right_type)
        }
        (left_type, operator, Type::Literal { type_, .. }) => {
            get_binop_type(left_type, operator, type_)
        }
        (Type::Union(Union { literal_type, .. }), operator, right_type) => {
            get_binop_type(literal_type, operator, right_type)
        }
        (left_type, operator, Type::Union(Union { literal_type, .. })) => {
            get_binop_type(left_type, operator, literal_type)
        }
        (Type::Array(left), BinaryOperator::Add, Type::Array(right))
            if type_equals(left, right) =>
        {
            Ok(Type::Array(left.clone()))
        }
        (Type::Array(left), BinaryOperator::Add, right) if type_equals(left, right) => {
            Ok(Type::Array(left.clone()))
        }
        _ => Err(format!(
            "Unexpected binary operator {:?} for types {:?} and {:?}",
            operator, left_type, right_type,
        )),
    }
}
