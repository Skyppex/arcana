use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{self, Assignment, Binary, Expression, For, If, Match, VariableDeclaration, While},
    type_checker::{
        model::{Index, ValueLiteral},
        type_annotation_equals, type_equals_unstrict, StructField,
    },
    types::{TypeAnnotation, TypeIdentifier},
};

use super::{
    decision_tree::{create_decision_tree, Constructor, Pattern},
    get_field_by_name,
    model::{
        BinaryOperator, Block, FieldInitializer, Member, Typed, TypedClosureParameter,
        TypedExpression, TypedMatchArm, TypedStatement, UnaryOperator,
    },
    scope::ScopeType,
    statements::{self, check_type_annotation},
    type_equals, type_equals_coerce, DiscoveredType, Enum, FullName, Function, LiteralType, Rcrc,
    Struct, Type, TypeAlias, TypeEnvironment, Union,
};

pub fn check_type(
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
                            Some(*t.type_)
                        } else {
                            None
                        }
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
            let callee =
                if let Some(built_in_function) = call.callee.get_built_in_function_identifier() {
                    // turn the callee into a TypedExpression which is the build-in function
                    TypedExpression::Member(Member::BuiltInFunction(built_in_function))
                } else {
                    check_type(
                        &call.callee,
                        discovered_types,
                        type_environment.clone(),
                        context.clone(),
                    )?
                };

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
                    ));
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
                } else if let Type::Function(Function { param: None, .. }) = callee_type {
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
                };
            }

            let arg = arg_typed_expression.clone().map(Box::new);

            Ok(TypedExpression::Call {
                callee: Box::new(callee),
                argument: arg,
                type_: return_type,
            })
        }
        Expression::VariableDeclaration(VariableDeclaration {
            mutable,
            type_annotation,
            pattern,
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

                    if *mutable {
                        if let Type::Literal {
                            type_: literal_type,
                            ..
                        } = type_
                        {
                            type_ = literal_type.get_runtime_type();
                        }
                    }

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

            check_type_pattern(
                pattern,
                initializer.as_ref().map(|i| i.get_type()).as_ref(),
                type_environment.clone(),
                Some(type_.clone()),
            )?;

            Ok(TypedExpression::VariableDeclaration {
                mutable: *mutable,
                pattern: pattern.clone(),
                initializer: initializer.map(Box::new),
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
                condition,
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
                true_expression,
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

            let type_ = if let Some(else_type) = else_type {
                if !is_option(&else_type) {
                    if !type_equals_unstrict(&if_block_type, &else_type) {
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
                    pattern,
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
            crate::ast::Member::Identifier { symbol, generics } => {
                let type_ = type_environment
                    .borrow()
                    .get_variable(symbol)
                    .or_else(|| {
                        let type_ = type_environment.borrow().get_type(member);

                        match (type_, generics) {
                            (None, _) => None,
                            (Some(type_), None) => Some(type_),
                            (Some(type_), Some(generics)) => Some(
                                type_
                                    .clone_with_concrete_types(
                                        generics.iter().map(|g| g.type_annotation()).collect(),
                                        discovered_types,
                                        type_environment.clone(),
                                        None,
                                    )
                                    .expect("Failed to clone type with concrete types"),
                            ),
                        }
                    })
                    .ok_or_else(|| format!("Unexpected variable: {}", symbol))?
                    .clone();

                Ok(TypedExpression::Member(Member::Identifier {
                    symbol: symbol.clone(),
                    type_,
                }))
            }
            crate::ast::Member::StaticMemberAccess {
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
            crate::ast::Member::MemberAccess { object, member, .. } => check_type_member_access(
                object,
                discovered_types,
                type_environment,
                member,
                context,
            ),
            crate::ast::Member::ParamPropagation { object, member, .. } => {
                check_type_param_propagation(
                    object,
                    member,
                    discovered_types,
                    type_environment,
                    context,
                )
            }
            crate::ast::Member::Index { object, index } => {
                check_type_index(object, index, discovered_types, type_environment, context)
            }
        },
        Expression::Literal(l) => match l {
            ast::ValueLiteral::Unit => Ok(TypedExpression::Literal(ValueLiteral::Unit)),
            ast::ValueLiteral::Int(v) => Ok(TypedExpression::Literal(ValueLiteral::Int(*v))),
            ast::ValueLiteral::UInt(v) => Ok(TypedExpression::Literal(ValueLiteral::UInt(*v))),
            ast::ValueLiteral::Float(v) => Ok(TypedExpression::Literal(ValueLiteral::Float(*v))),
            ast::ValueLiteral::String(v) => {
                Ok(TypedExpression::Literal(ValueLiteral::String(v.clone())))
            }
            ast::ValueLiteral::Char(v) => Ok(TypedExpression::Literal(ValueLiteral::Char(*v))),
            ast::ValueLiteral::Bool(v) => Ok(TypedExpression::Literal(ValueLiteral::Bool(*v))),
            ast::ValueLiteral::Array(values) => {
                let v: Result<(Vec<TypedExpression>, Type), String> = {
                    let mut v_: Vec<TypedExpression> = vec![];
                    let mut previous_type = Type::Void;

                    for value in values {
                        let value =
                            check_type(value, discovered_types, type_environment.clone(), None)?;

                        let type_ = value.get_deep_type();

                        if !type_equals(&previous_type, &Type::Void)
                            && !type_equals_unstrict(&type_, &previous_type)
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

                Ok(TypedExpression::Literal(ValueLiteral::Array {
                    values: v.0,
                    type_: target_type,
                }))
            }
            ast::ValueLiteral::Struct {
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

                let mut field_initializers = field_initializers?;

                let field_initializer_map: HashMap<_, _> = field_initializers
                    .iter()
                    .map(|fi| (fi.identifier.clone(), fi.initializer.clone()))
                    .collect();

                for (field, initializer) in fields
                    .iter()
                    .map(|f| (f, field_initializer_map.get(&f.field_name)))
                {
                    match (initializer, &field.default_value) {
                        (None, None) => {
                            return Err(format!(
                                "Field '{}' is missing from struct initializer",
                                field.field_name
                            ))
                        }
                        (None, Some(default_value)) => {
                            let field_type = field.field_type.clone();

                            if !type_equals(&field_type, default_value) {
                                return Err(format!(
                                    "Field type {} does not match initializer type {}",
                                    field_type, default_value
                                ));
                            }

                            let lit = ValueLiteral::try_from(default_value.clone())?;

                            let initializer = TypedExpression::Literal(lit);

                            field_initializers.push(FieldInitializer {
                                identifier: field.field_name.clone(),
                                initializer,
                            });
                        }
                        (Some(initializer), _) => {
                            let field_type = field.field_type.clone();
                            let initializer_type = initializer.get_type();

                            if !type_equals(&field_type, &initializer_type) {
                                return Err(format!(
                                    "Field type {} does not match initializer type {}",
                                    field_type, initializer_type
                                ));
                            }
                        }
                    }
                }

                Ok(TypedExpression::Literal(ValueLiteral::Struct {
                    type_annotation: type_annotation.clone(),
                    field_initializers,
                    type_,
                }))
            }
            ast::ValueLiteral::Enum {
                type_annotation,
                member,
                field_initializers,
            } => {
                let field_initializers: Result<Vec<FieldInitializer>, String> = {
                    let mut fis = Vec::new();

                    for ast::model::FieldInitializer {
                        identifier,
                        initializer,
                    } in field_initializers
                    {
                        fis.push(FieldInitializer {
                            identifier: identifier.clone(),
                            initializer: check_type(
                                initializer,
                                discovered_types,
                                type_environment.clone(),
                                None,
                            )?,
                        });
                    }

                    Ok(fis)
                };

                let type_ = type_environment
                    .borrow()
                    .get_type_from_annotation(type_annotation)?;

                let Type::Struct(Struct { fields, .. }) = &type_ else {
                    Err(format!(
                        "{} is not a member of {}",
                        member,
                        type_.full_name()
                    ))?
                };

                let mut field_initializers = field_initializers?;

                let field_initializer_map: HashMap<_, _> = field_initializers
                    .iter()
                    .map(|fi| (fi.identifier.clone(), fi.initializer.clone()))
                    .collect();

                for (struct_field, initializer) in fields
                    .iter()
                    .map(|f| (f, field_initializer_map.get(&f.field_name)))
                {
                    match (initializer, &struct_field.default_value) {
                        (None, None) => {
                            return Err(format!(
                                "Field '{}' is missing from struct initializer",
                                struct_field.field_name
                            ))
                        }
                        (None, Some(default_value)) => {
                            let field_type = struct_field.field_type.clone();

                            if !type_equals(&field_type, default_value) {
                                return Err(format!(
                                    "Field type {} does not match initializer type {}",
                                    field_type, default_value
                                ));
                            }

                            let lit = ValueLiteral::try_from(default_value.clone())?;

                            let initializer = TypedExpression::Literal(lit);

                            field_initializers.push(FieldInitializer {
                                identifier: struct_field.field_name.clone(),
                                initializer,
                            });
                        }
                        (Some(initializer), _) => {
                            let field_type = struct_field.field_type.clone();
                            let initializer_type = initializer.get_type();

                            if !type_equals(&field_type, &initializer_type) {
                                return Err(format!(
                                    "Field type {} does not match initializer type {}",
                                    field_type, initializer_type
                                ));
                            }
                        }
                    }
                }

                Ok(TypedExpression::Literal(ValueLiteral::Enum {
                    type_annotation: type_annotation.clone(),
                    member: member.clone(),
                    field_initializers,
                    type_,
                }))
            }
        },
        Expression::Tuple(elements) => {
            let typed_elements = elements
                .iter()
                .map(|element| {
                    check_type(
                        element,
                        discovered_types,
                        type_environment.clone(),
                        context.clone(),
                    )
                })
                .collect::<Result<Vec<TypedExpression>, String>>()?;

            let types = typed_elements.iter().map(|e| e.get_type()).collect();

            Ok(TypedExpression::Tuple {
                elements: typed_elements,
                type_: Type::Tuple(types),
            })
        }
        Expression::Unary(unary) => {
            let expression =
                check_type(&unary.expression, discovered_types, type_environment, None)?;
            let type_ = expression.get_deep_type();

            let operator = match unary.operator {
                ast::UnaryOperator::Identity => UnaryOperator::Identity,
                ast::UnaryOperator::Negate => UnaryOperator::Negate,
                ast::UnaryOperator::LogicalNot => UnaryOperator::LogicalNot,
                ast::UnaryOperator::BitwiseNot => UnaryOperator::BitwiseNot,
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
            ) && !type_equals_coerce(&right.get_type(), &left.get_type())
            {
                return Err(format!(
                    "Range operator requires both sides to be of the same type, found {} and {}",
                    left.get_type(),
                    right.get_type()
                ));
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
                        type_ = e.get_type();
                    }
                    _ => continue,
                }
            }

            Ok(TypedExpression::Block(Block {
                statements: typed_statements,
                type_,
            }))
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
                return Err("While condition must be of type bool".to_string());
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

                    if !type_equals(&type_, &Type::Void)
                        && !type_equals_unstrict(&type_, &else_type)
                    {
                        return Err(format!("While block breaks with value of type {} which does not match else blocks type {}", type_, else_body.get_type()));
                    }

                    type_ = else_type
                }
                None => {
                    if !type_equals(&type_, &Type::Void) {
                        return Err(
                            "Must have an else block if the while block breaks with a value"
                                .to_string(),
                        );
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
            pattern,
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

            check_type_pattern(
                pattern,
                Some(inner_type.as_ref()),
                for_environment.clone(),
                context.clone(),
            )?;

            // for_environment
            //     .borrow_mut()
            //     .add_variable(identifier.clone(), *inner_type);

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
                        && !type_equals_unstrict(&type_, &else_body.get_type())
                    {
                        return Err(format!("For block breaks with value of type {} which does not match else blocks type {}", type_, else_body.get_type()));
                    }

                    type_ = else_type
                }
                None => {
                    if !type_equals(&type_, &Type::Void) {
                        return Err(
                            "Must have an else block if the for block breaks with a value"
                                .to_string(),
                        );
                    }
                }
            };

            Ok(TypedExpression::For {
                pattern: pattern.clone(),
                iterable: Box::new(iterable),
                body: Box::new(body),
                else_body: else_body.map(Box::new),
                type_: type_.clone(),
            })
        }
    }
}

fn is_option(type_: &Type) -> bool {
    let Type::Enum(Enum {
        type_identifier,
        shared_fields,
        members,
    }) = type_
    else {
        return false;
    };

    match type_identifier {
        TypeIdentifier::GenericType(name, ..) | TypeIdentifier::ConcreteType(name, ..) => {
            if name != "Option" {
                return false;
            }

            if !shared_fields.is_empty() {
                return false;
            }

            members.get("Some").is_some_and(|member| {
                let Type::Struct(Struct { fields, .. }) = member else {
                    return false;
                };

                get_field_by_name(fields, "f0").is_some_and(|_| true)
            })
        }
        _ => false,
    }
}

fn check_type_static_member_access(
    type_annotation: &TypeAnnotation,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
    member: &ast::Member,
    context: Option<Type>,
) -> Result<TypedExpression, String> {
    let object_type =
        check_type_annotation(type_annotation, discovered_types, type_environment.clone())?;

    match member.clone() {
        ast::Member::Identifier { symbol, .. } => match &object_type {
            Type::Struct(struct_) => {
                let Some(static_member_type) = type_environment
                    .borrow()
                    .get_static_member(&object_type, &symbol)
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
            Type::Enum(enum_) => {
                let Some(static_member_type) = type_environment
                    .borrow()
                    .get_static_member(&object_type, &symbol)
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
        ast::Member::StaticMemberAccess {
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
        ast::Member::MemberAccess { object, member, .. } => check_type_member_access(
            &object,
            discovered_types,
            type_environment,
            &member,
            context,
        ),
        ast::Member::ParamPropagation { object, member, .. } => check_type_param_propagation(
            &object,
            &member,
            discovered_types,
            type_environment,
            context,
        ),
        ast::Member::Index { object, index } => {
            check_type_index(&object, &index, discovered_types, type_environment, context)
        }
    }
}

fn check_type_member_access(
    object: &Expression,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
    member: &ast::Member,
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
    member: &ast::Member,
    type_environment: Rcrc<TypeEnvironment>,
    object_typed_expression: TypedExpression,
    discovered_types: &Vec<DiscoveredType>,
    context: Option<Type>,
) -> Result<TypedExpression, String> {
    match member.clone() {
        ast::Member::Identifier { symbol, .. } => match object_type {
            Type::Struct(struct_) => {
                let field_type = get_field_by_name(&struct_.fields, &symbol)
                    .ok_or(format!(
                        "Struct '{}' does not have a field called '{}'",
                        struct_.type_identifier, symbol
                    ))?
                    .field_type
                    .clone();

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
                let field_type = get_field_by_name(&shared_fields, &symbol)
                    .ok_or(format!(
                        "Enum '{}' does not have a shared field called '{}'",
                        type_identifier, symbol
                    ))?
                    .field_type
                    .clone();

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
        ast::Member::StaticMemberAccess {
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
        ast::Member::MemberAccess { object, member, .. } => check_type_member_access(
            &object,
            discovered_types,
            type_environment,
            &member,
            context,
        ),
        ast::Member::ParamPropagation { object, member, .. } => check_type_param_propagation(
            &object,
            &member,
            discovered_types,
            type_environment,
            context,
        ),
        ast::Member::Index { object, index } => {
            check_type_index(&object, &index, discovered_types, type_environment, context)
        }
    }
}

fn check_type_param_propagation(
    object: &Expression,
    member: &ast::Member,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
    context: Option<Type>,
) -> Result<TypedExpression, String> {
    let ast::Member::Identifier { .. } = member.clone() else {
        return Err("Param propagation must be followed by a member access".to_string());
    };

    let object_type_expression =
        check_type(object, discovered_types, type_environment.clone(), context)?;

    let object_type = object_type_expression.get_type();

    let member_type = type_environment.borrow().get_type(member).or_else(|| {
        type_environment
            .borrow()
            .get_static_member(&object_type, member)
    });

    let Some(Type::Function(Function { param, .. })) = member_type else {
        return Err(format!("{} is not a function", member));
    };

    let Some(param) = param else {
        Err(format!(
            "Function {} must have at least one parameter",
            member
        ))?
    };

    if !type_equals(&param.type_, &object_type) {
        Err(format!(
            "Function '{}' must be called on type {}. Found {}",
            member, param.type_, object_type
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
    member: &ast::Member,
    type_environment: Rcrc<TypeEnvironment>,
    object_typed_expression: TypedExpression,
) -> Result<TypedExpression, String> {
    match member.clone() {
        ast::Member::Identifier { symbol, .. } => {
            let type_ = type_environment
                .borrow()
                .get_variable(&symbol)
                .or_else(|| type_environment.borrow().get_type(&symbol))
                .or_else(|| {
                    type_environment
                        .borrow()
                        .get_static_member(&object_type, &symbol)
                })
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

            if !type_equals(&param.type_, &object_type) {
                Err(format!(
                    "Function '{}' must be called on type {}. Found {}",
                    symbol, param.type_, object_type
                ))?
            }

            // This version will return a function requiring parens for it to be called
            // 16:sqrt will produce a closure that needs to be called. 16:sqrt() will produce 4
            Ok(TypedExpression::Closure {
                param: None,
                return_type: *return_type.clone(),
                body: Box::new(TypedExpression::Call {
                    callee: Box::new(TypedExpression::Member(Member::Identifier {
                        symbol: symbol.clone(),
                        type_: type_.clone(),
                    })),
                    argument: Some(Box::new(object_typed_expression)),
                    type_: *return_type.clone(),
                }),
                type_: Type::Function(Function {
                    identifier: None,
                    param: None,
                    return_type,
                }),
            })

            // // This version will call a function without requiring parens. 16:sqrt will just produce 4
            // Ok(TypedExpression::Call {
            //     callee: Box::new(TypedExpression::Member(Member::Identifier {
            //         symbol: symbol.clone(),
            //         type_: type_.clone(),
            //     })),
            //     argument: Some(Box::new(object_typed_expression)),
            //     type_: *return_type,
            // })
        }
        ast::Member::StaticMemberAccess { .. } => todo!("Static member access"),
        ast::Member::MemberAccess { .. } => todo!("Member access"),
        ast::Member::ParamPropagation { .. } => todo!("Param propagation"),
        ast::Member::Index { .. } => todo!("Index"),
    }
}

fn check_type_index(
    object: &Expression,
    index: &ast::Index,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rc<RefCell<TypeEnvironment>>,
    context: Option<Type>,
) -> Result<TypedExpression, String> {
    let typed_object = check_type(
        object,
        discovered_types,
        type_environment.clone(),
        context.clone(),
    )?;

    let object_type = typed_object.get_type();

    let Type::Array(ref inner_type) = object_type else {
        return Err("Indexing can only be done on arrays".to_string());
    };

    let (typed_index, return_type) = match index {
        ast::Index::Value(expression) => {
            let typed_index = check_type(expression, discovered_types, type_environment, context)?;

            let index_type = typed_index.get_type();

            if !type_equals_coerce(&Type::UInt, &index_type) {
                return Err(format!(
                    "Indexing requires an integer, found {}",
                    index_type.full_name()
                ));
            }

            (Index::Value(Box::new(typed_index)), *inner_type.clone())
        }
        ast::Index::Range {
            start,
            end,
            inclusive,
        } => {
            let start = start
                .clone()
                .map(|start| {
                    check_type(
                        &start,
                        discovered_types,
                        type_environment.clone(),
                        context.clone(),
                    )
                })
                .transpose()?;

            if let Some(start) = &start {
                let index_type = start.get_type();

                if !type_equals_coerce(&Type::UInt, &index_type) {
                    return Err(format!(
                        "Indexing requires an integer, found {}",
                        index_type.full_name()
                    ));
                }
            }

            let end = end
                .clone()
                .map(|end| {
                    check_type(
                        &end,
                        discovered_types,
                        type_environment.clone(),
                        context.clone(),
                    )
                })
                .transpose()?;

            if let Some(end) = &end {
                let index_type = end.get_type();

                if !type_equals_coerce(&Type::UInt, &index_type) {
                    return Err(format!(
                        "Indexing requires an integer, found {}",
                        index_type.full_name()
                    ));
                }
            }

            (
                Index::Range {
                    start: start.map(Box::new),
                    end: end.map(Box::new),
                    inclusive: *inclusive,
                },
                object_type,
            )
        }
    };

    Ok(TypedExpression::Member(Member::Index {
        object: Box::new(typed_object),
        index: typed_index,
        type_: return_type,
    }))
}

fn check_type_pattern(
    pattern: &Pattern,
    initializer_type: Option<&Type>,
    type_environment: Rcrc<TypeEnvironment>,
    context: Option<Type>,
) -> Result<(), String> {
    match pattern {
        Pattern::Wildcard => Ok(()),
        Pattern::Unit => Ok(()),
        Pattern::Variable(identifier) => {
            if let Some(context) = context {
                type_environment
                    .borrow_mut()
                    .add_variable(identifier.clone(), context);

                return Ok(());
            }

            if let Some(initializer_type) = initializer_type {
                type_environment
                    .borrow_mut()
                    .add_variable(identifier.clone(), initializer_type.clone());

                return Ok(());
            }

            type_environment
                .borrow_mut()
                .add_variable(identifier.clone(), Type::Unknown);

            Ok(())
        }
        Pattern::Constructor(Constructor::Struct {
            type_annotation,
            field_patterns,
        }) => {
            let Some(initializer_type) = initializer_type else {
                return Err("Expected initializer for constructor pattern".to_string());
            };

            let mut is_enum_member = false;

            match &initializer_type {
                Type::Enum(Enum { members, .. }) => {
                    for member_type in members.values() {
                        if type_annotation_equals(type_annotation, &member_type.type_annotation()) {
                            is_enum_member = true;
                            break;
                        }
                    }
                }
                Type::Struct(Struct {
                    type_identifier: TypeIdentifier::MemberType(_, discriminant_name),
                    ..
                }) => {
                    if type_annotation_equals(
                        type_annotation,
                        &TypeAnnotation::Type(discriminant_name.clone()),
                    ) {
                        is_enum_member = true;
                    }
                }
                _ => {}
            }

            if !is_enum_member
                && !type_annotation_equals(
                    &initializer_type.type_annotation(),
                    &type_annotation.clone(),
                )
            {
                return Err(format!(
                    "Expected type annotation {} but got {}",
                    initializer_type.type_annotation(),
                    type_annotation,
                ));
            }

            let fields = match initializer_type.clone() {
                Type::Struct(Struct { fields, .. }) => fields,
                Type::Enum(Enum {
                    shared_fields,
                    members,
                    ..
                }) => {
                    let member = members.get(&type_annotation.name()).expect(
                        "Already checked if the constructor name exists in the member list",
                    );

                    let Type::Struct(Struct { fields, .. }) = member.clone() else {
                        return Err(format!("Expected enum member but got {:?}", member.clone()));
                    };

                    fields.into_iter().chain(shared_fields).collect()
                }
                _ => {
                    return Err(format!(
                        "Expected struct, enum or enum member but got {:?}",
                        initializer_type.clone()
                    ));
                }
            };

            for StructField {
                field_name,
                field_type,
                ..
            } in fields
            {
                let field_pattern = field_patterns
                    .iter()
                    .find(|field_pattern| field_pattern.identifier == field_name)
                    .ok_or_else(|| {
                        format!(
                            "Field {} not found in constructor pattern",
                            field_name.clone()
                        )
                    })?;

                check_type_pattern(
                    &field_pattern.pattern,
                    Some(&field_type),
                    type_environment.clone(),
                    context.clone(),
                )?;
            }

            Ok(())
        }
        Pattern::Tuple(patterns) => {
            let Some(initializer_type) = initializer_type else {
                return Err("Expected initializer for tuple pattern".to_string());
            };

            let Type::Tuple(types) = initializer_type else {
                return Err(format!(
                    "Expected tuple type but got {}",
                    initializer_type.full_name()
                ));
            };

            if patterns.len() != types.len() {
                return Err(format!(
                    "Expected {} patterns but got {}",
                    types.len(),
                    patterns.len()
                ));
            }

            for (pattern, type_) in patterns.iter().zip(types) {
                check_type_pattern(
                    pattern,
                    Some(type_),
                    type_environment.clone(),
                    context.clone(),
                )?;
            }

            Ok(())
        }
        Pattern::Bool(_)
        | Pattern::Int(_)
        | Pattern::UInt(_)
        | Pattern::Float(_)
        | Pattern::Char(_)
        | Pattern::String(_)
        | Pattern::LessThan(_)
        | Pattern::GreaterThan(_)
        | Pattern::LessThanOrEqual(_)
        | Pattern::GreaterThanOrEqual(_)
        | Pattern::Range(_, _, _) => Err("Refutable pattern".to_string()),
    }
}

fn get_unop_type(operator: &UnaryOperator, operand: &Type) -> Result<Type, String> {
    match (operator, operand) {
        (UnaryOperator::Identity, Type::Int) => Ok(Type::Int),
        (UnaryOperator::Identity, Type::UInt) => Ok(Type::UInt),
        (UnaryOperator::Identity, Type::Float) => Ok(Type::Float),
        (UnaryOperator::Identity, Type::Literal { name, type_ })
            if matches!(
                **type_,
                LiteralType::Int | LiteralType::UInt | LiteralType::Float
            ) =>
        {
            let mut buf = String::new();
            buf.push_str(name);

            Ok(Type::Literal {
                name: buf,
                type_: type_.clone(),
            })
        }
        (UnaryOperator::Negate, Type::Int) => Ok(Type::Int),
        (UnaryOperator::Negate, Type::UInt) => Ok(Type::UInt),
        (UnaryOperator::Negate, Type::Float) => Ok(Type::Float),
        (UnaryOperator::Negate, Type::Literal { name, type_ })
            if matches!(
                **type_,
                LiteralType::Int | LiteralType::UInt | LiteralType::Float
            ) =>
        {
            let mut buf = String::new();
            buf.push('-');
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
                let t = get_binop_type(type_, operator, right_type)?;

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
                let t = get_binop_type(left_type, operator, type_)?;

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
        (Type::Int, BinaryOperator::RangeInclusive, Type::Int) => {
            Ok(Type::Array(Box::new(Type::Int)))
        }
        (Type::UInt, BinaryOperator::RangeInclusive, Type::UInt) => {
            Ok(Type::Array(Box::new(Type::UInt)))
        }
        (Type::Char, BinaryOperator::RangeInclusive, Type::Char) => {
            Ok(Type::Array(Box::new(Type::Char)))
        }
        (Type::Literal { type_, .. }, operator, Type::Int)
            if matches!(**type_, LiteralType::UIntValue(_)) =>
        {
            let LiteralType::UIntValue(value) = type_.as_ref() else {
                unreachable!()
            };

            if *value < i64::MAX as u64 {
                get_binop_type(&Type::UInt, operator, &Type::UInt)
            } else {
                Err(format!("{} is not a valid i64", value))
            }
        }
        (Type::Literal { type_, .. }, operator, Type::UInt)
            if matches!(**type_, LiteralType::IntValue(_)) =>
        {
            let LiteralType::IntValue(value) = type_.as_ref() else {
                unreachable!()
            };

            if *value >= 0 {
                get_binop_type(&Type::UInt, operator, &Type::UInt)
            } else {
                Err(format!("{} is not a valid u64", value))
            }
        }
        (Type::Int, operator, Type::Literal { type_, .. })
            if matches!(**type_, LiteralType::UIntValue(_)) =>
        {
            let LiteralType::UIntValue(value) = type_.as_ref() else {
                unreachable!()
            };

            if *value < i64::MAX as u64 {
                get_binop_type(&Type::UInt, operator, &Type::UInt)
            } else {
                Err(format!("{} is not a valid i64", value))
            }
        }
        (Type::UInt, operator, Type::Literal { type_, .. })
            if matches!(**type_, LiteralType::IntValue(_)) =>
        {
            let LiteralType::IntValue(value) = type_.as_ref() else {
                unreachable!()
            };

            if *value >= 0 {
                get_binop_type(&Type::UInt, operator, &Type::UInt)
            } else {
                Err(format!("{} is not a valid u64", value))
            }
        }
        (Type::Literal { type_, .. }, operator, right_type) => {
            get_binop_type(&type_.get_runtime_type(), operator, right_type)
        }
        (left_type, operator, Type::Literal { type_, .. }) => {
            get_binop_type(left_type, operator, &type_.get_runtime_type())
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
