use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{
        self, Assignment, Binary, Expression, For, If, Match, UseExpr, VariableDeclaration, While,
    },
    built_in::{BuiltInFunction, BuiltInFunctionType},
    type_checker::{
        model::{ArrayItem, Index, ValueLiteral},
        type_equals_unstrict, Parameter,
    },
    types::{TypeAnnotation, TypeIdentifier},
};

use super::{
    contains_generic,
    decision_tree::{compile_match, CompilableArm},
    get_enum_member, get_field_by_name, join_types,
    model::{
        BinaryOperator, Block, FieldInitializer, Member, Typed, TypedClosureParameter,
        TypedExpression, TypedMatchArm, TypedStatement, UnaryOperator,
    },
    overloaded_member_name,
    pattern::{check_pattern, CheckedPattern},
    runtime_type,
    scope::ScopeType,
    statements::{self, check_type_annotation},
    type_equals, type_equals_coerce, DiscoveredType, Enum, FullName, Function, LiteralType, Rcrc,
    Struct, Type, TypeAlias, TypeEnvironment, Union,
};

use crate::ast::pattern::Pattern;

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
            // A static member may come from several implementations — `C::from`
            // for both `From<A>` and `From<B>`. Which one is meant is settled
            // by the argument, so that is checked before the callee.
            if let Expression::Member(ast::Member::StaticMemberAccess {
                type_annotation,
                member,
                ..
            }) = call.callee.as_ref()
            {
                if let (ast::Member::Identifier { symbol, .. }, Some(argument)) =
                    (member.as_ref(), &call.argument)
                {
                    if let Some(overloaded) = check_overloaded_static_call(
                        type_annotation,
                        symbol,
                        argument,
                        discovered_types,
                        type_environment.clone(),
                    )? {
                        return Ok(overloaded);
                    }
                }
            }

            // `value:typeof()` propagates the value in as the argument, so the
            // call carries none of its own. Both spellings answer the same
            // question and fold the same way.
            if let (
                Expression::Member(ast::Member::ParamPropagation { object, member, .. }),
                None,
            ) = (call.callee.as_ref(), &call.argument)
            {
                if is_typeof(member) {
                    let argument =
                        check_type(object, discovered_types, type_environment.clone(), None)?;

                    return Ok(typeof_literal(&argument));
                }
            }

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

            // `typeof` asks about the static type, which only exists here, so
            // it is answered at check time and folds away to the string it
            // produced — `typeof(4)` becomes the literal `"#4"`, of type `#"#4"`.
            if let TypedExpression::Member(Member::BuiltInFunction(BuiltInFunction {
                function_type: BuiltInFunctionType::TypeOf,
                ..
            })) = &callee
            {
                let Some(argument) = &arg_typed_expression else {
                    return Err(format!(
                        "{}, and needs a value to report on",
                        TYPEOF_IS_NOT_A_VALUE
                    ));
                };

                return Ok(typeof_literal(argument));
            }

            let mut callee = callee;
            let mut return_type = return_type;
            let mut callee_type = callee_type;

            // A member a type gets from an implementation written for a bare
            // parameter is specialised here: its body may dispatch on the
            // implementation's parameters, which only exist while checking.
            if let (
                Expression::Member(ast::Member::ParamPropagation { object, member, .. }),
                None,
            ) = (call.callee.as_ref(), &call.argument)
            {
                if let ast::Member::Identifier { symbol, .. } = member.as_ref() {
                    if let Some(specialised) = specialise_universal_member(
                        object,
                        symbol,
                        discovered_types,
                        type_environment.clone(),
                    )? {
                        return Ok(specialised);
                    }
                }
            }

            // With the type arguments written out there is nothing to infer, but
            // a dispatching body still needs specialising to them.
            if let Some(bindings) = written_type_arguments(&call.callee, type_environment.clone()) {
                if let Some(specialised) = specialise_generic_call(
                    &callee,
                    &bindings,
                    discovered_types,
                    type_environment.clone(),
                )? {
                    callee = specialised;
                }
            }

            // A generic function called without type arguments takes them from
            // the argument it was given and from where its result is going.
            let argument_type = arg_typed_expression.as_ref().map(|arg| arg.get_type());

            if let Some((inferred, bindings)) = infer_call_type_arguments(
                &callee_type,
                argument_type.as_ref(),
                context.as_ref(),
                discovered_types,
                type_environment.clone(),
            )? {
                if let Type::Function(Function {
                    return_type: inferred_return,
                    ..
                }) = &inferred
                {
                    return_type = *inferred_return.clone();
                }

                // A body that dispatches on a type parameter is replaced by a
                // copy specialised to these arguments.
                callee = match specialise_generic_call(
                    &callee,
                    &bindings,
                    discovered_types,
                    type_environment.clone(),
                )? {
                    Some(specialised) => specialised,
                    None => retype(callee, inferred.clone()),
                };

                callee_type = inferred;
            }

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
                type_: Type::Bool,
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

            let type_ = match else_type {
                // An `else if` chain can run out of branches, so an optional
                // else makes the whole expression optional. The branches are
                // joined inside the Option, and the bare branch is wrapped in
                // Some when it is evaluated.
                Some(else_type) if is_option(&else_type) => {
                    let else_inner = option_inner(&else_type).unwrap_or(Type::Unknown);

                    let joined = if matches!(else_inner, Type::Unknown) {
                        if_block_type.clone()
                    } else {
                        join_types(&if_block_type, &else_inner).ok_or(format!(
                            "If block type {} does not match else block type {}",
                            if_block_type, else_inner
                        ))?
                    };

                    Type::option_of(joined)
                }
                Some(else_type) => join_types(&if_block_type, &else_type).ok_or(format!(
                    "If block type {} does not match else block type {}",
                    if_block_type, else_type
                ))?,
                None => Type::option_of(if_block_type.clone()),
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

            let matchee_type = runtime_type(&expression.get_type());

            if arms.is_empty() {
                return Err("Match must have at least one arm".to_string());
            }

            // Each arm is checked once, in a scope holding the bindings its own
            // pattern introduces.
            let mut typed_arms: Vec<TypedMatchArm> = vec![];
            let mut type_: Option<Type> = None;

            for arm in arms {
                let arm_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
                    match_environment.clone(),
                )));

                let (checked_pattern, bindings) =
                    check_pattern(&arm.pattern, &matchee_type, arm_environment.clone())?;

                for (identifier, binding_type) in bindings {
                    arm_environment
                        .borrow_mut()
                        .add_variable(identifier, binding_type);
                }

                let body = check_type(
                    &arm.expression,
                    discovered_types,
                    arm_environment.clone(),
                    context.clone(),
                )?;

                type_ = Some(match type_ {
                    None => body.get_type(),
                    Some(joined) => join_types(&joined, &body.get_type()).ok_or(format!(
                        "Match arms have incompatible types: {} and {}",
                        joined,
                        body.get_type()
                    ))?,
                });

                typed_arms.push(TypedMatchArm {
                    pattern: arm.pattern.clone(),
                    checked_pattern,
                    expression: body,
                });
            }

            let type_ = type_.expect("at least one arm");

            let compilable = typed_arms
                .iter()
                .map(|arm| CompilableArm {
                    pattern: arm.checked_pattern.clone(),
                    body: arm.expression.clone(),
                })
                .collect::<Vec<_>>();

            let compiled = compile_match(matchee_type, &compilable, type_.clone())?;

            if let Some(arm) = compiled.unreachable_arms.first() {
                return Err(format!(
                    "Match arm `{}` is unreachable",
                    typed_arms[*arm].pattern
                ));
            }

            Ok(TypedExpression::Match {
                expression: Box::new(expression),
                arms: typed_arms,
                decision_tree: compiled.decision,
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
                // `typeof` is answered while checking and has no value to carry
                // into the program, so it cannot be referred to as one.
                if is_typeof(member) {
                    return Err(TYPEOF_IS_NOT_A_VALUE.to_string());
                }

                let variable = type_environment.borrow().get_variable(symbol);

                let type_ = match variable {
                    Some(variable) => variable,
                    None => {
                        let type_ = type_environment
                            .borrow()
                            .get_type(member)
                            .ok_or_else(|| format!("Unexpected variable: {}", symbol))?;

                        match generics {
                            None => type_,
                            Some(generics) => {
                                check_type_argument_count(&type_, generics.len(), symbol)?;

                                type_.clone_with_concrete_types(
                                    generics.iter().map(|g| g.type_annotation()).collect(),
                                    discovered_types,
                                    type_environment.clone(),
                                    None,
                                )?
                            }
                        }
                    }
                };

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
            ast::ValueLiteral::Unit => Ok(TypedExpression::Literal {
                literal: ValueLiteral::Unit,
                type_: Type::Unit,
            }),
            ast::ValueLiteral::Int(v) => Ok(TypedExpression::Literal {
                literal: ValueLiteral::Int(*v),
                type_: Type::int_literal(*v),
            }),
            ast::ValueLiteral::UInt(v) => Ok(TypedExpression::Literal {
                literal: ValueLiteral::UInt(*v),
                type_: Type::uint_literal(*v),
            }),
            ast::ValueLiteral::Float(v) => Ok(TypedExpression::Literal {
                literal: ValueLiteral::Float(*v),
                type_: Type::float_literal(*v),
            }),
            ast::ValueLiteral::String(v) => Ok(TypedExpression::Literal {
                literal: ValueLiteral::String(v.clone()),
                type_: Type::string_literal(v.clone()),
            }),
            ast::ValueLiteral::Rune(v) => Ok(TypedExpression::Literal {
                literal: ValueLiteral::Rune(*v),
                type_: Type::rune_literal(v.to_string()),
            }),
            ast::ValueLiteral::Bool(v) => Ok(TypedExpression::Literal {
                literal: ValueLiteral::Bool(*v),
                type_: Type::bool_literal(*v),
            }),
            ast::ValueLiteral::Array(items) => {
                let v: Result<(Vec<ArrayItem>, Type), String> = {
                    let mut v_: Vec<ArrayItem> = vec![];
                    let mut previous_type = Type::Unknown;

                    for item in items {
                        match item {
                            ast::ArrayItem::Expression(value) => {
                                let value = check_type(
                                    value,
                                    discovered_types,
                                    type_environment.clone(),
                                    None,
                                )?;

                                let type_ = if let Type::Array(_) = value.get_type() {
                                    value.get_type()
                                } else {
                                    value.get_deep_type()
                                };

                                if !type_equals(&previous_type, &Type::Unknown)
                                    && !type_equals_unstrict(&type_, &previous_type)
                                {
                                    return Err(format!(
                                            "Array element type {:?} does not match previous element type {:?}",
                                            type_, previous_type
                                    ));
                                }

                                // The array's element type covers every
                                // element, so differing literals widen rather
                                // than the last one winning.
                                previous_type = join_types(&previous_type, &type_)
                                    .unwrap_or_else(|| type_.clone());
                                v_.push(ArrayItem::Expression(value));
                            }
                            ast::ArrayItem::Spread(value) => {
                                let value = check_type(
                                    value,
                                    discovered_types,
                                    type_environment.clone(),
                                    None,
                                )?;

                                let type_ = if let Type::Array(inner_type) = value.get_type() {
                                    *inner_type
                                } else {
                                    return Err(format!(
                                        "Exprected to spread and array but found {:?}",
                                        value.get_type()
                                    ));
                                };

                                if !type_equals(&previous_type, &Type::Unknown)
                                    && !type_equals_unstrict(&type_, &previous_type)
                                {
                                    return Err(format!(
                                        "Array element type {:?} does not match previous element type {:?}",
                                        type_, previous_type
                                    ));
                                }

                                // The array's element type covers every
                                // element, so differing literals widen rather
                                // than the last one winning.
                                previous_type = join_types(&previous_type, &type_)
                                    .unwrap_or_else(|| type_.clone());

                                v_.push(ArrayItem::Spread(value));
                            }
                        }
                    }

                    Ok((v_, previous_type.clone()))
                };

                let v = v?;

                let mut target_type = v.1.clone();

                if type_equals(&v.1, &Type::Unknown) {
                    if let Some(Type::Array(inner)) = context {
                        target_type = *inner;
                    }
                }

                Ok(TypedExpression::Literal {
                    literal: ValueLiteral::Array {
                        items: v.0,
                        type_: target_type.clone(),
                    },
                    type_: Type::Array(Box::new(target_type)),
                })
            }
            ast::ValueLiteral::Struct {
                type_annotation,
                field_initializers,
            } => {
                // A literal names the struct but never its type arguments, so a
                // generic one would be checked against `T`. The expected type
                // carries the arguments, so it is preferred when it names the
                // same struct.
                let type_ = match instantiation_of(context.as_ref(), type_annotation) {
                    Some(instantiated) => instantiated,
                    None => type_environment
                        .borrow()
                        .get_type_from_annotation(type_annotation)?,
                };

                // With no expected type to take arguments from, the fields the
                // literal provides pin them down instead.
                let type_ = infer_struct_type_arguments(
                    type_,
                    field_initializers,
                    discovered_types,
                    type_environment.clone(),
                )?;

                let Type::Struct(Struct { fields, .. }) = type_.clone().unsubstitute() else {
                    Err(format!("{} is not a struct", type_.full_name()))?
                };

                // Each initializer is checked against the field it fills, so a
                // nested literal inherits the type arguments too.
                let mut field_initializers_: Vec<FieldInitializer> = vec![];

                for field_initializer in field_initializers {
                    let field_type = get_field_by_name(&fields, &field_initializer.identifier)
                        .map(|field| field.field_type.clone());

                    field_initializers_.push(FieldInitializer {
                        identifier: field_initializer.identifier.clone(),
                        initializer: check_type(
                            &field_initializer.initializer,
                            discovered_types,
                            type_environment.clone(),
                            field_type,
                        )?,
                    });
                }

                let mut field_initializers = field_initializers_;

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

                            let initializer = TypedExpression::Literal {
                                literal: lit,
                                type_: default_value.clone(),
                            };

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

                Ok(TypedExpression::Literal {
                    literal: ValueLiteral::Struct {
                        type_annotation: type_annotation.clone(),
                        field_initializers,
                        type_: type_.clone(),
                    },
                    type_,
                })
            }
            ast::ValueLiteral::Enum {
                type_annotation,
                member,
                field_initializers,
            } => {
                // As with structs, the literal names the variant but not the
                // enum's type arguments. The expected type supplies them, and
                // the variant is taken from the instantiated enum.
                let type_ = match instantiated_variant(context.as_ref(), type_annotation, member) {
                    Some(instantiated) => instantiated,
                    None => type_environment
                        .borrow()
                        .get_type_from_annotation(type_annotation)?,
                };

                let Type::Struct(Struct { fields, .. }) = &type_ else {
                    Err(format!(
                        "{} is not a member of {}",
                        member,
                        type_.full_name()
                    ))?
                };

                let mut checked_initializers = Vec::new();

                for ast::model::FieldInitializer {
                    identifier,
                    initializer,
                } in field_initializers
                {
                    let field_type =
                        get_field_by_name(fields, identifier).map(|field| field.field_type.clone());

                    checked_initializers.push(FieldInitializer {
                        identifier: identifier.clone(),
                        initializer: check_type(
                            initializer,
                            discovered_types,
                            type_environment.clone(),
                            field_type,
                        )?,
                    });
                }

                let mut field_initializers = checked_initializers;

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

                            let initializer = TypedExpression::Literal {
                                literal: lit,
                                type_: default_value.clone(),
                            };

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

                Ok(TypedExpression::Literal {
                    literal: ValueLiteral::Enum {
                        type_annotation: type_annotation.clone(),
                        member: member.clone(),
                        field_initializers,
                        type_: type_.clone(),
                    },
                    type_,
                })
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
        Expression::Use(UseExpr { args, expr }) => {
            let typed_expr = check_type(expr, discovered_types, type_environment, context)?;

            let expr_type = typed_expr.get_type();

            let Type::Function(function) = expr_type else {
                return Err("Right hand side of a use expression must be a function.".to_string());
            };

            let Some(Parameter { type_, .. }) = function.param else {
                return Err(
                    "Last argument of a function in a use expression must be a function"
                        .to_string(),
                );
            };

            let Type::Function(Function { param, .. }) = *type_ else {
                return Err(
                    "Last argument of a function in a use expression must be a function"
                        .to_string(),
                );
            };

            todo!()

            // let for_and_else_environment =
            //     Rc::new(RefCell::new(TypeEnvironment::new_parent(type_environment)));
            //
            // let for_environment = Rc::new(RefCell::new(TypeEnvironment::new_scope(
            //     for_and_else_environment.clone(),
            //     ScopeType::Break,
            // )));
            //
            // let iterable = check_type(
            //     iterable,
            //     discovered_types,
            //     for_and_else_environment.clone(),
            //     None,
            // )?;
            //
            // let Type::Array(inner_type) = iterable.get_type() else {
            //     return Err(format!(
            //         "For iterable must be of type array, found {}",
            //         iterable.get_type()
            //     ));
            // };
            //
            // check_type_pattern(
            //     pattern,
            //     Some(inner_type.as_ref()),
            //     for_environment.clone(),
            //     context.clone(),
            // )?;
            //
            // // for_environment
            // //     .borrow_mut()
            // //     .add_variable(identifier.clone(), *inner_type);
            //
            // let body = check_type(body, discovered_types, for_environment.clone(), None)?;
            //
            // let else_body = match else_body {
            //     Some(else_body) => Some(check_type(
            //         else_body,
            //         discovered_types,
            //         for_and_else_environment,
            //         None,
            //     )?),
            //     None => None,
            // };
            //
            // let mut type_ = for_environment
            //     .borrow()
            //     .get_scope(&ScopeType::Break)
            //     .map(|scope| scope.fold())
            //     .unwrap_or(Ok(Type::Void))?;
            //
            // match &else_body {
            //     Some(else_body) => {
            //         let else_type = else_body.get_type();
            //
            //         if !type_equals(&type_, &Type::Void)
            //             && !type_equals_unstrict(&type_, &else_body.get_type())
            //         {
            //             return Err(format!("For block breaks with value of type {} which does not match else blocks type {}", type_, else_body.get_type()));
            //         }
            //
            //         type_ = else_type
            //     }
            //     None => {
            //         if !type_equals(&type_, &Type::Void) {
            //             return Err(
            //                 "Must have an else block if the for block breaks with a value"
            //                     .to_string(),
            //             );
            //         }
            //     }
            // };
            //
            // Ok(TypedExpression::For {
            //     pattern: pattern.clone(),
            //     iterable: Box::new(iterable),
            //     body: Box::new(body),
            //     else_body: else_body.map(Box::new),
            //     type_: type_.clone(),
            // })
        }
    }
}

/// The expected type, when it is an instantiation of the type the annotation
/// names — `Foo<Int>` for a `Foo { .. }` literal.
///
/// A literal spells out the type's name but never its type arguments, so this
/// is where a generic one gets them from.
fn instantiation_of(expected: Option<&Type>, annotation: &TypeAnnotation) -> Option<Type> {
    let expected = expected?.clone().unsubstitute();

    let identifier = match &expected {
        Type::Struct(Struct {
            type_identifier, ..
        }) => type_identifier,
        Type::Enum(Enum {
            type_identifier, ..
        }) => type_identifier,
        _ => return None,
    };

    // Only an instantiation is useful here; the declaration itself still has
    // its parameters standing in for real types.
    if !matches!(identifier, TypeIdentifier::ConcreteType(_, _)) {
        return None;
    }

    if identifier.name() != annotation.name() {
        return None;
    }

    Some(expected)
}

/// Instantiates a generic function for a call that gave no explicit type
/// arguments, taking them from the argument it was called with and from where
/// its result is going.
///
/// Both directions are needed: a parameter mentioning `T` pins it from the
/// argument, while a `T` that appears only in the return type can be pinned
/// only by the expected type at the call site.
///
/// Returns `None` when the callee is not a generic function, when it was
/// already instantiated, or when neither direction pins down every type
/// parameter — in which case the call is checked as before and reports the
/// mismatch itself.
fn infer_call_type_arguments(
    callee_type: &Type,
    argument_type: Option<&Type>,
    expected_type: Option<&Type>,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<Option<(Type, TypeBindings)>, String> {
    let Type::Function(Function {
        identifier: Some(TypeIdentifier::GenericType(_, generics)),
        param,
        return_type,
    }) = callee_type
    else {
        return Ok(None);
    };

    let mut bindings = HashMap::new();

    // A literal argument pins the parameter to its runtime type: `id(1)` means
    // `id::<Int>(1)`, not `id::<#1>(1)`.
    if let (Some(param), Some(argument_type)) = (param, argument_type) {
        if contains_generic(&param.type_) {
            unify_type_argument(&param.type_, &widen_literals(argument_type), &mut bindings);
        }
    }

    // Whatever the argument left open, the expected type may settle. The
    // expectation is written out by hand, so it is taken as given rather than
    // widened.
    if let Some(expected_type) = expected_type {
        if contains_generic(return_type) {
            unify_type_argument(return_type, expected_type, &mut bindings);
        }
    }

    let concrete_types = generics
        .iter()
        .map(|generic| bindings.get(&generic.type_name).cloned())
        .collect::<Option<Vec<TypeAnnotation>>>();

    let Some(concrete_types) = concrete_types else {
        return Ok(None);
    };

    let instantiated = callee_type.clone_with_concrete_types(
        concrete_types,
        discovered_types,
        type_environment,
        None,
    )?;

    Ok(Some((instantiated, bindings)))
}

/// Builds the call for a member reached through an implementation written for
/// a bare type parameter — `p3:into()` where `into` comes from
/// `imp<T1, T2> Into<T2> for T1`.
///
/// The body is specialised to the type it is used on, since it may name the
/// implementation's parameters where a type belongs.
fn specialise_universal_member(
    object: &Expression,
    symbol: &str,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<Option<TypedExpression>, String> {
    let object = check_type(object, discovered_types, type_environment.clone(), None)?;
    let object_type = object.get_type();

    let source = type_environment
        .borrow()
        .universal_member_source(&object_type, symbol);

    let Some((declaration, bindings)) = source else {
        return Ok(None);
    };

    let specialised_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
        type_environment.clone(),
    )));

    // `Self` is the type the member was reached on.
    specialised_environment
        .borrow_mut()
        .add_type_alias("Self".to_owned(), object_type.clone());

    for (name, annotation) in &bindings {
        let argument =
            check_type_annotation(annotation, discovered_types, type_environment.clone())?;

        specialised_environment
            .borrow_mut()
            .add_type_alias(name.clone(), argument);
    }

    let declaration = ast::FunctionDeclaration {
        type_identifier: TypeIdentifier::Type(declaration.type_identifier.name().to_owned()),
        where_clause: vec![],
        ..declaration
    };

    let TypedStatement::FunctionDeclaration {
        param,
        return_type,
        body: Some(body),
        ..
    } = statements::check_type(
        &ast::Statement::FunctionDeclaration(declaration),
        discovered_types,
        specialised_environment,
    )?
    else {
        return Ok(None);
    };

    let closure_type = Type::Function(Function {
        identifier: None,
        param: param.as_ref().map(|param| Parameter {
            identifier: param.identifier.clone(),
            type_: param.type_.clone(),
        }),
        return_type: Box::new(return_type.clone()),
    });

    let callee = TypedExpression::Closure {
        param: param.map(|param| TypedClosureParameter {
            identifier: param.identifier,
            type_annotation: Some(param.type_annotation),
            type_: param.type_,
        }),
        return_type: return_type.clone(),
        body: Box::new(body),
        type_: closure_type,
    };

    Ok(Some(TypedExpression::Call {
        callee: Box::new(callee),
        argument: Some(Box::new(object)),
        type_: return_type,
    }))
}

/// What each of a generic's type parameters is bound to.
type TypeBindings = HashMap<String, TypeAnnotation>;

/// The type arguments written at a call, paired with the parameters they fill.
///
/// Returns `None` when none were written or the callee is not a generic
/// function whose body needs specialising.
fn written_type_arguments(
    callee: &Expression,
    type_environment: Rcrc<TypeEnvironment>,
) -> Option<TypeBindings> {
    let Expression::Member(ast::Member::Identifier {
        symbol,
        generics: Some(arguments),
    }) = callee
    else {
        return None;
    };

    let declaration = type_environment.borrow().get_generic_function(symbol)?;

    let TypeIdentifier::GenericType(_, parameters) = declaration.type_identifier else {
        return None;
    };

    Some(
        parameters
            .iter()
            .zip(arguments)
            .map(|(parameter, argument)| (parameter.type_name.clone(), argument.type_annotation()))
            .collect(),
    )
}

/// Builds a specialised copy of a generic function's body with its type
/// parameters replaced, as a closure to be called in place of the original.
///
/// A body like `T::show()` cannot run with `T` still standing for a parameter,
/// so each call gets its own copy checked against the types it was called with.
fn specialise_generic_call(
    callee: &TypedExpression,
    bindings: &TypeBindings,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<Option<TypedExpression>, String> {
    let TypedExpression::Member(Member::Identifier { symbol, .. }) = callee else {
        return Ok(None);
    };

    let Some(declaration) = type_environment.borrow().get_generic_function(symbol) else {
        return Ok(None);
    };

    // The copy is no longer generic: its parameters are bound below, and left
    // on the declaration they would be re-registered and shadow the bindings.
    // Its bounds were already checked at the call, so they come off too.
    let declaration = ast::FunctionDeclaration {
        type_identifier: TypeIdentifier::Type(declaration.type_identifier.name().to_owned()),
        where_clause: vec![],
        ..declaration
    };

    let specialised_environment = Rc::new(RefCell::new(TypeEnvironment::new_parent(
        type_environment.clone(),
    )));

    for (name, annotation) in bindings {
        let argument =
            check_type_annotation(annotation, discovered_types, type_environment.clone())?;

        specialised_environment
            .borrow_mut()
            .add_type_alias(name.clone(), argument);
    }

    let TypedStatement::FunctionDeclaration {
        param,
        return_type,
        body: Some(body),
        ..
    } = statements::check_type(
        &ast::Statement::FunctionDeclaration(declaration),
        discovered_types,
        specialised_environment,
    )?
    else {
        return Ok(None);
    };

    let closure_type = Type::Function(Function {
        identifier: None,
        param: param.as_ref().map(|param| Parameter {
            identifier: param.identifier.clone(),
            type_: param.type_.clone(),
        }),
        return_type: Box::new(return_type.clone()),
    });

    Ok(Some(TypedExpression::Closure {
        param: param.map(|param| TypedClosureParameter {
            identifier: param.identifier,
            type_annotation: Some(param.type_annotation),
            type_: param.type_,
        }),
        return_type,
        body: Box::new(body),
        type_: closure_type,
    }))
}

/// Instantiates a generic struct from the fields a literal provides, for a
/// literal with no expected type to take arguments from.
///
/// Returns the type unchanged when it is not a generic declaration or when the
/// fields do not pin down every type parameter.
fn infer_struct_type_arguments(
    type_: Type,
    field_initializers: &[ast::model::FieldInitializer],
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<Type, String> {
    let Type::Struct(Struct {
        type_identifier: TypeIdentifier::GenericType(_, generics),
        fields,
        ..
    }) = &type_
    else {
        return Ok(type_);
    };

    let (generics, fields) = (generics.clone(), fields.clone());
    let mut bindings = HashMap::new();

    for initializer in field_initializers {
        let Some(field) = get_field_by_name(&fields, &initializer.identifier) else {
            continue;
        };

        if !contains_generic(&field.field_type) {
            continue;
        }

        let checked = check_type(
            &initializer.initializer,
            discovered_types,
            type_environment.clone(),
            None,
        )?;

        unify_type_argument(
            &field.field_type,
            &widen_literals(&checked.get_type()),
            &mut bindings,
        );
    }

    let concrete_types = generics
        .iter()
        .map(|generic| bindings.get(&generic.type_name).cloned())
        .collect::<Option<Vec<TypeAnnotation>>>();

    let Some(concrete_types) = concrete_types else {
        return Ok(type_);
    };

    type_.clone_with_concrete_types(concrete_types, discovered_types, type_environment, None)
}

/// Widens literal types to their runtime type, reaching inside arrays and
/// tuples.
///
/// Inferring from an argument takes the runtime type — `id(1)` means
/// `id::<Int>(1)`, not `id::<#1>(1)` — and that has to hold just as much for the
/// `Int` inside a `[Int]`.
fn widen_literals(type_: &Type) -> Type {
    match type_ {
        Type::Array(inner) => Type::Array(Box::new(widen_literals(inner))),
        Type::Tuple(types) => Type::Tuple(types.iter().map(widen_literals).collect()),
        other => runtime_type(other),
    }
}

/// Matches a parameter type against an argument type, recording what each type
/// parameter would have to be. The first binding wins.
fn unify_type_argument(
    parameter: &Type,
    argument: &Type,
    bindings: &mut HashMap<String, TypeAnnotation>,
) {
    match (parameter, argument) {
        (Type::Generic(generic), argument) => {
            bindings
                .entry(generic.type_name.clone())
                .or_insert_with(|| argument.type_annotation());
        }
        (Type::Array(parameter), Type::Array(argument)) => {
            unify_type_argument(parameter, argument, bindings)
        }
        (Type::Tuple(parameters), Type::Tuple(arguments)) => {
            for (parameter, argument) in parameters.iter().zip(arguments) {
                unify_type_argument(parameter, argument, bindings);
            }
        }
        _ => {}
    }
}

/// Replaces the type on a checked expression, for when instantiation settles it
/// after the expression was built.
fn retype(expression: TypedExpression, type_: Type) -> TypedExpression {
    match expression {
        TypedExpression::Member(Member::Identifier { symbol, .. }) => {
            TypedExpression::Member(Member::Identifier { symbol, type_ })
        }
        other => other,
    }
}

/// The variant of the expected enum that an enum literal names, when the
/// expected type is an instantiation of that enum — the `Res<Int>::Ok` for a
/// `Res::Ok { .. }` literal expected to be a `Res<Int>`.
fn instantiated_variant(
    expected: Option<&Type>,
    annotation: &TypeAnnotation,
    member: &str,
) -> Option<Type> {
    let Type::Enum(Enum {
        type_identifier,
        members,
        ..
    }) = expected?.clone().unsubstitute()
    else {
        return None;
    };

    if !matches!(type_identifier, TypeIdentifier::ConcreteType(_, _)) {
        return None;
    }

    // The literal's annotation is the qualified variant, `Res::Ok`.
    let annotation_name = annotation.name();
    let (enum_name, _) = annotation_name.rsplit_once("::")?;

    if enum_name != type_identifier.name() {
        return None;
    }

    get_enum_member(&members, &type_identifier, member).cloned()
}

/// Rejects a type argument list that doesn't match what the type declares,
/// which would otherwise be silently truncated during substitution.
fn check_type_argument_count(type_: &Type, given: usize, symbol: &str) -> Result<(), String> {
    let declared = match type_ {
        Type::Function(Function {
            identifier: Some(TypeIdentifier::GenericType(_, generics)),
            ..
        }) => generics.len(),
        Type::Struct(Struct {
            type_identifier: TypeIdentifier::GenericType(_, generics),
            ..
        }) => generics.len(),
        Type::Enum(Enum {
            type_identifier: TypeIdentifier::GenericType(_, generics),
            ..
        }) => generics.len(),
        Type::TypeAlias(TypeAlias {
            type_identifier: TypeIdentifier::GenericType(_, generics),
            ..
        }) => generics.len(),
        // Not generic at all, so any type argument is one too many.
        _ => 0,
    };

    if declared == given {
        return Ok(());
    }

    Err(format!(
        "`{}` takes {} type argument{}, but {} {} given",
        symbol,
        declared,
        if declared == 1 { "" } else { "s" },
        given,
        if given == 1 { "was" } else { "were" }
    ))
}

const TYPEOF_IS_NOT_A_VALUE: &str =
    "`typeof` is resolved while type checking, so it cannot be used as a value at runtime";

/// Resolves a call to a static member that several implementations provide,
/// choosing by the argument's type.
///
/// Returns `None` when the member is not overloaded, leaving the call to the
/// ordinary path.
fn check_overloaded_static_call(
    type_annotation: &TypeAnnotation,
    symbol: &str,
    argument: &Expression,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<Option<TypedExpression>, String> {
    let object_type =
        check_type_annotation(type_annotation, discovered_types, type_environment.clone())?;

    let candidates = type_environment
        .borrow()
        .get_static_member_candidates(&object_type, symbol);

    if candidates.len() < 2 {
        return Ok(None);
    }

    let argument = check_type(argument, discovered_types, type_environment.clone(), None)?;
    let argument_type = argument.get_type();

    let matching = candidates
        .iter()
        .filter(|candidate| match candidate {
            Type::Function(Function {
                param: Some(param), ..
            }) => type_equals(&param.type_, &argument_type),
            _ => false,
        })
        .collect::<Vec<_>>();

    let [member_type] = matching.as_slice() else {
        return Err(format!(
            "{} of `{}` {} for an argument of type {}",
            symbol,
            object_type.full_name(),
            if matching.is_empty() {
                "has no implementation"
            } else {
                "has more than one implementation"
            },
            argument_type
        ));
    };

    let Type::Function(Function {
        param, return_type, ..
    }) = member_type
    else {
        return Err(format!("{} is not a function", symbol));
    };

    // The chosen candidate is named so that evaluation reaches the same one
    // rather than whichever was registered last under the plain name.
    let resolved = overloaded_member_name(symbol, param.as_ref().map(|p| p.type_.as_ref()));

    let callee = TypedExpression::Member(Member::StaticMemberAccess {
        type_annotation: type_annotation.clone(),
        member: Box::new(Member::Identifier {
            symbol: resolved.clone(),
            type_: (*member_type).clone(),
        }),
        symbol: resolved,
        type_: (*member_type).clone(),
    });

    Ok(Some(TypedExpression::Call {
        callee: Box::new(callee),
        argument: Some(Box::new(argument)),
        type_: *return_type.clone(),
    }))
}

/// Whether an expression uses one of these type parameters where a type
/// belongs — `T::show()` — which is the case a running program cannot serve
/// without the parameter having been substituted first.
pub fn dispatches_on_type_parameter(
    expression: &Expression,
    generics: &[crate::types::GenericType],
) -> bool {
    let names_a_parameter = |annotation: &TypeAnnotation| {
        generics
            .iter()
            .any(|generic| generic.type_name == annotation.name())
    };

    match expression {
        Expression::Member(ast::Member::StaticMemberAccess {
            type_annotation, ..
        }) => names_a_parameter(type_annotation),
        Expression::Member(ast::Member::MemberAccess { object, .. }) => {
            dispatches_on_type_parameter(object, generics)
        }
        Expression::Member(ast::Member::ParamPropagation { object, .. }) => {
            dispatches_on_type_parameter(object, generics)
        }
        Expression::Call(call) => {
            dispatches_on_type_parameter(&call.callee, generics)
                || call
                    .argument
                    .as_ref()
                    .is_some_and(|argument| dispatches_on_type_parameter(argument, generics))
        }
        Expression::Block(statements) => statements
            .iter()
            .any(|statement| statement_dispatches_on_type_parameter(statement, generics)),
        Expression::Binary(binary) => {
            dispatches_on_type_parameter(&binary.left, generics)
                || dispatches_on_type_parameter(&binary.right, generics)
        }
        Expression::Unary(unary) => dispatches_on_type_parameter(&unary.expression, generics),
        _ => false,
    }
}

/// As above, for a statement inside a block.
fn statement_dispatches_on_type_parameter(
    statement: &ast::Statement,
    generics: &[crate::types::GenericType],
) -> bool {
    match statement {
        ast::Statement::Expression(expression) => {
            dispatches_on_type_parameter(expression, generics)
        }
        ast::Statement::Semi(statement) => {
            statement_dispatches_on_type_parameter(statement, generics)
        }
        _ => false,
    }
}

/// Whether a member names the `typeof` built-in.
fn is_typeof(member: &ast::Member) -> bool {
    let ast::Member::Identifier { symbol, .. } = member else {
        return false;
    };

    BuiltInFunction::new(symbol).is_some_and(|f| f.function_type == BuiltInFunctionType::TypeOf)
}

/// The answer `typeof` gives for an expression: its static type, rendered, as a
/// string literal carrying that same text as its type.
fn typeof_literal(argument: &TypedExpression) -> TypedExpression {
    let name = argument.get_type().to_string();

    TypedExpression::Literal {
        literal: ValueLiteral::String(name.clone()),
        type_: Type::string_literal(name),
    }
}

/// The `T` in an `Option<T>`.
pub fn option_inner(type_: &Type) -> Option<Type> {
    if !is_option(type_) {
        return None;
    }

    let Type::Enum(Enum { members, .. }) = type_ else {
        return None;
    };

    let Type::Struct(Struct { fields, .. }) = members.get("Some")? else {
        return None;
    };

    Some(get_field_by_name(fields, "value")?.field_type.clone())
}

pub fn is_option(type_: &Type) -> bool {
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

                get_field_by_name(fields, "value").is_some()
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
            // A bound puts its protocol's functions on the type parameter, so
            // `T::from(..)` is reachable inside a `where T is From<..>`.
            Type::Generic(generic) => {
                let Some(static_member_type) = type_environment
                    .borrow()
                    .get_static_member(&object_type, &symbol)
                else {
                    return Err(format!(
                        "Type parameter '{}' has no bound providing '{}'",
                        generic.type_name, symbol
                    ));
                };

                Ok(TypedExpression::Member(Member::StaticMemberAccess {
                    type_annotation: object_type.type_annotation(),
                    member: Box::new(Member::Identifier {
                        symbol: symbol.clone(),
                        type_: static_member_type.clone(),
                    }),
                    symbol: symbol.clone(),
                    type_: static_member_type,
                }))
            }
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
                    type_annotation: object_type.type_annotation(),
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
                    type_annotation: object_type.type_annotation(),
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
        object_type,
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
                    symbol,
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
                    symbol,
                    type_: field_type.clone(),
                }))
            }
            Type::Array(element_type) => {
                let element_access_expression = check_type_member_access_recurse(
                    *element_type,
                    member,
                    type_environment,
                    object_typed_expression.clone(),
                    discovered_types,
                    context,
                )?;

                let field_type = Type::Array(Box::new(element_access_expression.get_type()));

                Ok(TypedExpression::Member(Member::MemberAccess {
                    object: Box::new(object_typed_expression),
                    member: Box::new(Member::Identifier {
                        symbol: symbol.clone(),
                        type_: field_type.clone(),
                    }),
                    symbol,
                    type_: field_type,
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

    // `x:typeof()` is folded by the caller. Reaching here means the call was
    // left off, which would leave `typeof` standing as a value.
    if is_typeof(member) {
        return Err(format!(
            "{}, so `:typeof` must be called",
            TYPEOF_IS_NOT_A_VALUE
        ));
    }

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
    let known_type = context.or_else(|| initializer_type.cloned());

    // Without a type there is nothing to resolve the pattern against, so its
    // names are introduced untyped, as they were before.
    let Some(known_type) = known_type else {
        for identifier in pattern.bindings() {
            type_environment
                .borrow_mut()
                .add_variable(identifier, Type::Unknown);
        }

        return Ok(());
    };

    let (checked, bindings) = check_pattern(pattern, &known_type, type_environment.clone())?;

    // Declarations and loops bind unconditionally, so a pattern that can fail
    // has nowhere to fail to.
    if is_refutable(&checked) {
        return Err(format!("Pattern `{}` is refutable", pattern));
    }

    for (identifier, binding_type) in bindings {
        type_environment
            .borrow_mut()
            .add_variable(identifier, binding_type);
    }

    Ok(())
}

fn is_refutable(pattern: &CheckedPattern) -> bool {
    match pattern {
        CheckedPattern::Wildcard | CheckedPattern::Binding(_) => false,
        CheckedPattern::Tuple(patterns) => patterns.iter().any(is_refutable),
        CheckedPattern::Fields(fields) => fields.iter().any(|f| is_refutable(&f.pattern)),
        _ => true,
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
        (UnaryOperator::LogicalNot, Type::Literal { type_, .. }) => match **type_ {
            LiteralType::Bool => Ok(Type::Literal {
                name: "Bool".to_string(),
                type_: type_.clone(),
            }),
            LiteralType::BoolValue(value) => Ok(Type::bool_literal(!value)),
            _ => Err(format!(
                "Invalid unary operator {:?} for type {}",
                operator, operand
            )),
        },
        (UnaryOperator::BitwiseNot, Type::Int) => Ok(Type::Int),
        (UnaryOperator::BitwiseNot, Type::UInt) => Ok(Type::UInt),
        (UnaryOperator::BitwiseNot, Type::Literal { type_, .. }) => match **type_ {
            LiteralType::Int | LiteralType::UInt => Ok(Type::Literal {
                name: operand.to_string(),
                type_: type_.clone(),
            }),
            LiteralType::IntValue(value) => Ok(Type::int_literal(!value)),
            LiteralType::UIntValue(value) => Ok(Type::uint_literal(!value)),
            _ => Err(format!(
                "Invalid unary operator {:?} for type {}",
                operator, operand
            )),
        },
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
        (Type::Rune, BinaryOperator::Add, Type::Rune) => Ok(Type::String),
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
        (Type::Rune, BinaryOperator::Equal, Type::Rune) => Ok(Type::Bool),
        (Type::Bool, BinaryOperator::Equal, Type::Bool) => Ok(Type::Bool),
        (Type::Unit, BinaryOperator::Equal, Type::Unit) => Ok(Type::Bool),
        (Type::Int, BinaryOperator::NotEqual, Type::Int) => Ok(Type::Bool),
        (Type::UInt, BinaryOperator::NotEqual, Type::UInt) => Ok(Type::Bool),
        (Type::Float, BinaryOperator::NotEqual, Type::Float) => Ok(Type::Bool),
        (Type::String, BinaryOperator::NotEqual, Type::String) => Ok(Type::Bool),
        (Type::Rune, BinaryOperator::NotEqual, Type::Rune) => Ok(Type::Bool),
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
        (Type::Rune, BinaryOperator::Range, Type::Rune) => Ok(Type::Array(Box::new(Type::Rune))),
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
        (Type::Rune, BinaryOperator::RangeInclusive, Type::Rune) => {
            Ok(Type::Array(Box::new(Type::Rune)))
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
