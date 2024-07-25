use std::{cell::RefCell, ops::Deref, rc::Rc};

use shared::{
    type_checker::{
        ast::{Block, *},
        Type,
    },
    types::TypeIdentifier,
};

use crate::scope::Scope;

use super::{
    environment::{Environment, Rcrc},
    evaluate_binop,
    scope::ScopeType,
    value::{EnumFields, EnumMember, Number, Value},
};

pub fn evaluate<'a>(
    typed_statement: TypedStatement,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    match typed_statement {
        TypedStatement::None => Ok(Value::Void),
        TypedStatement::StructDeclaration { .. } => Ok(Value::Void),
        TypedStatement::EnumDeclaration { .. } => Ok(Value::Void),
        TypedStatement::UnionDeclaration { .. } => Ok(Value::Void),
        TypedStatement::Program { statements } => evaluate_program(statements, environment),
        TypedStatement::FunctionDeclaration {
            identifier,
            param,
            return_type: _,
            body,
            type_: _,
        } => evaluate_function_declaration(environment, identifier, param, body),
        TypedStatement::Semi(s) => {
            evaluate(*s, environment)?;
            Ok(Value::Void)
        }
        TypedStatement::Break(e) => evaluate_break(e, Type::Void, environment),
        TypedStatement::Continue => evaluate_continue(environment),
        TypedStatement::Return(e) => evaluate_return(e, Type::Void, environment),
        TypedStatement::Expression(e) => evaluate_expression(e, environment),
        TypedStatement::Print(e) => {
            let value = evaluate_expression(e, environment)?;
            println!("{}", value);
            Ok(Value::Void)
        }
    }
}

fn evaluate_function_declaration(
    environment: Rc<RefCell<Environment>>,
    identifier: TypeIdentifier,
    param: Option<TypedParameter>,
    body: TypedExpression,
) -> Result<Value, String> {
    let function_environment = Rc::new(RefCell::new(environment.deref().clone().borrow().clone()));

    let function = Value::Function {
        param_name: param.map(|p| p.identifier),
        body,
        environment: function_environment.clone(),
    };

    function_environment
        .borrow_mut()
        .add_function(identifier.to_string(), function.clone(), false);

    environment
        .borrow_mut()
        .add_function(identifier.to_string(), function, false);

    Ok(Value::Void)
}

fn evaluate_expression<'a>(
    typed_expression: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    match typed_expression {
        // TypedExpression::None => Ok(Value::Void),
        TypedExpression::VariableDeclaration {
            mutable,
            identifier,
            initializer,
            type_,
        } => evaluate_variable_declaration(mutable, identifier, initializer, type_, environment),
        TypedExpression::If {
            condition,
            true_expression,
            false_expression,
            type_,
        } => evaluate_if(
            condition,
            true_expression,
            false_expression,
            type_,
            environment,
        ),
        TypedExpression::Match {
            expression, arms, ..
        } => evaluate_match(expression, arms, environment),
        TypedExpression::Assignment {
            member,
            initializer,
            type_,
        } => evaluate_assignment(member, initializer, environment),
        TypedExpression::Member(m) => evaluate_member(m, environment),
        TypedExpression::Literal(l) => evaluate_literal(l, environment),
        TypedExpression::Closure {
            param,
            return_type: _,
            body,
            type_: _,
        } => evaluate_closure(param, *body, environment),
        TypedExpression::Call {
            callee,
            argument,
            type_,
        } => evaluate_call(callee, argument, type_, environment),
        TypedExpression::Index {
            callee,
            argument,
            type_,
        } => evaluate_index(callee, argument, type_, environment),
        TypedExpression::Unary {
            operator,
            expression,
            type_,
        } => evaluate_unary(operator, expression, type_, environment),
        TypedExpression::Binary {
            left,
            operator,
            right,
            type_,
        } => evaluate_binary(left, operator, right, type_, environment),
        TypedExpression::Block(Block { statements, type_ }) => {
            evaluate_block(statements, type_, environment)
        }
        TypedExpression::Drop { identifier, type_ } => {
            evaluate_drop(identifier, type_, environment)
        }
        TypedExpression::Loop(Block {
            statements,
            type_: _,
        }) => evaluate_loop(statements, environment),
        TypedExpression::While {
            condition,
            block,
            else_block,
            type_,
        } => evaluate_while(condition, block, else_block, type_, environment),
    }
}

fn evaluate_program<'a>(
    statements: Vec<TypedStatement>,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let mut value = Value::Void;
    for statement in statements {
        value = evaluate(statement, environment.clone())?;

        if environment.borrow().get_scope(&ScopeType::Return).is_some() {
            break;
        }
    }
    Ok(value)
}

fn evaluate_variable_declaration<'a>(
    mutable: bool,
    identifier: String,
    initializer: Option<Box<TypedExpression>>,
    _type_: Type,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let value = match initializer {
        Some(initializer) => evaluate_expression(*initializer, environment.clone())?,
        None => Value::Uninitialized,
    };
    environment
        .borrow_mut()
        .add_variable(identifier, value.clone(), mutable);
    Ok(value)
}

fn evaluate_if<'a>(
    condition: Box<TypedExpression>,
    true_expression: Box<TypedExpression>,
    false_expression: Option<Box<TypedExpression>>,
    _type_: Type,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let if_environment = Rc::new(RefCell::new(Environment::new_parent(environment.clone())));
    let condition = evaluate_expression(*condition, if_environment.clone())?;

    match condition {
        Value::Bool(v) => {
            if v {
                let value = evaluate_expression(*true_expression, if_environment)?;
                if false_expression.is_none() {
                    return Ok(Value::option_some(value));
                } else {
                    return Ok(value);
                }
            } else {
                match false_expression {
                    Some(false_expression) => {
                        evaluate_expression(*false_expression, if_environment)
                    }
                    None => Ok(Value::option_none()),
                }
            }
        }
        _ => Err(format!("If condition must be boolean '{}'", condition)),
    }
}

fn evaluate_match(
    expression: Box<TypedExpression>,
    arms: Vec<TypedMatchArm>,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let match_environment = Rc::new(RefCell::new(Environment::new_parent(environment.clone())));
    let value = evaluate_expression(*expression, match_environment.clone())?;

    for arm in arms {
        let arm_environment = Rc::new(RefCell::new(Environment::new_parent(
            match_environment.clone(),
        )));

        match arm.pattern {
            Pattern::Wildcard => {
                return evaluate_expression(arm.expression, arm_environment);
            }
            Pattern::Int(v) => {
                let Value::Number(Number::Int(value)) = value else {
                    return Err(format!("Expected integer, found '{}'", value));
                };

                if value == v {
                    return evaluate_expression(arm.expression, arm_environment);
                }
            }
        }
    }

    Err(format!("No match arm found for value '{}'", value))
}

fn evaluate_assignment<'a>(
    member: Box<Member>,
    initializer: Box<TypedExpression>,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let value = evaluate_expression(*initializer, environment.clone())?;
    environment
        .borrow_mut()
        .set_variable(*member, value.clone())?;
    Ok(value)
}

fn evaluate_member<'a>(member: Member, environment: Rcrc<Environment>) -> Result<Value, String> {
    match member {
        Member::Identifier { symbol, type_: _ } => Ok(environment
            .borrow()
            .get_variable(&symbol)
            .or(environment.borrow().get_function(&symbol))
            .ok_or(format!("Variable '{}' not found", symbol))?
            .borrow()
            .value
            .clone()),
        Member::MemberAccess { object, member, .. } => {
            evaluate_member_access(object, environment, member)
        } // Member::MemberFunctionAccess { object, member, .. } => {
          //     evaluate_member_function_access(object, environment, member)
          // }
    }
}

fn evaluate_member_access<'a>(
    object: Box<TypedExpression>,
    environment: Rcrc<Environment>,
    member: Box<Member>,
) -> Result<Value, String> {
    let value = evaluate_expression(*object, environment.clone())?;

    match value {
        Value::Struct {
            struct_name,
            fields,
        } => match *member.clone() {
            Member::Identifier { symbol, type_: _ } => {
                let field_value = fields.get(&symbol).ok_or(format!(
                    "Field '{}' not found in struct '{}'",
                    symbol, struct_name
                ))?;

                Ok(field_value.clone())
            }
            Member::MemberAccess { object, member, .. } => {
                evaluate_member_access(object, environment, member)
            } // Member::MemberFunctionAccess { object, member, .. } => {
              //     evaluate_member_function_access(object, environment, member)
              // }
        },
        _ => Err(format!(
            "Cannot access member of non-struct value '{}'",
            value
        )),
    }
}

// fn evaluate_member_function_access<'a>(
//     object: Box<TypedExpression>,
//     environment: Rcrc<Environment>,
//     member: Box<Member>,
// ) -> Result<Value, String> {
//     let value = evaluate_expression(*object, environment.clone())?;
//
//     match value {
//         Value::Struct {
//             struct_name,
//             fields,
//         } => match *member.clone() {
//             Member::Identifier { symbol, type_: _ } => {
//                 let field_value = fields.get(&symbol).ok_or(format!(
//                     "Field '{}' not found in struct '{}'",
//                     symbol, struct_name
//                 ))?;
//
//                 Ok(field_value.clone())
//             }
//             Member::MemberAccess { object, member, .. } => {
//                 evaluate_member_access(object, environment, member)
//             }
//             Member::MemberFunctionAccess { object, member, .. } => {
//                 evaluate_member_function_access(object, environment, member)
//             }
//         },
//         _ => {
//             let Member::Identifier { symbol, .. } = *member else {
//                 return Err(format!("Expected identifier, found '{}'", member));
//             };
//
//             let function_value = environment
//                 .borrow()
//                 .get_variable(&symbol)
//                 .or_else(|| environment.borrow().get_function(&symbol))
//                 .ok_or(format!("Variable '{}' not found in env", symbol))?;
//
//             let new_function_value = match function_value.borrow().value.clone() {
//                 Value::Function {
//                     param_name,
//                     body,
//                     environment,
//                 } => Value::Function {
//                     param_name,
//                     body,
//                     environment: Rc::new(RefCell::new(Environment::new_parent(environment))),
//                 },
//                 _ => return Err(format!("Expected function, found '{}'", function_value)),
//             };
//             Ok(function_value.clone().borrow().value.clone())
//         }
//     }
// }

fn evaluate_literal<'a>(literal: Literal, environment: Rcrc<Environment>) -> Result<Value, String> {
    match literal {
        Literal::Void => panic!("Void literals should never be evaluated"),
        Literal::Unit => Ok(Value::Unit),
        Literal::Int(v) => Ok(Value::Number(Number::Int(v))),
        Literal::UInt(v) => Ok(Value::Number(Number::UInt(v))),
        Literal::Float(v) => Ok(Value::Number(Number::Float(v))),
        Literal::String(v) => Ok(Value::String(v)),
        Literal::Char(v) => Ok(Value::Char(v)),
        Literal::Bool(v) => Ok(Value::Bool(v)),
        Literal::Array { values, type_: _ } => {
            let mut array = Vec::new();

            for element in values {
                array.push(evaluate_expression(element, environment.clone())?);
            }

            Ok(Value::Array(array))
        }
        Literal::Range {
            start,
            end,
            inclusive,
            type_: _,
        } => {
            let start = evaluate_expression(*start, environment.clone())?;
            let end = evaluate_expression(*end, environment.clone())?;

            match (start, end) {
                (Value::Number(Number::Int(start)), Value::Number(Number::Int(end))) => {
                    let mut array = Vec::new();

                    if inclusive {
                        for i in start..=end {
                            array.push(Value::Number(Number::Int(i)));
                        }
                    } else {
                        for i in start..end {
                            array.push(Value::Number(Number::Int(i)));
                        }
                    }

                    Ok(Value::Array(array))
                }
                (Value::Number(Number::UInt(start)), Value::Number(Number::UInt(end))) => {
                    let mut array = Vec::new();

                    if inclusive {
                        for i in start..=end {
                            array.push(Value::Number(Number::UInt(i)));
                        }
                    } else {
                        for i in start..end {
                            array.push(Value::Number(Number::UInt(i)));
                        }
                    }

                    Ok(Value::Array(array))
                }
                (Value::Char(start), Value::Char(end)) => {
                    let mut array = Vec::new();

                    if inclusive {
                        for i in start..=end {
                            array.push(Value::Char(i));
                        }
                    } else {
                        for i in start..end {
                            array.push(Value::Char(i));
                        }
                    }

                    Ok(Value::Array(array))
                }
                (s, e) => Err(format!(
                    "Range start and end must be unsigned integers, found '{}' and '{}'",
                    s, e
                )),
            }
        }
        Literal::Struct {
            type_annotation,
            field_initializers,
            type_: _,
        } => {
            let mut fields = std::collections::HashMap::new();

            for field_initializer in field_initializers {
                fields.insert(
                    field_initializer.identifier.unwrap(),
                    evaluate_expression(field_initializer.initializer, environment.clone())?,
                );
            }

            Ok(Value::Struct {
                struct_name: type_annotation,
                fields,
            })
        }
        Literal::Enum {
            type_annotation,
            member,
            field_initializers,
            type_: _,
        } => {
            let fields: EnumFields = match field_initializers {
                EnumMemberFieldInitializers::None => EnumFields::None,
                EnumMemberFieldInitializers::Named(field_initializers) => {
                    let mut fields = std::collections::HashMap::new();

                    for (identifier, initializer) in field_initializers {
                        fields.insert(
                            identifier,
                            evaluate_expression(initializer, environment.clone())?,
                        );
                    }

                    EnumFields::Named(fields)
                }
            };

            Ok(Value::Enum {
                enum_member: EnumMember {
                    enum_name: type_annotation,
                    member_name: member,
                },
                fields,
            })
        }
    }
}

fn evaluate_closure(
    param: Option<TypedClosureParameter>,
    body: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    Ok(Value::Function {
        param_name: param.map(|p| p.identifier),
        body,
        environment,
    })
}

fn evaluate_call(
    callee: Box<TypedExpression>,
    argument: Option<Box<TypedExpression>>,
    type_: Type,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let callee_value = evaluate_expression(*callee, environment.clone())?;

    let evaluated_arg = argument
        .map(|arg| evaluate_expression(*arg, environment.clone()))
        .transpose()?;

    match callee_value {
        Value::Function {
            param_name,
            body,
            environment,
        } => {
            let function_environment = Rc::new(RefCell::new(Environment::new_scope(
                environment.clone(),
                ScopeType::Return,
            )));

            match evaluated_arg {
                Some(evaluated_arg) => {
                    function_environment.borrow_mut().add_variable(
                        param_name.clone().unwrap(),
                        evaluated_arg.clone(),
                        false,
                    );
                }
                None => (),
            }

            let mut value = evaluate_expression(body, function_environment.clone())?;

            if let Some(Scope::Return(v)) =
                function_environment.borrow().get_scope(&ScopeType::Return)
            {
                match v {
                    Some(v) => {
                        if type_ == Type::Void {
                            return Err(format!("Cannot return a value from a void function",));
                        }

                        value = v.clone();
                    }
                    None => {
                        if type_ != Type::Void {
                            return Err(format!(
                                "Cannot return void from a non-void function. Expected type '{}', found type 'void'",
                                type_
                            ));
                        }

                        value = Value::Void;
                    }
                }
            }

            if type_ == Type::Void {
                return Ok(Value::Void);
            }

            Ok(value)
        }
        _ => Err(format!("Cannot call non-function value '{}'", callee_value)),
    }
}

fn evaluate_index(
    callee: Box<TypedExpression>,
    argument: Box<TypedExpression>,
    _type_: Type,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let callee_value = evaluate_expression(*callee, environment.clone())?;

    let evaluated_argument = evaluate_expression(*argument, environment.clone())?;

    let Value::Number(Number::UInt(index)) = evaluated_argument else {
        unreachable!("Type is known after type checking, this should never happen")
    };

    match callee_value {
        Value::Array(values) => {
            let value = values
                .get(index as usize)
                .cloned()
                .ok_or(format!("Index out of bounds '{}'", index))?;

            Ok(value)
        }
        _ => Err(format!("Cannot index non-array value '{}'", callee_value)),
    }
}

fn evaluate_unary<'a>(
    operator: UnaryOperator,
    expression: Box<TypedExpression>,
    _type_: Type,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let value = evaluate_expression(*expression, environment)?;

    match operator {
        UnaryOperator::Identity => match value {
            Value::Number(number) => Ok(Value::Number(number)),
            _ => Err(format!(
                "Cannot apply unary operator '+' to non-number value '{}'",
                value
            )),
        },
        UnaryOperator::Negate => match value {
            Value::Number(number) => match number {
                Number::Int(v) => Ok(Value::Number(Number::Int(-v))),
                Number::Float(v) => Ok(Value::Number(Number::Float(-v))),
                other => Err(format!(
                    "Cannot apply unary operator '-' to unsigned integers '{}'",
                    other
                )),
            },
            _ => Err(format!(
                "Cannot apply unary operator '-' to non-number value '{}'",
                value
            )),
        },
        UnaryOperator::BitwiseNot => match value {
            Value::Number(number) => match number {
                Number::Int(v) => Ok(Value::Number(Number::Int(!v))),
                Number::UInt(v) => Ok(Value::Number(Number::UInt(!v))),
                other => Err(format!(
                    "Cannot apply unary operator '~' to floating point numbers '{}'",
                    other
                )),
            },
            _ => Err(format!(
                "Cannot apply unary operator '~' to non-number value '{}'",
                value
            )),
        },
        UnaryOperator::LogicalNot => match value {
            Value::Bool(v) => Ok(Value::Bool(!v)),
            _ => Err(format!(
                "Cannot apply unary operator '!' to non-boolean value '{}'",
                value
            )),
        },
    }
}

fn evaluate_binary<'a>(
    left: Box<TypedExpression>,
    operator: BinaryOperator,
    right: Box<TypedExpression>,
    _type_: Type,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(*left, environment.clone())?;
    let right = evaluate_expression(*right, environment)?;

    evaluate_binop::evaluate_binop(left, operator, right)
}

fn evaluate_block<'a>(
    statements: Vec<TypedStatement>,
    _type_: Type,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let block_environment = Rc::new(RefCell::new(Environment::new_parent(environment)));
    let mut value = Value::Void;

    for statement in statements {
        value = evaluate(statement, block_environment.clone())?;

        if block_environment
            .borrow()
            .get_scope(&ScopeType::Return)
            .is_some()
        {
            value = Value::Void;
            break;
        }
    }

    Ok(value)
}

fn evaluate_drop<'a>(
    identifier: String,
    _type_: Type,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let variable = environment
        .borrow_mut()
        .remove_variable(&identifier)
        .ok_or(format!("Variable '{}' not found", identifier))?;

    let value = variable.borrow().value.clone();
    Ok(value)
}

fn evaluate_loop(
    statements: Vec<TypedStatement>,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let loop_environment = Rc::new(RefCell::new(Environment::new_scopes(
        environment,
        [Scope::Break(None), Scope::Continue],
    )));

    let break_value;

    'outer: loop {
        for statement in statements.clone() {
            evaluate(statement, loop_environment.clone())?;

            if let Some(Scope::Break(v)) = loop_environment.borrow().get_scope(&ScopeType::Break) {
                break_value = match v {
                    Some(v) => v.clone(),
                    None => Value::Void,
                };

                break 'outer;
            }

            if let Some(Scope::Continue) = loop_environment.borrow().get_scope(&ScopeType::Continue)
            {
                continue 'outer;
            }

            if loop_environment
                .borrow()
                .get_scope(&ScopeType::Return)
                .is_some()
            {
                break_value = Value::Void;
                break 'outer;
            }
        }
    }

    Ok(break_value)
}

fn evaluate_while(
    condition: Box<TypedExpression>,
    statements: Vec<TypedStatement>,
    else_statements: Option<Vec<TypedStatement>>,
    _type_: Type,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let while_environment = Rc::new(RefCell::new(Environment::new_scopes(
        environment,
        [ScopeType::Break, ScopeType::Continue],
    )));

    let break_value;

    'outer: loop {
        let value = evaluate_expression(*condition.clone(), while_environment.clone())?;

        match value {
            Value::Bool(v) => {
                if !v {
                    break_value = match else_statements {
                        Some(else_statements) => {
                            let mut value = Value::Void;

                            for statement in else_statements {
                                value = evaluate(statement, while_environment.clone())?;

                                if while_environment
                                    .borrow()
                                    .get_scope(&ScopeType::Return)
                                    .is_some()
                                {
                                    value = Value::Void;
                                    break;
                                }
                            }

                            value
                        }
                        None => Value::Void,
                    };

                    break 'outer;
                }
            }
            _ => return Err(format!("While condition must be boolean '{}'", value)),
        }

        for statement in statements.clone() {
            evaluate(statement, while_environment.clone())?;

            if let Some(Scope::Break(v)) = while_environment.borrow().get_scope(&ScopeType::Break) {
                break_value = match v {
                    Some(v) => match else_statements {
                        None => Err(format!("Cannot break with a value in a while loop without an else block (add an else block with 'else {{}}')")),
                        Some(_) => Ok(v.clone())
                    },
                    None => Ok(Value::Void)
                }?;

                break 'outer;
            }

            if let Some(Scope::Continue) =
                while_environment.borrow().get_scope(&ScopeType::Continue)
            {
                continue 'outer;
            }

            if while_environment
                .borrow()
                .get_scope(&ScopeType::Return)
                .is_some()
            {
                break_value = Value::Void;
                break 'outer;
            }
        }
    }

    Ok(break_value)
}

fn evaluate_break(
    expression: Option<TypedExpression>,
    _type_: Type,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    if !environment.borrow().has_scope(&ScopeType::Break) {
        return Err(format!("Cannot break outside of a loop"));
    };

    match expression {
        Some(expression) => {
            let value = evaluate_expression(expression, environment.clone())?;
            environment
                .borrow_mut()
                .activate_scope(Scope::Break(Some(value)))?;
            Ok(Value::Void)
        }
        None => {
            environment
                .borrow_mut()
                .activate_scope(Scope::Break(None))?;
            Ok(Value::Void)
        }
    }
}

fn evaluate_continue(environment: Rcrc<Environment>) -> Result<Value, String> {
    if !environment.borrow().has_scope(&ScopeType::Continue) {
        return Err(format!("Cannot continue outside of a loop"));
    };

    environment.borrow_mut().activate_scope(Scope::Continue)?;
    Ok(Value::Void)
}

fn evaluate_return(
    expression: Option<TypedExpression>,
    _type_: Type,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    if !environment.borrow().has_scope(&ScopeType::Return) {
        return Err(format!("Cannot return outside of a function"));
    };

    match expression {
        Some(expression) => {
            let value = evaluate_expression(expression, environment.clone())?;
            environment
                .borrow_mut()
                .activate_scope(Scope::Return(Some(value)))?;
            Ok(Value::Void)
        }
        None => {
            environment
                .borrow_mut()
                .activate_scope(Scope::Return(None))?;
            Ok(Value::Void)
        }
    }
}
