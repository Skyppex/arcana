use std::{cell::RefCell, rc::Rc};

use shared::type_checker::{ast::{Block, *}, Type};

use crate::interpreter::scope::Scope;

use super::{evaluate_binop, evironment::{Environment, Rcrc}, scope::ScopeType, value::{Value, Number, UnionMember, UnionFields}};

pub fn evaluate<'a>(typed_statement: TypedStatement, environment: Rcrc<Environment>) -> Result<Value, String> {
    match typed_statement {
        TypedStatement::None => Ok(Value::Void),
        TypedStatement::StructDeclaration { .. } => Ok(Value::Void),
        TypedStatement::UnionDeclaration { .. } => Ok(Value::Void),
        TypedStatement::Program {
            statements
        } => evaluate_program(statements, environment),
        TypedStatement::FunctionDeclaration {
            identifier,
            parameters,
            return_type: _,
            body,
            type_: _
        } => {
            environment.borrow_mut().add_variable(
                identifier.clone(),
                Value::Function {
                    parameters: parameters.iter().map(|p| p.identifier.clone()).collect(),
                    body,
                },
                false);

            Ok(Value::Void)
        }
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

fn evaluate_expression<'a>(typed_expression: TypedExpression, environment: Rcrc<Environment>) -> Result<Value, String> {
    match typed_expression {
        TypedExpression::None => Ok(Value::Void),
        TypedExpression::VariableDeclaration {
            mutable,
            identifier,
            initializer,
            type_
        } => evaluate_variable_declaration(mutable, identifier, initializer, type_, environment),
        TypedExpression::If {
            r#if,
            else_ifs,
            r#else,
            type_
        } => evaluate_if(r#if, else_ifs, r#else, type_, environment),
        TypedExpression::Assignment {
            member,
            initializer,
            type_
        } => evaluate_assignment(member, initializer, type_, environment),
        TypedExpression::Member(m) => evaluate_member(m, environment),
        TypedExpression::Literal(l) => evaluate_literal(l, environment),
        TypedExpression::Call {
            caller,
            arguments,
            type_
        } => evaluate_call(caller, arguments, type_, environment),
        TypedExpression::Unary {
            operator,
            expression,
            type_
        } => evaluate_unary(operator, expression, type_, environment),
        TypedExpression::Binary {
            left,
            operator,
            right,
            type_
        } => evaluate_binary(left, operator, right, type_, environment),
        TypedExpression::Ternary {
            condition,
            true_expression,
            false_expression,
            type_
        } => evaluate_ternary(condition, true_expression, false_expression, type_, environment),
        TypedExpression::Block(Block {
            statements,
            type_
        }) => evaluate_block(statements, type_, environment),
        TypedExpression::Drop {
            identifier,
            type_
        } => evaluate_drop(identifier, type_, environment),
        TypedExpression::Loop(Block {
            statements,
            type_: _
        }) => evaluate_loop(statements, environment),
        TypedExpression::While {
            condition,
            block,
            else_block,
            type_
        } => evaluate_while(condition, block, else_block, type_, environment),
    }
}

fn evaluate_program<'a>(statements: Vec<TypedStatement>, environment: Rcrc<Environment>) -> Result<Value, String> {
    let mut value = Value::Void;
    for statement in statements {
        value = evaluate(statement, environment.clone())?;
    }
    Ok(value)
}

fn evaluate_variable_declaration<'a>(
    mutable: bool,
    identifier: String,
    initializer: Option<Box<TypedExpression>>,
    _type_: Type,
    environment: Rcrc<Environment>
) -> Result<Value, String> {
    let value = match initializer {
        Some(initializer) => evaluate_expression(*initializer, environment.clone())?,
        None => Value::Uninitialized
    };
    environment.borrow_mut().add_variable(identifier, value.clone(), mutable);
    Ok(value)
}

fn evaluate_if<'a>(
    r#if: ConditionBlock,
    else_ifs: Vec<ConditionBlock>,
    r#else: Option<Box<TypedExpression>>,
    _type_: Type,
    environment: Rcrc<Environment>
) -> Result<Value, String> {
    let if_environment = Rc::new(RefCell::new(Environment::new_parent(environment.clone())));
    let condition = evaluate_expression(*r#if.condition, if_environment.clone())?;

    match condition {
        Value::Bool(v) => {
            if v {
                let value = evaluate_expression(*r#if.block, if_environment)?;
                if r#else.is_none() {
                    return Ok(Value::Void);
                } else {
                    return Ok(value);
                }
            } else {
                for else_if in else_ifs {
                    let else_if_environment = Rc::new(RefCell::new(Environment::new_parent(environment.clone())));
                    let condition = evaluate_expression(*else_if.condition, else_if_environment.clone())?;

                    match condition {
                        Value::Bool(v) => {
                            if v {
                                let value =  evaluate_expression(*else_if.block, else_if_environment);
                                if r#else.is_none() {
                                    return Ok(Value::Void);
                                } else {
                                    return value;
                                }
                            }
                        }
                        _ => return Err(format!("Else if condition must be boolean '{}'", condition))
                    }
                }

                match r#else {
                    Some(r#else) => evaluate_expression(*r#else, if_environment),
                    None => Ok(Value::Void)
                }
            }
        }
        _ => Err(format!("If condition must be boolean '{}'", condition))
    }
}

fn evaluate_assignment<'a>(
    member: Box<Member>,
    initializer: Box<TypedExpression>,
    _type_: Type,
    environment: Rcrc<Environment>
) -> Result<Value, String> {
    let value = evaluate_expression(*initializer, environment.clone())?;
    environment.borrow_mut().set_variable(*member, value.clone())?;
    Ok(value)
}

fn evaluate_member<'a>(member: Member, environment: Rcrc<Environment>) -> Result<Value, String> {
    match member {
        Member::Identifier {
            symbol,
            type_: _
        } => {
            Ok(environment.borrow().get_variable(&symbol)
                .ok_or(format!("Variable '{}' not found", symbol))?
                .borrow().value.clone())
        }
        Member::MemberAccess {
            object,
            member,
            symbol: _,
            type_: _
        } => {
            evaluate_member_access(object, environment, member)
        }
    }
}

fn evaluate_member_access<'a>(object: Box<TypedExpression>, environment: Rcrc<Environment>, member: Box<Member>) -> Result<Value, String> {
    let value = evaluate_expression(*object, environment.clone())?;

    match value {
        Value::Struct { struct_name, fields } => {
            match *member.clone() {
                Member::Identifier {
                    symbol,
                    type_: _
                } => {
                    let field_value = fields.get(&symbol)
                        .ok_or(format!("Field '{}' not found in struct '{}'", symbol, struct_name))?;

                    Ok(field_value.clone())
                },
                Member::MemberAccess {
                    object,
                    member,
                    symbol: _,
                    type_: _
                } => evaluate_member_access(object, environment, member),
            }
        }
        _ => Err(format!("Member access is only supported on structs '{}'", value))
    }
}

fn evaluate_literal<'a>(literal: Literal, environment: Rcrc<Environment>) -> Result<Value, String> {
    match literal {
        Literal::Unit => Ok(Value::Unit),
        Literal::I8(v) => Ok(Value::Number(Number::I8(v))),
        Literal::I16(v) => Ok(Value::Number(Number::I16(v))),
        Literal::I32(v) => Ok(Value::Number(Number::I32(v))),
        Literal::I64(v) => Ok(Value::Number(Number::I64(v))),
        Literal::I128(v) => Ok(Value::Number(Number::I128(v))),
        Literal::U8(v) => Ok(Value::Number(Number::U8(v))),
        Literal::U16(v) => Ok(Value::Number(Number::U16(v))),
        Literal::U32(v) => Ok(Value::Number(Number::U32(v))),
        Literal::U64(v) => Ok(Value::Number(Number::U64(v))),
        Literal::U128(v) => Ok(Value::Number(Number::U128(v))),
        Literal::F32(v) => Ok(Value::Number(Number::F32(v))),
        Literal::F64(v) => Ok(Value::Number(Number::F64(v))),
        Literal::String(v) => Ok(Value::String(v)),
        Literal::Char(v) => Ok(Value::Char(v)),
        Literal::Bool(v) => Ok(Value::Bool(v)),
        Literal::Struct {
            type_name,
            field_initializers,
            type_: _
        } => {
            let mut fields = std::collections::HashMap::new();

            for field_initializer in field_initializers {
                fields.insert(
                    field_initializer.identifier.unwrap(),
                     evaluate_expression(field_initializer.initializer, environment.clone())?);
            }

            Ok(Value::Struct { struct_name: type_name, fields })
        }        
        Literal::Union {
            type_name,
            member,
            field_initializers,
            type_: _
        } => {
            let fields: UnionFields = match field_initializers {
                UnionMemberFieldInitializers::None => UnionFields::None,
                UnionMemberFieldInitializers::Named(field_initializers) => {
                    let mut fields = std::collections::HashMap::new();

                    for (identifier, initializer) in field_initializers {
                        fields.insert(identifier, evaluate_expression(initializer, environment.clone())?);
                    }

                    UnionFields::Named(fields)
                }
                UnionMemberFieldInitializers::Unnamed(field_initializers) => {
                    let mut fields = Vec::new();

                    for initializer in field_initializers {
                        fields.push(evaluate_expression(initializer, environment.clone())?);
                    }

                    UnionFields::Unnamed(fields)
                }
            };
            
            Ok(Value::Union { union_member: UnionMember {
                union_name: type_name,
                member_name: member,
            }, fields })
        }
    }
}

fn evaluate_call<'a>(
    caller: Box<TypedExpression>,
    arguments: Vec<TypedExpression>,
    _type_: Type,
    environment: Rcrc<Environment>
) -> Result<Value, String> {
    let caller_value = evaluate_expression(*caller, environment.clone())?;
 
    let mut evaluated_arguments = Vec::new();

    for argument in arguments {
        let evaluated_arg = evaluate_expression(argument, environment.clone())?;
        evaluated_arguments.push(evaluated_arg);
    }

    match caller_value {
        Value::Function { parameters, body } => {
            let function_environment =
                Rc::new(RefCell::new(Environment::new_scope(
                    environment,
                    ScopeType::Return)));

            for (index, parameter) in parameters.iter().enumerate() {
                function_environment.borrow_mut().add_variable(parameter.clone(), evaluated_arguments[index].clone(), false);
            }

            let mut value = Value::Void;
            for statement in body {
                value = evaluate(statement, function_environment.clone())?;

                if let Some(Scope::Return(v)) =
                    function_environment.borrow()
                    .get_scope(&ScopeType::Return) {
                    match v {
                        Some(v) => {
                            if _type_ == Type::Void {
                                return Err(format!(
                                    "Cannot return a value from a void function",
                                ));
                            }

                            value = v.clone();
                            break;
                        },
                        None => {
                            if _type_ != Type::Void {
                                return Err(format!(
                                    "Cannot return void from a non-void function. Expected type '{}', found type 'void'",
                                    _type_
                                ));
                            }

                            value = Value::Void;
                            break;
                        }
                    }
                }
            }

            if _type_ == Type::Void {
                return Ok(Value::Void);
            }

            Ok(value)
        }
        _ => Err(format!("Cannot call non-function value '{}'", caller_value))
    }
}

fn evaluate_unary<'a>(
    operator: UnaryOperator,
    expression: Box<TypedExpression>,
    _type_: Type,
    environment: Rcrc<Environment>
) -> Result<Value, String> {
    let value = evaluate_expression(*expression, environment)?;

    match operator {
        UnaryOperator::Identity => {
            match value {
                Value::Number(number) => Ok(Value::Number(number)),
                _ => Err(format!("Cannot apply unary operator '+' to non-number value '{}'", value))
            }
        }
        UnaryOperator::Negate => {
            match value {
                Value::Number(number) => {
                    match number {
                        Number::I8(v) => Ok(Value::Number(Number::I8(-v))),
                        Number::I16(v) => Ok(Value::Number(Number::I16(-v))),
                        Number::I32(v) => Ok(Value::Number(Number::I32(-v))),
                        Number::I64(v) => Ok(Value::Number(Number::I64(-v))),
                        Number::I128(v) => Ok(Value::Number(Number::I128(-v))),
                        Number::F32(v) => Ok(Value::Number(Number::F32(-v))),
                        Number::F64(v) => Ok(Value::Number(Number::F64(-v))),
                        other => Err(format!("Cannot apply unary operator '-' to unsigned integers '{}'", other))
                    }
                }
                _ => Err(format!("Cannot apply unary operator '-' to non-number value '{}'", value))
            }
        }
        UnaryOperator::BitwiseNot => {
            match value {
                Value::Number(number) => {
                    match number {
                        Number::I8(v) => Ok(Value::Number(Number::I8(!v))),
                        Number::I16(v) => Ok(Value::Number(Number::I16(!v))),
                        Number::I32(v) => Ok(Value::Number(Number::I32(!v))),
                        Number::I64(v) => Ok(Value::Number(Number::I64(!v))),
                        Number::I128(v) => Ok(Value::Number(Number::I128(!v))),
                        Number::U8(v) => Ok(Value::Number(Number::U8(!v))),
                        Number::U16(v) => Ok(Value::Number(Number::U16(!v))),
                        Number::U32(v) => Ok(Value::Number(Number::U32(!v))),
                        Number::U64(v) => Ok(Value::Number(Number::U64(!v))),
                        Number::U128(v) => Ok(Value::Number(Number::U128(!v))),
                        other => Err(format!("Cannot apply unary operator '~' to floating point numbers '{}'", other))
                    }
                }
                _ => Err(format!("Cannot apply unary operator '~' to non-number value '{}'", value))
            }
        },
        UnaryOperator::LogicalNot => {
            match value {
                Value::Bool(v) => Ok(Value::Bool(!v)),
                _ => Err(format!("Cannot apply unary operator '!' to non-boolean value '{}'", value))
            }
        }
    }
}

fn evaluate_binary<'a>(
    left: Box<TypedExpression>,
    operator: BinaryOperator,
    right: Box<TypedExpression>,
    _type_: Type,
    environment: Rcrc<Environment>
) -> Result<Value, String> {
    let left = evaluate_expression(*left, environment.clone())?;
    let right = evaluate_expression(*right, environment)?;

    evaluate_binop::evaluate_binop(left, operator, right)
}

fn evaluate_ternary<'a>(
    condition: Box<TypedExpression>,
    true_expression: Box<TypedExpression>,
    false_expression: Box<TypedExpression>,
    _type_: Type,
    environment: Rcrc<Environment>
) -> Result<Value, String> {
    let ternary_environment = Rc::new(RefCell::new(Environment::new_parent(environment)));
    let condition = evaluate_expression(*condition, ternary_environment.clone())?;

    match condition {
        Value::Bool(v) => {
            if v {
                evaluate_expression(*true_expression, ternary_environment.clone())
            } else {
                evaluate_expression(*false_expression, ternary_environment)
            }
        }
        _ => Err(format!("First argument in ternary must be boolean '{}'", condition))
    }
}

fn evaluate_block<'a>(
    statements: Vec<TypedStatement>,
    _type_: Type,
    environment: Rcrc<Environment>
) -> Result<Value, String> {
    let block_environment = Rc::new(RefCell::new(Environment::new_parent(environment)));
    let mut value = Value::Void;

    for statement in statements {
        value = evaluate(statement, block_environment.clone())?;
    }

    Ok(value)
}

fn evaluate_drop<'a>(
    identifier: String,
    _type_: Type,
    environment: Rcrc<Environment>
) -> Result<Value, String> {
    let variable = environment.borrow_mut().remove_variable(&identifier)
        .ok_or(format!("Variable '{}' not found", identifier))?;

    let value = variable.borrow().value.clone();
    Ok(value)
}

fn evaluate_loop(
    statements: Vec<TypedStatement>,
    environment: Rcrc<Environment>) -> Result<Value, String> {
    let loop_environment =
        Rc::new(RefCell::new(
            Environment::new_scopes(environment, 
                [Scope::Break(None), Scope::Continue])));

    let break_value;

    'outer: loop {
        for statement in statements.clone() {
            evaluate(statement, loop_environment.clone())?;

            if let Some(Scope::Break(v)) = loop_environment.borrow()
                .get_scope(&ScopeType::Break) {
                break_value = match v {
                    Some(v) => v.clone(),
                    None => Value::Void
                };

                break 'outer;
            }

            if let Some(Scope::Continue) = loop_environment.borrow().get_scope(&ScopeType::Continue) {
                continue 'outer;
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
    environment: Rcrc<Environment>
) -> Result<Value, String> {
    let while_environment =
        Rc::new(RefCell::new(
            Environment::new_scopes(environment, 
                [ScopeType::Break, ScopeType::Continue])));

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
                            }

                            value
                        },
                        None => Value::Void
                    };

                    break 'outer;
                }
            }
            _ => return Err(format!("While condition must be boolean '{}'", value))
        }

        for statement in statements.clone() {
            evaluate(statement, while_environment.clone())?;

            if let Some(Scope::Break(v)) = while_environment.borrow()
                .get_scope(&ScopeType::Break) {
                break_value = match v {
                    Some(v) => match else_statements {
                        None => Err(format!("Cannot break with a value in a while loop without an else block (add an else block with 'else {{}}')")),
                        Some(_) => Ok(v.clone())
                    },
                    None => Ok(Value::Void)
                }?;

                break 'outer;
            }

            if let Some(Scope::Continue) = while_environment.borrow().get_scope(&ScopeType::Continue) {
                continue 'outer;
            }
        }
    }

    Ok(break_value)
}

fn evaluate_break(
    expression: Option<TypedExpression>,
    _type_: Type,
    environment: Rcrc<Environment>
) -> Result<Value, String> {
    if !environment.borrow().has_scope(&ScopeType::Break) {
        return Err(format!("Cannot break outside of a loop"));
    };
    
    match expression {
        Some(expression) => {
            environment.borrow_mut().activate_scope(Scope::Break(Some(evaluate_expression(expression, environment.clone())?)))?;
            Ok(Value::Void)
        },
        None => {
            environment.borrow_mut().activate_scope(Scope::Break(None))?;
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
    environment: Rcrc<Environment>
) -> Result<Value, String> {
    if !environment.borrow().has_scope(&ScopeType::Return) {
        return Err(format!("Cannot return outside of a function"));
    };

    match expression {
        Some(expression) => {
            let value = evaluate_expression(expression, environment.clone())?;
            environment.borrow_mut().activate_scope(Scope::Return(Some(value)))?;
            Ok(Value::Void)
        },
        None => {
            environment.borrow_mut().activate_scope(Scope::Return(None))?;
            Ok(Value::Void)
        }
    }
}
