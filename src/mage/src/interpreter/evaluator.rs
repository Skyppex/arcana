use shared::type_checker::{ast::*, Type};

use super::{value::{Value, Number, UnionMember, UnionFields}, evironment::Environment, evaluate_binop};

pub fn evaluate(typed_statement: TypedStatement, environment: &mut Environment) -> Result<Value, String> {
    match typed_statement {
        TypedStatement::Program {
            statements
        } => evaluate_program(statements, environment),
        TypedStatement::Expression(e) => evaluate_expression(e, environment),
        TypedStatement::Print(e) => {
            let value = evaluate_expression(e, environment)?;
            println!("{}", value);
            Ok(Value::Void)
        }
        _ => Ok(Value::Void)
    }
}

fn evaluate_expression(typed_expression: TypedExpression, environment: &mut Environment) -> Result<Value, String> {
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
        TypedExpression::Literal(l) => evaluate_literal(l),
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
        TypedExpression::Block {
            statements,
            type_
        } => evaluate_block(statements, type_, environment),
        TypedExpression::Drop {
            identifier,
            type_
        } => evaluate_drop(identifier, type_, environment),
    }
}

fn evaluate_program(statements: Vec<TypedStatement>, environment: &mut Environment) -> Result<Value, String> {
    let mut value = Value::Void;
    for statement in statements {
        value = evaluate(statement, environment)?;
    }
    Ok(value)
}

fn evaluate_variable_declaration(
    mutable: bool,
    identifier: String,
    initializer: Option<Box<TypedExpression>>,
    _type_: Type,
    environment: &mut Environment
) -> Result<Value, String> {
    let value = match initializer {
        Some(initializer) => evaluate_expression(*initializer, environment)?,
        None => Value::Void
    };
    environment.add_variable(identifier, value.clone(), mutable);
    Ok(value)
}

fn evaluate_if(
    r#if: ConditionBlock,
    else_ifs: Vec<ConditionBlock>,
    r#else: Option<Box<TypedExpression>>,
    _type_: Type,
    environment: &mut Environment
) -> Result<Value, String> {
    let if_environment = &mut environment.new_child();
    let condition = evaluate_expression(*r#if.condition, if_environment)?;

    match condition {
        Value::Bool(v) => {
            if v {
                let value = evaluate_expression(*r#if.block, if_environment);
                if r#else.is_none() {
                    return Ok(Value::Void);
                } else {
                    return value;
                }
            } else {
                for else_if in else_ifs {
                    let else_if_environment = &mut environment.new_child();
                    let condition = evaluate_expression(*else_if.condition, else_if_environment)?;

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

fn evaluate_assignment(
    member: Box<Member>,
    initializer: Box<TypedExpression>,
    _type_: Type,
    environment: &mut Environment
) -> Result<Value, String> {
    let value = evaluate_expression(*initializer, environment)?;
    environment.set_variable(*member, value.clone())?;
    Ok(value)
}

fn evaluate_member(member: Member, environment: &mut Environment) -> Result<Value, String> {
    match member {
        Member::Identifier {
            symbol,
            type_: _
        } => {
            Ok(environment.get_variable(&symbol)
                .ok_or(format!("Variable '{}' not found", symbol))?
                .value.clone())
        }
        Member::MemberAccess {
            object,
            member,
            symbol,
            type_: _
        } => {
            todo!()
        }
    }
}

fn evaluate_literal(literal: Literal) -> Result<Value, String> {
    match literal {
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
                fields.insert(field_initializer.identifier.unwrap(), evaluate_expression(field_initializer.initializer, &mut Environment::new())?);
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
                        fields.insert(identifier, evaluate_expression(initializer, &mut Environment::new())?);
                    }

                    UnionFields::Named(fields)
                }
                UnionMemberFieldInitializers::Unnamed(field_initializers) => {
                    let mut fields = Vec::new();

                    for initializer in field_initializers {
                        fields.push(evaluate_expression(initializer, &mut Environment::new())?);
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

fn evaluate_call(
    caller: Box<TypedExpression>,
    arguments: Vec<TypedExpression>,
    type_: Type,
    environment: &mut Environment
) -> Result<Value, String> {
    todo!()
}

fn evaluate_unary(
    operator: UnaryOperator,
    expression: Box<TypedExpression>,
    _type_: Type,
    environment: &mut Environment
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

fn evaluate_binary(
    left: Box<TypedExpression>,
    operator: BinaryOperator,
    right: Box<TypedExpression>,
    _type_: Type,
    environment: &mut Environment
) -> Result<Value, String> {
    let left = evaluate_expression(*left, environment)?;
    let right = evaluate_expression(*right, environment)?;

    evaluate_binop::evaluate_binop(left, operator, right)
}

fn evaluate_ternary(
    condition: Box<TypedExpression>,
    true_expression: Box<TypedExpression>,
    false_expression: Box<TypedExpression>,
    _type_: Type,
    environment: &mut Environment
) -> Result<Value, String> {
    let ternary_environment = &mut environment.new_child();
    let condition = evaluate_expression(*condition, ternary_environment)?;

    match condition {
        Value::Bool(v) => {
            if v {
                evaluate_expression(*true_expression, ternary_environment)
            } else {
                evaluate_expression(*false_expression, ternary_environment)
            }
        }
        _ => Err(format!("First argument in ternary must be boolean '{}'", condition))
    }
}

fn evaluate_block(
    statements: Vec<TypedStatement>,
    _type_: Type,
    environment: &mut Environment
) -> Result<Value, String> {
    let block_environment = &mut environment.new_child();
    let mut value = Value::Void;

    for statement in statements {
        value = evaluate(statement, block_environment)?;
    }

    Ok(value)
}

fn evaluate_drop(
    identifier: String,
    _type_: Type,
    environment: &mut Environment
) -> Result<Value, String> {
    let variable: super::value::Variable = environment.remove_variable(&identifier)
        .ok_or(format!("Variable '{}' not found", identifier))?;

    Ok(variable.value)
}