use shared::type_checker::{ast::*, Type};

use super::{value::{Value, Number}, evironment::Environment};

pub fn evaluate(typed_statement: TypedStatement, environment: &mut Environment) -> Result<Value, String> {
    match typed_statement {
        TypedStatement::Program {
            statements
        } => evaluate_program(statements, environment),
        TypedStatement::Expression(e) => evaluate_expression(e, environment),
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
    else_ifs: Option<Vec<ConditionBlock>>,
    r#else: Option<Box<TypedExpression>>,
    type_: Type,
    environment: &mut Environment
) -> Result<Value, String> {
    todo!()
}

fn evaluate_assignment(
    member: Box<TypedExpression>,
    initializer: Box<TypedExpression>,
    type_: Type,
    environment: &mut Environment
) -> Result<Value, String> {
    todo!()
}

fn evaluate_member(m: Member, environment: &mut Environment) -> Result<Value, String> {
    todo!()
}

fn evaluate_literal(l: Literal, environment: &mut Environment) -> Result<Value, String> {
    match l {
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
        Literal::Struct { type_name, field_initializers, type_ } => todo!(),
        Literal::Union { type_name, member, field_initializers, type_ } => todo!(),
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
    type_: Type,
    environment: &mut Environment
) -> Result<Value, String> {
    todo!()
}

fn evaluate_binary(
    left: Box<TypedExpression>,
    operator: BinaryOperator,
    right: Box<TypedExpression>,
    type_: Type,
    environment: &mut Environment
) -> Result<Value, String> {
    todo!()
}

fn evaluate_ternary(
    condition: Box<TypedExpression>,
    true_expression: Box<TypedExpression>,
    false_expression: Box<TypedExpression>,
    type_: Type,
    environment: &mut Environment
) -> Result<Value, String> {
    todo!()
}

fn evaluate_block(
    statements: Vec<TypedStatement>,
    type_: Type,
    environment: &mut Environment
) -> Result<Value, String> {
    todo!()
}

fn evaluate_drop(
    identifier: String,
    type_: Type,
    environment: &mut Environment
) -> Result<Value, String> {
    todo!()
}