use crate::parser::{Expression, VariableDeclaration, If, Assignment, Call, Unary, Binary, Ternary};

use super::ast::TypedExpression;



pub fn check_type(expression: &Expression) -> Result<TypedExpression, String> {
    match expression {
        Expression::None => Ok(TypedExpression::None),
        Expression::VariableDeclaration(VariableDeclaration {
            mutable,
            type_name,
            identifier,
            initializer
        }) => todo!(),
        Expression::If(If {
            r#if,
            else_ifs,
            r#else
        }) => todo!(),
        Expression::Assignment(Assignment {
            member,
            initializer
        }) => todo!(),
        Expression::Member(m) => todo!(),
        Expression::Literal(l) => todo!(),
        Expression::Call(Call {
            caller,
            arguments
        }) => todo!(),
        Expression::Unary(Unary {
            operator,
            expression
        }) => todo!(),
        Expression::Binary(Binary {
            left,
            operator,
            right
        }) => todo!(),
        Expression::Ternary(Ternary {
            condition,
            true_expression,
            false_expression
        }) => todo!(),
        Expression::Block(statements) => todo!(),
        Expression::Drop(identifier) => todo!(),
    }
}