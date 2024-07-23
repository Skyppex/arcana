use crate::type_checker::{ast::BinaryOperator, Type};

use super::ast::{TypedExpression, TypedMatchArm};

pub enum Desicion {
    Success {
        expression: TypedExpression,
    },
    Failure {
        error_message: String,
    },
    Guard {
        condition: TypedExpression,
        true_: Box<Desicion>,
        false_: Box<Desicion>,
    },
}

pub fn create_desicion_tree(
    matchee: TypedExpression,
    arms: Vec<TypedMatchArm>,
) -> Result<Desicion, String> {
    let arm = arms.first().expect("testing matches");
    let desicion = match arm.pattern {
        crate::type_checker::ast::Pattern::Wildcard => todo!(),
        crate::type_checker::ast::Pattern::Int(v) => {
            let desicion = Desicion::Guard {
                condition: TypedExpression::Binary {
                    left: Box::new(matchee.clone()),
                    operator: BinaryOperator::Equal,
                    right: Box::new(TypedExpression::Literal(
                        crate::type_checker::ast::Literal::Int(v),
                    )),
                    type_: Type::Bool,
                },
                true_: Box::new(Desicion::Success {
                    expression: arm.expression.clone(),
                }),
                false_: Box::new(create_desicion_tree(
                    matchee,
                    arms.into_iter().skip(1).collect(),
                )?),
            };

            Ok(desicion)
        }
    };

    desicion
}
