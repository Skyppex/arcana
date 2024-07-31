use crate::type_checker::{
    ast::{BinaryOperator, Pattern, Typed},
    expressions::check_type,
    type_equals, type_equals_coerce, Type,
};

use super::{
    ast::{TypedExpression, TypedMatchArm},
    DiscoveredType,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Decision {
    Success {
        expression: Box<TypedExpression>,
        type_: Type,
    },
    Failure {
        error_message: String,
    },
    Guard {
        condition: Box<TypedExpression>,
        consequence: Box<Decision>,
        alternative: Box<Decision>,
        type_: Type,
    },
    Switch {
        variable: Variable,
        cases: Vec<Case>,
        fallback: Box<Decision>,
        type_: Type,
    },
}

impl Typed for Decision {
    fn get_type(&self) -> Type {
        match self {
            Decision::Success { type_, .. } => type_.clone(),
            Decision::Failure { .. } => Type::Unknown,
            Decision::Guard { type_, .. } => type_.clone(),
            Decision::Switch { type_, .. } => type_.clone(),
        }
    }

    fn get_deep_type(&self) -> Type {
        self.get_type()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Case {
    pub pattern: Pattern,
    pub argument: Variable,
    pub body: Decision,
}

impl Typed for Case {
    fn get_type(&self) -> Type {
        self.body.get_type()
    }

    fn get_deep_type(&self) -> Type {
        self.get_type()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub identifier: String,
    pub type_: Type,
}

impl Typed for Variable {
    fn get_type(&self) -> Type {
        self.type_.clone()
    }

    fn get_deep_type(&self) -> Type {
        self.get_type()
    }
}

pub fn create_decision_tree(
    matchee: TypedExpression,
    arms: Vec<TypedMatchArm>,
    discovered_types: &Vec<DiscoveredType>,
    body_type: Option<Type>,
) -> Result<Decision, String> {
    if arms.is_empty() {
        return Ok(Decision::Failure {
            error_message: "No match found".to_string(),
        });
    }

    let arm = arms.first().expect("testing matches");

    let decision = match arm.pattern.clone() {
        Pattern::Wildcard => {
            let expression = &arm.expression;
            let type_environment = &arm.type_environment;
            let expression =
                check_type(expression, discovered_types, type_environment.clone(), None)?;

            let type_ = expression.get_type();

            if let Some(body_type) = body_type {
                if !type_equals_coerce(&body_type, &type_) {
                    return Err(format!("Expected type {:?} but got {:?}", body_type, type_));
                }
            }

            Ok(Decision::Success {
                expression: Box::new(expression),
                type_: type_.clone(),
            })
        }
        Pattern::Unit => {
            if !type_equals(&Type::Unit, &matchee.get_type()) {
                return Err(format!(
                    "Expected type {:?} but got {:?}",
                    Type::Unit,
                    matchee.get_type()
                ));
            }

            let expression = &arm.expression;
            let type_environment = &arm.type_environment;
            let expression =
                check_type(expression, discovered_types, type_environment.clone(), None)?;

            let type_ = expression.get_type();

            if let Some(body_type) = body_type {
                if !type_equals_coerce(&body_type, &type_) {
                    return Err(format!("Expected type {:?} but got {:?}", body_type, type_));
                }
            }

            let false_ = create_decision_tree(
                matchee.clone(),
                arms.into_iter().skip(1).collect(),
                discovered_types,
                Some(type_.clone()),
            )?;

            let decision = Decision::Guard {
                condition: Box::new(TypedExpression::Binary {
                    left: Box::new(matchee),
                    operator: BinaryOperator::Equal,
                    right: Box::new(TypedExpression::Literal(
                        crate::type_checker::ast::Literal::Unit,
                    )),
                    type_: Type::Bool,
                }),
                consequence: Box::new(Decision::Success {
                    expression: Box::new(expression),
                    type_: type_.clone(),
                }),
                alternative: Box::new(false_),
                type_,
            };

            Ok(decision)
        }
        Pattern::Bool(v) => {
            if !type_equals(&Type::Bool, &matchee.get_type()) {
                return Err(format!(
                    "Expected type {:?} but got {:?}",
                    Type::Bool,
                    matchee.get_type()
                ));
            }

            let expression = &arm.expression;
            let type_environment = &arm.type_environment;
            let expression =
                check_type(expression, discovered_types, type_environment.clone(), None)?;

            let type_ = expression.get_type();

            if let Some(body_type) = body_type {
                if !type_equals_coerce(&body_type, &type_) {
                    return Err(format!("Expected type {:?} but got {:?}", body_type, type_));
                }
            }

            let false_ = create_decision_tree(
                matchee.clone(),
                arms.into_iter().skip(1).collect(),
                discovered_types,
                Some(type_.clone()),
            )?;

            let decision = Decision::Guard {
                condition: Box::new(TypedExpression::Binary {
                    left: Box::new(matchee),
                    operator: BinaryOperator::Equal,
                    right: Box::new(TypedExpression::Literal(
                        crate::type_checker::ast::Literal::Bool(v),
                    )),
                    type_: Type::Bool,
                }),
                consequence: Box::new(Decision::Success {
                    expression: Box::new(expression),
                    type_: type_.clone(),
                }),
                alternative: Box::new(false_),
                type_,
            };

            Ok(decision)
        }
        Pattern::Int(v) => {
            if !type_equals(&Type::Int, &matchee.get_type()) {
                return Err(format!(
                    "Expected type {:?} but got {:?}",
                    Type::Int,
                    matchee.get_type()
                ));
            }
            let expression = &arm.expression;
            let type_environment = &arm.type_environment;
            let expression =
                check_type(expression, discovered_types, type_environment.clone(), None)?;

            let type_ = expression.get_type();

            if let Some(body_type) = body_type {
                if !type_equals_coerce(&body_type, &type_) {
                    return Err(format!("Expected type {:?} but got {:?}", body_type, type_));
                }
            }

            let false_ = create_decision_tree(
                matchee.clone(),
                arms.into_iter().skip(1).collect(),
                discovered_types,
                Some(type_.clone()),
            )?;

            let decision = Decision::Guard {
                condition: Box::new(TypedExpression::Binary {
                    left: Box::new(matchee),
                    operator: BinaryOperator::Equal,
                    right: Box::new(TypedExpression::Literal(
                        crate::type_checker::ast::Literal::Int(v),
                    )),
                    type_: Type::Bool,
                }),
                consequence: Box::new(Decision::Success {
                    expression: Box::new(expression),
                    type_: type_.clone(),
                }),
                alternative: Box::new(false_),
                type_,
            };

            Ok(decision)
        }
        Pattern::UInt(v) => {
            if !type_equals(&Type::UInt, &matchee.get_type()) {
                return Err(format!(
                    "Expected type {:?} but got {:?}",
                    Type::UInt,
                    matchee.get_type()
                ));
            }
            let expression = &arm.expression;
            let type_environment = &arm.type_environment;
            let expression =
                check_type(expression, discovered_types, type_environment.clone(), None)?;

            let type_ = expression.get_type();

            if let Some(body_type) = body_type {
                if !type_equals_coerce(&body_type, &type_) {
                    return Err(format!("Expected type {:?} but got {:?}", body_type, type_));
                }
            }

            let false_ = create_decision_tree(
                matchee.clone(),
                arms.into_iter().skip(1).collect(),
                discovered_types,
                Some(type_.clone()),
            )?;

            let decision = Decision::Guard {
                condition: Box::new(TypedExpression::Binary {
                    left: Box::new(matchee),
                    operator: BinaryOperator::Equal,
                    right: Box::new(TypedExpression::Literal(
                        crate::type_checker::ast::Literal::UInt(v),
                    )),
                    type_: Type::Bool,
                }),
                consequence: Box::new(Decision::Success {
                    expression: Box::new(expression),
                    type_: type_.clone(),
                }),
                alternative: Box::new(false_),
                type_,
            };

            Ok(decision)
        }
        Pattern::Float(v) => {
            if !type_equals(&Type::Float, &matchee.get_type()) {
                return Err(format!(
                    "Expected type {:?} but got {:?}",
                    Type::Float,
                    matchee.get_type()
                ));
            }
            let expression = &arm.expression;
            let type_environment = &arm.type_environment;
            let expression =
                check_type(expression, discovered_types, type_environment.clone(), None)?;

            let type_ = expression.get_type();

            if let Some(body_type) = body_type {
                if !type_equals_coerce(&body_type, &type_) {
                    return Err(format!("Expected type {:?} but got {:?}", body_type, type_));
                }
            }

            let false_ = create_decision_tree(
                matchee.clone(),
                arms.into_iter().skip(1).collect(),
                discovered_types,
                Some(type_.clone()),
            )?;

            let decision = Decision::Guard {
                condition: Box::new(TypedExpression::Binary {
                    left: Box::new(matchee),
                    operator: BinaryOperator::Equal,
                    right: Box::new(TypedExpression::Literal(
                        crate::type_checker::ast::Literal::Float(v),
                    )),
                    type_: Type::Bool,
                }),
                consequence: Box::new(Decision::Success {
                    expression: Box::new(expression),
                    type_: type_.clone(),
                }),
                alternative: Box::new(false_),
                type_,
            };

            Ok(decision)
        }
        Pattern::Char(v) => {
            if !type_equals(&Type::Char, &matchee.get_type()) {
                return Err(format!(
                    "Expected type {:?} but got {:?}",
                    Type::Char,
                    matchee.get_type()
                ));
            }
            let expression = &arm.expression;
            let type_environment = &arm.type_environment;
            let expression =
                check_type(expression, discovered_types, type_environment.clone(), None)?;

            let type_ = expression.get_type();

            if let Some(body_type) = body_type {
                if !type_equals_coerce(&body_type, &type_) {
                    return Err(format!("Expected type {:?} but got {:?}", body_type, type_));
                }
            }

            let false_ = create_decision_tree(
                matchee.clone(),
                arms.into_iter().skip(1).collect(),
                discovered_types,
                Some(type_.clone()),
            )?;

            let decision = Decision::Guard {
                condition: Box::new(TypedExpression::Binary {
                    left: Box::new(matchee),
                    operator: BinaryOperator::Equal,
                    right: Box::new(TypedExpression::Literal(
                        crate::type_checker::ast::Literal::Char(v),
                    )),
                    type_: Type::Bool,
                }),
                consequence: Box::new(Decision::Success {
                    expression: Box::new(expression),
                    type_: type_.clone(),
                }),
                alternative: Box::new(false_),
                type_,
            };

            Ok(decision)
        }
        Pattern::String(v) => {
            if !type_equals(&Type::String, &matchee.get_type()) {
                return Err(format!(
                    "Expected type {:?} but got {:?}",
                    Type::String,
                    matchee.get_type()
                ));
            }
            let expression = &arm.expression;
            let type_environment = &arm.type_environment;
            let expression =
                check_type(expression, discovered_types, type_environment.clone(), None)?;

            let type_ = expression.get_type();

            if let Some(body_type) = body_type {
                if !type_equals_coerce(&body_type, &type_) {
                    return Err(format!("Expected type {:?} but got {:?}", body_type, type_));
                }
            }

            let false_ = create_decision_tree(
                matchee.clone(),
                arms.into_iter().skip(1).collect(),
                discovered_types,
                Some(type_.clone()),
            )?;

            let decision = Decision::Guard {
                condition: Box::new(TypedExpression::Binary {
                    left: Box::new(matchee),
                    operator: BinaryOperator::Equal,
                    right: Box::new(TypedExpression::Literal(
                        crate::type_checker::ast::Literal::String(v.clone()),
                    )),
                    type_: Type::Bool,
                }),
                consequence: Box::new(Decision::Success {
                    expression: Box::new(expression),
                    type_: type_.clone(),
                }),
                alternative: Box::new(false_),
                type_,
            };

            Ok(decision)
        }
        Pattern::Variable(ref identifier) => {
            let expression = &arm.expression;
            let type_environment = &arm.type_environment;
            let matchee_type = matchee.get_type();

            type_environment
                .borrow_mut()
                .add_variable(identifier.clone(), matchee_type.clone());

            let expression =
                check_type(expression, discovered_types, type_environment.clone(), None)?;

            let type_ = expression.get_type();

            if let Some(body_type) = body_type {
                if !type_equals_coerce(&type_, &body_type) {
                    return Err(format!("Expected type {:?} but got {:?}", body_type, type_));
                }
            }

            let case = Case {
                pattern: Pattern::Variable(identifier.clone()),
                argument: Variable {
                    identifier: identifier.clone(),
                    type_: matchee_type.clone(),
                },
                body: Decision::Success {
                    expression: Box::new(expression),
                    type_: type_.clone(),
                },
            };

            Ok(Decision::Switch {
                variable: Variable {
                    identifier: identifier.clone(),
                    type_: matchee_type.clone(),
                },
                cases: vec![case],
                fallback: Box::new(Decision::Failure {
                    error_message: "No match found".to_string(),
                }),
                type_,
            })
        }
    };

    decision
}
