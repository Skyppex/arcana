use std::fmt::Display;

use crate::{
    type_checker::{
        ast::{BinaryOperator, Member, Typed},
        expressions::check_type,
        type_annotation_equals, type_equals, type_equals_coerce, Struct, Type,
    },
    types::TypeAnnotation,
};

use super::{
    ast::{TypedExpression, TypedMatchArm},
    DiscoveredType,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Constructor {
    Struct {
        type_annotation: TypeAnnotation,
        field_patterns: Vec<FieldPattern>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Unit,
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    Char(char),
    String(String),
    Variable(String),
    Constructor(Constructor),
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Wildcard => write!(f, "_"),
            Pattern::Unit => write!(f, "()"),
            Pattern::Bool(v) => write!(f, "{}", v),
            Pattern::Int(v) => write!(f, "{}", v),
            Pattern::UInt(v) => write!(f, "{}", v),
            Pattern::Float(v) => write!(f, "{}", v),
            Pattern::Char(v) => write!(f, "{}", v),
            Pattern::String(v) => write!(f, "{}", v),
            Pattern::Variable(v) => write!(f, "{}", v),
            Pattern::Constructor(Constructor::Struct {
                type_annotation,
                field_patterns,
            }) => {
                write!(f, "{} {{ ", type_annotation)?;
                for (i, field_pattern) in field_patterns.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", field_pattern)?;
                }
                write!(f, " }}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldPattern {
    pub identifier: String,
    pub pattern: Pattern,
}

impl Display for FieldPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.identifier, self.pattern)
    }
}

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
    pub arguments: Vec<Variable>,
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
    pub accessor: Accessor,
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

#[derive(Debug, Clone, PartialEq)]
pub enum Accessor {
    Expression(Box<TypedExpression>),
    Environment,
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

    println!("matchee: {:?}", matchee);

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
                    matchee.get_type(),
                    Type::Unit,
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

            let alternative = create_decision_tree(
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
                alternative: Box::new(alternative),
                type_,
            };

            Ok(decision)
        }
        Pattern::Bool(v) => {
            if !type_equals(&Type::Bool, &matchee.get_type()) {
                return Err(format!(
                    "Expected type {:?} but got {:?}",
                    matchee.get_type(),
                    Type::Bool,
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

            let alternative = create_decision_tree(
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
                alternative: Box::new(alternative),
                type_,
            };

            Ok(decision)
        }
        Pattern::Int(v) => {
            if !type_equals(&Type::Int, &matchee.get_type()) {
                return Err(format!(
                    "Expected type {:?} but got {:?}",
                    matchee.get_type(),
                    Type::Int,
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

            let alternative = create_decision_tree(
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
                alternative: Box::new(alternative),
                type_,
            };

            Ok(decision)
        }
        Pattern::UInt(v) => {
            if !type_equals(&Type::UInt, &matchee.get_type()) {
                return Err(format!(
                    "Expected type {:?} but got {:?}",
                    matchee.get_type(),
                    Type::UInt,
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

            let alternative = create_decision_tree(
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
                alternative: Box::new(alternative),
                type_,
            };

            Ok(decision)
        }
        Pattern::Float(v) => {
            if !type_equals(&Type::Float, &matchee.get_type()) {
                return Err(format!(
                    "Expected type {:?} but got {:?}",
                    matchee.get_type(),
                    Type::Float,
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

            let alternative = create_decision_tree(
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
                alternative: Box::new(alternative),
                type_,
            };

            Ok(decision)
        }
        Pattern::Char(v) => {
            if !type_equals(&Type::Char, &matchee.get_type()) {
                return Err(format!(
                    "Expected type {:?} but got {:?}",
                    matchee.get_type(),
                    Type::Char,
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

            let alternative = create_decision_tree(
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
                alternative: Box::new(alternative),
                type_,
            };

            Ok(decision)
        }
        Pattern::String(v) => {
            if !type_equals(&Type::String, &matchee.get_type()) {
                return Err(format!(
                    "Expected type {:?} but got {:?}",
                    matchee.get_type(),
                    Type::String,
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

            let alternative = create_decision_tree(
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
                alternative: Box::new(alternative),
                type_,
            };

            Ok(decision)
        }
        Pattern::Variable(ref identifier) => {
            let expression = &arm.expression;
            let type_environment = &arm.type_environment;
            let matchee_type = matchee.get_type();

            println!("identifier: {:?}", identifier);
            println!("matchee_type: {:?}", matchee_type);

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

            let variable = Variable {
                identifier: identifier.clone(),
                accessor: Accessor::Environment,
                type_: matchee_type.clone(),
            };

            let case = Case {
                pattern: Pattern::Variable(identifier.clone()),
                arguments: vec![variable.clone()],
                body: Decision::Success {
                    expression: Box::new(expression),
                    type_: type_.clone(),
                },
            };

            Ok(Decision::Switch {
                variable,
                cases: vec![case],
                fallback: Box::new(Decision::Failure {
                    error_message: "No match found".to_string(),
                }),
                type_,
            })
        }
        Pattern::Constructor(Constructor::Struct {
            type_annotation,
            field_patterns,
        }) => {
            if !type_annotation_equals(
                &matchee.get_type().type_annotation(),
                &type_annotation.clone(),
            ) {
                return Err(format!(
                    "Expected type annotation {} but got {}",
                    matchee.get_type().type_annotation(),
                    type_annotation,
                ));
            }

            let expression = &arm.expression;
            let type_environment = arm.type_environment.clone();
            let matchee_type = matchee.get_type();

            println!("struct_matchee: {:?}", matchee);
            println!("matchee_type: {:?}", matchee_type);

            let Type::Struct(Struct { fields, .. }) = matchee_type.clone() else {
                return Err(format!(
                    "Expected struct but got {:?}",
                    matchee_type.clone()
                ));
            };

            for field in fields.iter() {
                println!("field_ident: {:?}", field.0);
                println!("field_type: {:?}", field.1);
            }

            let field_name = field_patterns.first().unwrap().identifier.clone();
            let field_type = fields.get(&field_name).expect("testing matches").clone();

            let expr = TypedExpression::Member(Member::MemberAccess {
                object: Box::new(matchee.clone()),
                member: Box::new(Member::Identifier {
                    symbol: field_name.clone(),
                    type_: field_type.clone(),
                }),
                symbol: field_name.clone(),
                type_: field_type.clone(),
            });

            let case = Case {
                pattern: Pattern::Constructor(Constructor::Struct {
                    type_annotation: type_annotation.clone(),
                    field_patterns: field_patterns.clone(),
                }),
                arguments: fields
                    .iter()
                    .map(|(field, type_)| Variable {
                        identifier: field.clone(),
                        accessor: Accessor::Environment,
                        type_: type_.clone(),
                    })
                    .collect(),
                body: create_decision_tree(
                    expr.clone(),
                    field_patterns
                        .into_iter()
                        .map(|field_pattern| TypedMatchArm {
                            pattern: field_pattern.pattern,
                            expression: expression.clone(),
                            type_environment: type_environment.clone(),
                        })
                        .collect(),
                    discovered_types,
                    body_type.clone(),
                )?,
            };

            let fallback = create_decision_tree(
                matchee.clone(),
                arms.into_iter().skip(1).collect(),
                discovered_types,
                body_type,
            )?;

            Ok(Decision::Switch {
                variable: Variable {
                    identifier: field_name.clone(),
                    accessor: Accessor::Expression(Box::new(expr.clone())),
                    type_: field_type.clone(),
                },
                cases: vec![case],
                fallback: Box::new(fallback),
                type_: field_type.clone(),
            })
        }
    };

    decision
}
