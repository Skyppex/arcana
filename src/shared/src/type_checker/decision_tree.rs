use std::fmt::Display;

use crate::{
    type_checker::{
        ast::{BinaryOperator, Member, Typed},
        expressions::check_type,
        get_field_by_name, type_annotation_equals, type_equals, type_equals_coerce, Enum, Struct,
        Type,
    },
    types::{TypeAnnotation, TypeIdentifier},
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
    LessThan(Box<Pattern>),
    GreaterThan(Box<Pattern>),
    LessThanOrEqual(Box<Pattern>),
    GreaterThanOrEqual(Box<Pattern>),
    Range(Box<Pattern>, Box<Pattern>, bool),
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
            Pattern::LessThan(p) => write!(f, "<{}", p),
            Pattern::GreaterThan(p) => write!(f, ">{}", p),
            Pattern::LessThanOrEqual(p) => write!(f, "<={}", p),
            Pattern::GreaterThanOrEqual(p) => write!(f, ">={}", p),
            Pattern::Range(p1, p2, inclusive) => {
                write!(f, "{}..{}{}", p1, p2, if *inclusive { "=" } else { "" })
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
            let matchee_type = matchee.get_type();

            let mut is_enum_member = false;

            match &matchee_type {
                Type::Enum(Enum { members, .. }) => {
                    for member_type in members.values() {
                        if type_annotation_equals(&type_annotation, &member_type.type_annotation())
                        {
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
                        &type_annotation,
                        &TypeAnnotation::Type(discriminant_name.clone()),
                    ) {
                        is_enum_member = true;
                    }
                }
                _ => {}
            }

            if !is_enum_member
                && !type_annotation_equals(
                    &matchee_type.type_annotation(),
                    &type_annotation.clone(),
                )
            {
                return Err(format!(
                    "Expected type annotation {} but got {}",
                    matchee_type.type_annotation(),
                    type_annotation,
                ));
            }

            let expression = &arm.expression;
            let type_environment = arm.type_environment.clone();

            let fields = match matchee_type.clone() {
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
                        matchee_type.clone()
                    ));
                }
            };

            if fields.is_empty() {
                let expression =
                    check_type(expression, discovered_types, type_environment.clone(), None)?;

                let type_ = body_type.unwrap_or_else(|| expression.get_type());

                let alternative = create_decision_tree(
                    matchee.clone(),
                    arms.into_iter().skip(1).collect(),
                    discovered_types,
                    Some(type_.clone()),
                )?;

                let condition = if is_enum_member {
                    let Type::Enum(Enum {
                        type_identifier, ..
                    }) = &matchee_type
                    else {
                        return Err("Expected matchee to be an enum type".to_owned());
                    };

                    Box::new(TypedExpression::Binary {
                        left: Box::new(matchee),
                        operator: BinaryOperator::Equal,
                        right: Box::new(TypedExpression::Literal(
                            crate::type_checker::ast::Literal::Enum {
                                type_annotation: TypeAnnotation::from(
                                    format!(
                                        "{}::{}",
                                        type_identifier.name(),
                                        type_annotation.name()
                                    )
                                    .as_str(),
                                ),
                                field_initializers: vec![],
                                member: type_annotation.name(),
                                type_: matchee_type.clone(),
                            },
                        )),
                        type_: Type::Bool,
                    })
                } else {
                    Box::new(TypedExpression::Binary {
                        left: Box::new(matchee),
                        operator: BinaryOperator::Equal,
                        right: Box::new(TypedExpression::Literal(
                            crate::type_checker::ast::Literal::Struct {
                                type_annotation: type_annotation.clone(),
                                field_initializers: vec![],
                                type_: matchee_type.clone(),
                            },
                        )),
                        type_: Type::Bool,
                    })
                };

                return Ok(Decision::Guard {
                    condition,
                    consequence: Box::new(Decision::Success {
                        expression: Box::new(expression),
                        type_: type_.clone(),
                    }),
                    alternative: Box::new(alternative),
                    type_,
                });
            }

            let field_name = field_patterns.first().unwrap().identifier.clone();
            let field_type = get_field_by_name(&fields, &field_name)
                .expect("testing matches")
                .clone()
                .field_type;

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
                    .map(|struct_field| Variable {
                        identifier: struct_field.field_name.clone(),
                        accessor: Accessor::Environment,
                        type_: struct_field.field_type.clone(),
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
        Pattern::LessThan(value) => {
            if !matches!(
                *value,
                Pattern::Int(_) | Pattern::UInt(_) | Pattern::Float(_) | Pattern::Variable(_)
            ) {
                return Err("Expected Int, UInt, Float or Variable pattern".to_owned());
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
                arms.clone().into_iter().skip(1).collect(),
                discovered_types,
                Some(type_.clone()),
            )?;

            let decision = Decision::Guard {
                condition: Box::new(TypedExpression::Binary {
                    left: Box::new(matchee),
                    operator: BinaryOperator::LessThan,
                    right: match *value {
                        Pattern::Int(v) => Box::new(TypedExpression::Literal(
                            crate::type_checker::ast::Literal::Int(v),
                        )),
                        Pattern::UInt(v) => Box::new(TypedExpression::Literal(
                            crate::type_checker::ast::Literal::UInt(v),
                        )),
                        Pattern::Float(v) => Box::new(TypedExpression::Literal(
                            crate::type_checker::ast::Literal::Float(v),
                        )),
                        Pattern::Variable(v) => {
                            let variable_type = type_environment
                                .borrow()
                                .get_variable(&v)
                                .expect("testing matches");

                            Box::new(TypedExpression::Member(Member::Identifier {
                                symbol: v.clone(),
                                type_: variable_type.clone(),
                            }))
                        }
                        _ => unreachable!(
                            "Already checked if the value is Int, UInt, Float or Identifier"
                        ),
                    },
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
        Pattern::GreaterThan(value) => {
            if !matches!(
                *value,
                Pattern::Int(_) | Pattern::UInt(_) | Pattern::Float(_) | Pattern::Variable(_)
            ) {
                return Err("Expected Int, UInt, Float or Variable pattern".to_owned());
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
                arms.clone().into_iter().skip(1).collect(),
                discovered_types,
                Some(type_.clone()),
            )?;

            let decision = Decision::Guard {
                condition: Box::new(TypedExpression::Binary {
                    left: Box::new(matchee),
                    operator: BinaryOperator::GreaterThan,
                    right: match *value {
                        Pattern::Int(v) => Box::new(TypedExpression::Literal(
                            crate::type_checker::ast::Literal::Int(v),
                        )),
                        Pattern::UInt(v) => Box::new(TypedExpression::Literal(
                            crate::type_checker::ast::Literal::UInt(v),
                        )),
                        Pattern::Float(v) => Box::new(TypedExpression::Literal(
                            crate::type_checker::ast::Literal::Float(v),
                        )),
                        Pattern::Variable(v) => {
                            let variable_type = type_environment
                                .borrow()
                                .get_variable(&v)
                                .expect("testing matches");

                            Box::new(TypedExpression::Member(Member::Identifier {
                                symbol: v.clone(),
                                type_: variable_type.clone(),
                            }))
                        }
                        _ => unreachable!(
                            "Already checked if the value is Int, UInt, Float or Identifier"
                        ),
                    },
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
        Pattern::LessThanOrEqual(value) => {
            if !matches!(
                *value,
                Pattern::Int(_) | Pattern::UInt(_) | Pattern::Float(_) | Pattern::Variable(_)
            ) {
                return Err("Expected Int, UInt, Float or Variable pattern".to_owned());
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
                arms.clone().into_iter().skip(1).collect(),
                discovered_types,
                Some(type_.clone()),
            )?;

            let decision = Decision::Guard {
                condition: Box::new(TypedExpression::Binary {
                    left: Box::new(matchee),
                    operator: BinaryOperator::LessThanOrEqual,
                    right: match *value {
                        Pattern::Int(v) => Box::new(TypedExpression::Literal(
                            crate::type_checker::ast::Literal::Int(v),
                        )),
                        Pattern::UInt(v) => Box::new(TypedExpression::Literal(
                            crate::type_checker::ast::Literal::UInt(v),
                        )),
                        Pattern::Float(v) => Box::new(TypedExpression::Literal(
                            crate::type_checker::ast::Literal::Float(v),
                        )),
                        Pattern::Variable(v) => {
                            let variable_type = type_environment
                                .borrow()
                                .get_variable(&v)
                                .expect("testing matches");

                            Box::new(TypedExpression::Member(Member::Identifier {
                                symbol: v.clone(),
                                type_: variable_type.clone(),
                            }))
                        }
                        _ => unreachable!(
                            "Already checked if the value is Int, UInt, Float or Identifier"
                        ),
                    },
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
        Pattern::GreaterThanOrEqual(value) => {
            if !matches!(
                *value,
                Pattern::Int(_) | Pattern::UInt(_) | Pattern::Float(_) | Pattern::Variable(_)
            ) {
                return Err("Expected Int, UInt, Float or Variable pattern".to_owned());
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
                arms.clone().into_iter().skip(1).collect(),
                discovered_types,
                Some(type_.clone()),
            )?;

            let decision = Decision::Guard {
                condition: Box::new(TypedExpression::Binary {
                    left: Box::new(matchee),
                    operator: BinaryOperator::GreaterThanOrEqual,
                    right: match *value {
                        Pattern::Int(v) => Box::new(TypedExpression::Literal(
                            crate::type_checker::ast::Literal::Int(v),
                        )),
                        Pattern::UInt(v) => Box::new(TypedExpression::Literal(
                            crate::type_checker::ast::Literal::UInt(v),
                        )),
                        Pattern::Float(v) => Box::new(TypedExpression::Literal(
                            crate::type_checker::ast::Literal::Float(v),
                        )),
                        Pattern::Variable(v) => {
                            let variable_type = type_environment
                                .borrow()
                                .get_variable(&v)
                                .expect("testing matches");

                            Box::new(TypedExpression::Member(Member::Identifier {
                                symbol: v.clone(),
                                type_: variable_type.clone(),
                            }))
                        }
                        _ => unreachable!(
                            "Already checked if the value is Int, UInt, Float or Identifier"
                        ),
                    },
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
        Pattern::Range(left, right, inclusive) => {
            if !matches!(
                (*left.clone(), *right.clone()),
                (Pattern::Int(_), Pattern::Int(_))
                    | (Pattern::Variable(_), Pattern::Int(_))
                    | (Pattern::Int(_), Pattern::Variable(_))
                    | (Pattern::UInt(_), Pattern::UInt(_))
                    | (Pattern::Variable(_), Pattern::UInt(_))
                    | (Pattern::UInt(_), Pattern::Variable(_))
                    | (Pattern::Float(_), Pattern::Float(_))
                    | (Pattern::Variable(_), Pattern::Float(_))
                    | (Pattern::Float(_), Pattern::Variable(_))
                    | (Pattern::Variable(_), Pattern::Variable(_))
            ) {
                return Err("Expected Int, UInt, Float or Variable pattern".to_owned());
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
                arms.clone().into_iter().skip(1).collect(),
                discovered_types,
                Some(type_.clone()),
            )?;

            let decision = Decision::Guard {
                condition: Box::new(TypedExpression::Binary {
                    left: Box::new(TypedExpression::Binary {
                        left: Box::new(matchee.clone()),
                        operator: BinaryOperator::GreaterThanOrEqual,
                        right: match *left {
                            Pattern::Int(v) => Box::new(TypedExpression::Literal(
                                crate::type_checker::ast::Literal::Int(v),
                            )),
                            Pattern::UInt(v) => Box::new(TypedExpression::Literal(
                                crate::type_checker::ast::Literal::UInt(v),
                            )),
                            Pattern::Float(v) => Box::new(TypedExpression::Literal(
                                crate::type_checker::ast::Literal::Float(v),
                            )),
                            Pattern::Variable(v) => {
                                let variable_type = type_environment
                                    .borrow()
                                    .get_variable(&v)
                                    .expect("testing matches");

                                Box::new(TypedExpression::Member(Member::Identifier {
                                    symbol: v.clone(),
                                    type_: variable_type.clone(),
                                }))
                            }
                            _ => unreachable!(
                                "Already checked if the value is Int, UInt, Float or Identifier"
                            ),
                        },
                        type_: Type::Bool,
                    }),
                    operator: BinaryOperator::LogicalAnd,
                    right: Box::new(TypedExpression::Binary {
                        left: Box::new(matchee),
                        operator: if inclusive {
                            BinaryOperator::LessThanOrEqual
                        } else {
                            BinaryOperator::LessThan
                        },
                        right: match *right {
                            Pattern::Int(v) => Box::new(TypedExpression::Literal(
                                crate::type_checker::ast::Literal::Int(v),
                            )),
                            Pattern::UInt(v) => Box::new(TypedExpression::Literal(
                                crate::type_checker::ast::Literal::UInt(v),
                            )),
                            Pattern::Float(v) => Box::new(TypedExpression::Literal(
                                crate::type_checker::ast::Literal::Float(v),
                            )),
                            Pattern::Variable(v) => {
                                let variable_type = type_environment
                                    .borrow()
                                    .get_variable(&v)
                                    .expect("testing matches");

                                Box::new(TypedExpression::Member(Member::Identifier {
                                    symbol: v.clone(),
                                    type_: variable_type.clone(),
                                }))
                            }
                            _ => unreachable!(
                                "Already checked if the value is Int, UInt, Float or Identifier"
                            ),
                        },
                        type_: Type::Bool,
                    }),
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
    };

    decision
}
