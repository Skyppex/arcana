use std::{cell::RefCell, ops::Deref, rc::Rc};

use shared::{
    ast::{ModPath, UseItem},
    built_in::BuiltInFunction,
    lexer::token::IdentifierType,
    type_checker::{
        decision_tree::{Accessor, Constructor, Decision, FieldPattern, Pattern},
        model::*,
        type_annotation_equals, Type,
    },
    types::{ToKey, TypeAnnotation, TypeIdentifier},
};

use crate::value::{get_built_in_function_value, FunctionBody, Variable};

use super::{
    environment::{Environment, Rcrc},
    evaluate_binop,
    scope::ScopeType,
    value::{self, Enum, Number, Struct, Value},
    Scope,
};

pub fn evaluate(
    typed_statement: TypedStatement,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    match typed_statement {
        TypedStatement::None => Ok(Value::Void),
        TypedStatement::Program { statements } => evaluate_program(statements, environment),
        TypedStatement::ModuleDeclaration { .. } => Ok(Value::Void),
        TypedStatement::Use { use_item, .. } => {
            let imports = evaluate_use_item(use_item, ModPath::root(), environment.clone())?;

            for import in imports {
                let borrow = import.borrow();
                environment.borrow_mut().add_variable(
                    borrow.identifier.clone(),
                    borrow.value.clone(),
                    false,
                );
            }

            Ok(Value::Void)
        }
        TypedStatement::StructDeclaration { .. } => Ok(Value::Void),
        TypedStatement::EnumDeclaration { .. } => Ok(Value::Void),
        TypedStatement::UnionDeclaration { .. } => Ok(Value::Void),
        TypedStatement::TypeAliasDeclaration { .. } => Ok(Value::Void),
        TypedStatement::ProtocolDeclaration { .. } => Ok(Value::Void),
        TypedStatement::ImplementationDeclaration {
            type_annotation,
            functions,
            ..
        } => evaluate_implementation_declaration(environment, type_annotation, functions),
        TypedStatement::FunctionDeclaration {
            type_identifier: identifier,
            param,
            body,
            ..
        } => evaluate_function_declaration(environment, identifier, param, body),
        TypedStatement::Semi(s) => {
            evaluate(*s, environment)?;
            Ok(Value::Void)
        }
        TypedStatement::Expression(e) => evaluate_expression(e, environment),
    }
}

fn evaluate_use_item(
    use_item: UseItem,
    module_path: ModPath,
    environment: Rcrc<Environment>,
) -> Result<Vec<Rcrc<Variable>>, String> {
    match use_item {
        UseItem::Item(item_name) => {
            if item_name.is_type_identifier_name() {
                return Ok(vec![]);
            }

            let (_, mod_environment) = environment
                .borrow()
                .get_module(&module_path)
                .ok_or(format!("Module '{}' not found", module_path))?;

            let function = mod_environment
                .borrow()
                .get_function(&item_name)
                .ok_or(format!("Couldn't find function '{}'", item_name))?;

            Ok(vec![function])
        }
        UseItem::Navigation(mod_name, use_item) => {
            evaluate_use_item(*use_item, module_path.join(mod_name), environment)
        }
        UseItem::List(use_items) => Ok(use_items
            .iter()
            .map(|use_item| {
                evaluate_use_item(use_item.clone(), module_path.clone(), environment.clone())
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()),
    }
}

fn evaluate_implementation_declaration(
    environment: Rc<RefCell<Environment>>,
    type_annotation: TypeAnnotation,
    functions: Vec<(String, TypedStatement)>,
) -> Result<Value, String> {
    for (function_name, function) in functions {
        let _ = evaluate(function, environment.clone())?;

        let variable = environment
            .borrow()
            .get_function(&function_name)
            .ok_or(format!("Function '{}' not found", function_name))?
            .clone();

        environment
            .borrow_mut()
            .add_static_member(&type_annotation, function_name, variable);
    }

    Ok(Value::Void)
}

fn evaluate_function_declaration(
    environment: Rc<RefCell<Environment>>,
    identifier: TypeIdentifier,
    param: Option<TypedParameter>,
    body: Option<TypedExpression>,
) -> Result<Value, String> {
    let Some(body) = body else {
        return Err(format!("Function '{}' must have a body", identifier));
    };

    let function_environment = Rc::new(RefCell::new(environment.deref().clone().borrow().clone()));

    let function = Value::Function {
        param_name: param.map(|p| p.identifier),
        body: FunctionBody::Expr(body),
        environment: function_environment.clone(),
    };

    function_environment
        .borrow_mut()
        .add_function(&identifier, function.clone(), false);

    environment
        .borrow_mut()
        .add_function(&identifier, function, false);

    Ok(Value::Void)
}

pub(super) fn evaluate_expression(
    typed_expression: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    match typed_expression {
        // TypedExpression::None => Ok(Value::Void),
        TypedExpression::VariableDeclaration {
            mutable,
            pattern,
            initializer,
            ..
        } => evaluate_variable_declaration(mutable, pattern, initializer, environment),
        TypedExpression::If {
            condition,
            true_expression,
            false_expression,
            ..
        } => evaluate_if(condition, true_expression, false_expression, environment),
        TypedExpression::Match {
            expression,
            decision_tree,
            ..
        } => evaluate_match(expression, decision_tree, environment),
        TypedExpression::Assignment {
            member,
            initializer,
            ..
        } => evaluate_assignment(member, initializer, environment),
        TypedExpression::Member(m) => evaluate_member(m, environment),
        TypedExpression::Literal(l) => evaluate_literal(l, environment),
        TypedExpression::Tuple { elements, .. } => evaluate_tuple(elements, environment),
        TypedExpression::Closure {
            param,
            return_type: _,
            body,
            ..
        } => evaluate_closure(param, *body, environment),
        TypedExpression::Call {
            callee,
            argument,
            type_,
        } => evaluate_call(callee, argument, type_, environment),
        TypedExpression::Unary {
            operator,
            expression,
            ..
        } => evaluate_unary(operator, expression, environment),
        TypedExpression::Binary {
            left,
            operator,
            right,
            ..
        } => evaluate_binary(left, operator, right, environment),
        TypedExpression::Block(Block { statements, .. }) => evaluate_block(statements, environment),
        TypedExpression::Loop { body, .. } => evaluate_loop(body, environment),
        TypedExpression::While {
            condition,
            body,
            else_body,
            ..
        } => evaluate_while(condition, body, else_body, environment),
        TypedExpression::For {
            pattern,
            iterable,
            body,
            else_body,
            ..
        } => evaluate_for(pattern, iterable, body, else_body, environment),
        TypedExpression::Break(e) => evaluate_break(e, environment),
        TypedExpression::Continue => evaluate_continue(environment),
        TypedExpression::Return(e) => evaluate_return(e, environment),
    }
}

fn evaluate_tuple(
    elements: Vec<TypedExpression>,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let mut tuple = Vec::new();

    for element in elements {
        tuple.push(evaluate_expression(element, environment.clone())?);
    }

    Ok(Value::Tuple(tuple))
}

fn evaluate_program(
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

fn evaluate_variable_declaration(
    mutable: bool,
    pattern: Pattern,
    initializer: Option<Box<TypedExpression>>,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let value = match initializer {
        Some(initializer) => evaluate_expression(*initializer, environment.clone())?,
        None => Value::Uninitialized,
    };

    if let Some(bindings) = evaluate_pattern(&pattern, &value, environment.clone())? {
        for (identifier, value) in bindings {
            environment
                .borrow_mut()
                .add_variable(identifier, value, mutable);
        }
    }

    Ok(value)
}

fn evaluate_if(
    condition: Box<TypedExpression>,
    true_expression: Box<TypedExpression>,
    false_expression: Option<Box<TypedExpression>>,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let if_environment = Rc::new(RefCell::new(Environment::new_parent(environment.clone())));
    let condition = evaluate_expression(*condition, if_environment.clone())?;

    match condition {
        Value::Bool(v) => {
            if v {
                let value = evaluate_expression(*true_expression, if_environment)?;
                if false_expression.is_none() {
                    Ok(Value::option_some(value))
                } else {
                    Ok(value)
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
    decision_tree: Decision,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let value = evaluate_expression(*expression, environment.clone())?;
    let match_environment = Rc::new(RefCell::new(Environment::new_parent(environment.clone())));

    evaluate_decision_tree(decision_tree, value, match_environment)
}

fn evaluate_decision_tree(
    decision_tree: Decision,
    value: Value,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    match decision_tree {
        Decision::Success { expression, .. } => evaluate_expression(*expression, environment),
        Decision::Failure { error_message } => Err(error_message),
        Decision::Guard {
            condition,
            consequence: true_,
            alternative: false_,
            type_: _,
        } => {
            let condition = evaluate_expression(*condition, environment.clone())?;

            match condition {
                Value::Bool(v) => {
                    if v {
                        evaluate_decision_tree(*true_, value, environment)
                    } else {
                        evaluate_decision_tree(*false_, value, environment)
                    }
                }
                _ => Err(format!(
                    "Match guard condition must be boolean '{}'",
                    condition
                )),
            }
        }
        Decision::Switch {
            variable,
            cases,
            fallback,
            type_: _,
        } => {
            let switch_value = match variable.accessor {
                Accessor::Environment => {
                    let Some(variable) = environment.borrow().get_variable(&variable.identifier)
                    else {
                        return Err(format!(
                            "Variable '{}' not found in environment",
                            variable.identifier
                        ));
                    };

                    let value = variable.borrow().value.clone();
                    value
                }
                Accessor::Expression(expression) => {
                    let argument_value = evaluate_expression(*expression, environment.clone())?;

                    environment.borrow_mut().add_variable(
                        variable.identifier.clone(),
                        argument_value.clone(),
                        false,
                    );

                    argument_value
                }
            };

            for case in cases {
                let pattern = case.pattern;
                let arguments = case.arguments;
                let body = case.body;

                let case_environment =
                    Rc::new(RefCell::new(Environment::new_parent(environment.clone())));

                for argument in arguments {
                    match argument.accessor {
                        Accessor::Environment => continue,
                        Accessor::Expression(expression) => {
                            let argument_value =
                                evaluate_expression(*expression, environment.clone())?;

                            case_environment.borrow_mut().add_variable(
                                argument.identifier,
                                argument_value,
                                false,
                            );
                        }
                    }
                }

                if let Some(bindings) = evaluate_pattern(&pattern, &value, environment.clone())? {
                    for (identifier, value) in bindings {
                        case_environment
                            .borrow_mut()
                            .add_variable(identifier, value, false);
                    }

                    return evaluate_decision_tree(body, switch_value, case_environment);
                }
            }

            evaluate_decision_tree(*fallback, value, environment)
        }
    }
}

fn evaluate_pattern(
    pattern: &Pattern,
    value: &Value,
    environment: Rcrc<Environment>,
) -> Result<Option<Vec<(String, Value)>>, String> {
    match pattern {
        Pattern::Wildcard => Ok(Some(Vec::new())),
        Pattern::Unit => match value {
            Value::Unit => Ok(Some(Vec::new())),
            _ => Err(format!("Expected unit, found '{}'", value)),
        },
        Pattern::Bool(v) => match value {
            Value::Bool(v2) => {
                if *v == *v2 {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            _ => Err(format!("Expected boolean, found '{}'", value)),
        },
        Pattern::Int(v) => match value {
            Value::Number(Number::Int(v2)) => {
                if *v == *v2 {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            _ => Err(format!("Expected int, found '{}'", value)),
        },
        Pattern::UInt(v) => match value {
            Value::Number(Number::UInt(v2)) => {
                if *v == *v2 {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            _ => Err(format!("Expected uint, found '{}'", value)),
        },
        Pattern::Float(v) => match value {
            Value::Number(Number::Float(v2)) => {
                if *v == *v2 {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            _ => Err(format!("Expected float, found '{}'", value)),
        },
        Pattern::Char(v) => match value {
            Value::Char(v2) => {
                if *v == *v2 {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            _ => Err(format!("Expected char, found '{}'", value)),
        },
        Pattern::String(v) => match value {
            Value::String(v2) => {
                if *v == *v2 {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            _ => Err(format!("Expected string, found '{}'", value)),
        },
        Pattern::Variable(v) => Ok(Some(vec![(v.clone(), value.clone())])),
        Pattern::Constructor(Constructor::Struct {
            type_annotation,
            field_patterns,
        }) => {
            let fields = match value {
                Value::Struct(Struct { type_name, fields }) => {
                    if type_name != &type_annotation.to_key() {
                        return Err(format!(
                            "Expected struct '{}', found struct '{}'",
                            type_annotation, type_name
                        ));
                    }

                    fields
                }
                Value::Enum(Enum {
                    enum_member: Struct { type_name, fields },
                    ..
                }) => {
                    let member_type_annotation = TypeAnnotation::Type(type_name.clone());

                    if !type_annotation_equals(&member_type_annotation, type_annotation) {
                        return Err(format!(
                            "Expected enum '{}', found '{}'",
                            type_annotation, type_name
                        ));
                    }

                    fields
                }
                _ => return Err(format!("Expected enum, found '{}'", value)),
            };

            let mut bindings = Vec::new();

            for FieldPattern {
                identifier,
                pattern,
            } in field_patterns
            {
                let field_value =
                    fields
                        .iter()
                        .find(|f| &f.identifier == identifier)
                        .ok_or(format!(
                            "Field '{}' not found in struct '{}'",
                            identifier, type_annotation
                        ))?;

                match evaluate_pattern(pattern, &field_value.value, environment.clone())? {
                    Some(mut bindings_) => bindings.append(&mut bindings_),
                    None => {
                        return Ok(None);
                    }
                }
            }

            Ok(Some(bindings))
        }
        Pattern::LessThan(v) => match (v.as_ref(), value) {
            (Pattern::Int(v), Value::Number(Number::Int(v2))) => {
                if *v2 < *v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::Variable(v), Value::Number(Number::Int(v2))) => {
                let Some(variable) = environment.borrow().get_variable(v) else {
                    return Err(format!("Variable '{}' not found", v));
                };

                let Value::Number(Number::Int(v)) = variable.borrow().value.clone() else {
                    return Err(format!("Expected int, found '{}'", variable.borrow().value));
                };

                if *v2 < v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::UInt(v), Value::Number(Number::UInt(v2))) => {
                if *v2 < *v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::Variable(v), Value::Number(Number::UInt(v2))) => {
                let Some(variable) = environment.borrow().get_variable(v) else {
                    return Err(format!("Variable '{}' not found", v));
                };

                let Value::Number(Number::UInt(v)) = variable.borrow().value.clone() else {
                    return Err(format!("Expected int, found '{}'", variable.borrow().value));
                };

                if *v2 < v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::Float(v), Value::Number(Number::Float(v2))) => {
                if *v2 < *v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::Variable(v), Value::Number(Number::Float(v2))) => {
                let Some(variable) = environment.borrow().get_variable(v) else {
                    return Err(format!("Variable '{}' not found", v));
                };

                let Value::Number(Number::Float(v)) = variable.borrow().value.clone() else {
                    return Err(format!("Expected int, found '{}'", variable.borrow().value));
                };

                if *v2 < v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            other => Err(format!("Unexpected pattern: '{:?}'", other)),
        },
        Pattern::GreaterThan(v) => match (v.as_ref(), value) {
            (Pattern::Int(v), Value::Number(Number::Int(v2))) => {
                if *v2 > *v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::Variable(v), Value::Number(Number::Int(v2))) => {
                let Some(variable) = environment.borrow().get_variable(v) else {
                    return Err(format!("Variable '{}' not found", v));
                };

                let Value::Number(Number::Int(v)) = variable.borrow().value.clone() else {
                    return Err(format!("Expected int, found '{}'", variable.borrow().value));
                };

                if *v2 > v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::UInt(v), Value::Number(Number::UInt(v2))) => {
                if *v2 > *v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::Variable(v), Value::Number(Number::UInt(v2))) => {
                let Some(variable) = environment.borrow().get_variable(v) else {
                    return Err(format!("Variable '{}' not found", v));
                };

                let Value::Number(Number::UInt(v)) = variable.borrow().value.clone() else {
                    return Err(format!("Expected int, found '{}'", variable.borrow().value));
                };

                if *v2 > v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::Float(v), Value::Number(Number::Float(v2))) => {
                if *v2 > *v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::Variable(v), Value::Number(Number::Float(v2))) => {
                let Some(variable) = environment.borrow().get_variable(v) else {
                    return Err(format!("Variable '{}' not found", v));
                };

                let Value::Number(Number::Float(v)) = variable.borrow().value.clone() else {
                    return Err(format!("Expected int, found '{}'", variable.borrow().value));
                };

                if *v2 > v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            other => Err(format!("Unexpected pattern: '{:?}'", other)),
        },
        Pattern::LessThanOrEqual(v) => match (v.as_ref(), value) {
            (Pattern::Int(v), Value::Number(Number::Int(v2))) => {
                if *v2 <= *v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::Variable(v), Value::Number(Number::Int(v2))) => {
                let Some(variable) = environment.borrow().get_variable(v) else {
                    return Err(format!("Variable '{}' not found", v));
                };

                let Value::Number(Number::Int(v)) = variable.borrow().value.clone() else {
                    return Err(format!("Expected int, found '{}'", variable.borrow().value));
                };

                if *v2 <= v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::UInt(v), Value::Number(Number::UInt(v2))) => {
                if *v2 <= *v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::Variable(v), Value::Number(Number::UInt(v2))) => {
                let Some(variable) = environment.borrow().get_variable(v) else {
                    return Err(format!("Variable '{}' not found", v));
                };

                let Value::Number(Number::UInt(v)) = variable.borrow().value.clone() else {
                    return Err(format!("Expected int, found '{}'", variable.borrow().value));
                };

                if *v2 <= v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::Float(v), Value::Number(Number::Float(v2))) => {
                if *v2 <= *v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::Variable(v), Value::Number(Number::Float(v2))) => {
                let Some(variable) = environment.borrow().get_variable(v) else {
                    return Err(format!("Variable '{}' not found", v));
                };

                let Value::Number(Number::Float(v)) = variable.borrow().value.clone() else {
                    return Err(format!("Expected int, found '{}'", variable.borrow().value));
                };

                if *v2 <= v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            other => Err(format!("Unexpected pattern: '{:?}'", other)),
        },
        Pattern::GreaterThanOrEqual(v) => match (v.as_ref(), value) {
            (Pattern::Int(v), Value::Number(Number::Int(v2))) => {
                if *v2 >= *v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::Variable(v), Value::Number(Number::Int(v2))) => {
                let Some(variable) = environment.borrow().get_variable(v) else {
                    return Err(format!("Variable '{}' not found", v));
                };

                let Value::Number(Number::Int(v)) = variable.borrow().value.clone() else {
                    return Err(format!("Expected int, found '{}'", variable.borrow().value));
                };

                if *v2 >= v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::UInt(v), Value::Number(Number::UInt(v2))) => {
                if *v2 >= *v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::Variable(v), Value::Number(Number::UInt(v2))) => {
                let Some(variable) = environment.borrow().get_variable(v) else {
                    return Err(format!("Variable '{}' not found", v));
                };

                let Value::Number(Number::UInt(v)) = variable.borrow().value.clone() else {
                    return Err(format!("Expected int, found '{}'", variable.borrow().value));
                };

                if *v2 >= v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::Float(v), Value::Number(Number::Float(v2))) => {
                if *v2 >= *v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            (Pattern::Variable(v), Value::Number(Number::Float(v2))) => {
                let Some(variable) = environment.borrow().get_variable(v) else {
                    return Err(format!("Variable '{}' not found", v));
                };

                let Value::Number(Number::Float(v)) = variable.borrow().value.clone() else {
                    return Err(format!("Expected int, found '{}'", variable.borrow().value));
                };

                if *v2 >= v {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            }
            other => Err(format!("Unexpected pattern: '{:?}'", other)),
        },
        Pattern::Tuple(patterns) => {
            let Value::Tuple(values) = value else {
                return Err(format!("Expected tuple, found '{}'", value));
            };

            let mut bindings = vec![];

            for (pattern, value) in patterns.iter().zip(values) {
                let Some(new_bindings) = evaluate_pattern(pattern, value, environment.clone())?
                else {
                    return Ok(None);
                };

                bindings.extend(new_bindings);
            }

            Ok(Some(bindings))
        }
        Pattern::Range(left, right, inclusive) => {
            let left = match left.as_ref() {
                Pattern::Int(v) => Value::Number(Number::Int(*v)),
                Pattern::UInt(v) => Value::Number(Number::UInt(*v)),
                Pattern::Float(v) => Value::Number(Number::Float(*v)),
                Pattern::Variable(v) => {
                    let variable = environment
                        .borrow()
                        .get_variable(v)
                        .ok_or(format!("Variable '{}' not found in env", v))?;

                    let value = variable.borrow().value.clone();

                    if !matches!(value, Value::Number(_)) {
                        return Err(format!("Expected number, found '{}'", value));
                    }

                    value
                }
                other => return Err(format!("Unexpected pattern: '{:?}'", other)),
            };

            let right = match right.as_ref() {
                Pattern::Int(v) => Value::Number(Number::Int(*v)),
                Pattern::UInt(v) => Value::Number(Number::UInt(*v)),
                Pattern::Float(v) => Value::Number(Number::Float(*v)),
                Pattern::Variable(v) => {
                    let variable = environment
                        .borrow()
                        .get_variable(v)
                        .ok_or(format!("Variable '{}' not found in env", v))?;

                    let value = variable.borrow().value.clone();

                    if !matches!(value, Value::Number(_)) {
                        return Err(format!("Expected number, found '{}'", value));
                    }

                    value
                }
                other => return Err(format!("Unexpected pattern: '{:?}'", other)),
            };

            match (left, value, right) {
                (
                    Value::Number(Number::Int(left)),
                    Value::Number(Number::Int(v)),
                    Value::Number(Number::Int(right)),
                ) => {
                    if *inclusive {
                        if *v >= left && *v <= right {
                            Ok(Some(Vec::new()))
                        } else {
                            Ok(None)
                        }
                    } else if *v >= left && *v < right {
                        Ok(Some(Vec::new()))
                    } else {
                        Ok(None)
                    }
                }
                (
                    Value::Number(Number::UInt(left)),
                    Value::Number(Number::UInt(v)),
                    Value::Number(Number::UInt(right)),
                ) => {
                    if *inclusive {
                        if *v >= left && *v <= right {
                            Ok(Some(Vec::new()))
                        } else {
                            Ok(None)
                        }
                    } else if *v >= left && *v < right {
                        Ok(Some(Vec::new()))
                    } else {
                        Ok(None)
                    }
                }
                (
                    Value::Number(Number::Float(left)),
                    Value::Number(Number::Float(v)),
                    Value::Number(Number::Float(right)),
                ) => {
                    if *inclusive {
                        if *v >= left && *v <= right {
                            Ok(Some(Vec::new()))
                        } else {
                            Ok(None)
                        }
                    } else if *v >= left && *v < right {
                        Ok(Some(Vec::new()))
                    } else {
                        Ok(None)
                    }
                }
                other => Err(format!(
                    "Expected number, found '{}', '{}', '{}'",
                    other.0, other.1, other.2
                )),
            }
        }
    }
}

fn evaluate_assignment(
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

fn evaluate_member(member: Member, environment: Rcrc<Environment>) -> Result<Value, String> {
    match member.clone() {
        Member::Identifier { symbol, .. } => Ok(environment
            .borrow()
            .get_variable(&symbol)
            .or_else(|| get_built_in_function(&symbol, environment.clone()))
            .or_else(|| environment.borrow().get_function(&member))
            .ok_or(format!("Variable '{}' not found", &member))?
            .borrow()
            .value
            .clone()),
        Member::StaticMemberAccess {
            type_annotation,
            member,
            ..
        } => evaluate_static_member_access(type_annotation.clone(), environment, member.clone()),
        Member::MemberAccess { object, member, .. } => {
            evaluate_member_access(object.clone(), environment, member.clone())
        }
        Member::BuiltInFunction(built_in_function) => {
            Ok(evaluate_built_in_function(built_in_function, environment))
        }
        Member::Index { object, index, .. } => evaluate_index(object, index, environment),
    }
}

fn evaluate_static_member_access(
    type_annotation: TypeAnnotation,
    environment: Rcrc<Environment>,
    member: Box<Member>,
) -> Result<Value, String> {
    Ok(environment
        .borrow()
        .get_static_member(&type_annotation, member.get_symbol())
        .ok_or(format!(
            "Static member '{}' not found in struct '{}'",
            member, type_annotation
        ))?
        .borrow()
        .value
        .clone())
}

fn evaluate_member_access(
    object: Box<TypedExpression>,
    environment: Rcrc<Environment>,
    member: Box<Member>,
) -> Result<Value, String> {
    let value = evaluate_expression(*object, environment.clone())?;

    match value {
        Value::Struct(Struct { type_name, fields }) => match *member.clone() {
            Member::Identifier { symbol, .. } => {
                let field = fields
                    .iter()
                    .find(|f| f.identifier == symbol)
                    .ok_or(format!(
                        "Field '{}' not found in struct '{}'",
                        symbol, type_name
                    ))?;

                Ok(field.value.clone())
            }
            Member::StaticMemberAccess { .. } => Err(format!(
                "Cannot access static member on an instance of a struct '{}'",
                type_name
            )),
            Member::MemberAccess { object, member, .. } => {
                evaluate_member_access(object, environment, member)
            }
            Member::BuiltInFunction(_) => {
                panic!("Cannot access members on a built-in function")
            }
            Member::Index { .. } => todo!("members on an indexed expression"),
        },
        Value::Enum(Enum {
            enum_member: Struct { type_name, fields },
            ..
        }) => match *member.clone() {
            Member::Identifier { symbol, .. } => {
                let field = fields
                    .iter()
                    .find(|f| f.identifier == symbol)
                    .ok_or(format!(
                        "Field '{}' not found in enum member '{}'",
                        symbol, type_name
                    ))?;

                Ok(field.value.clone())
            }
            Member::StaticMemberAccess { .. } => Err(format!(
                "Cannot access static member on an instance of a enum '{}'",
                type_name
            )),
            Member::MemberAccess { object, member, .. } => {
                evaluate_member_access(object, environment, member)
            }
            Member::BuiltInFunction(_) => {
                panic!("Cannot access members on a built-in function")
            }
            Member::Index { .. } => todo!("members on an indexed expression"),
        },
        _ => Err(format!("Cannot access member value: '{}'", value)),
    }
}

fn evaluate_literal(
    literal: ValueLiteral,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    match literal {
        ValueLiteral::Void => panic!("Void literals should never be evaluated"),
        ValueLiteral::Unit => Ok(Value::Unit),
        ValueLiteral::Int(v) => Ok(Value::Number(Number::Int(v))),
        ValueLiteral::UInt(v) => Ok(Value::Number(Number::UInt(v))),
        ValueLiteral::Float(v) => Ok(Value::Number(Number::Float(v))),
        ValueLiteral::String(v) => Ok(Value::String(v)),
        ValueLiteral::Char(v) => Ok(Value::Char(v)),
        ValueLiteral::Bool(v) => Ok(Value::Bool(v)),
        ValueLiteral::Array { values, .. } => {
            let mut array = Vec::new();

            for element in values {
                array.push(evaluate_expression(element, environment.clone())?);
            }

            Ok(Value::Array(array))
        }
        ValueLiteral::Struct {
            type_annotation,
            field_initializers,
            ..
        } => {
            let mut fields = vec![];

            for field_initializer in field_initializers {
                fields.push(value::StructField {
                    identifier: field_initializer.identifier,
                    value: evaluate_expression(field_initializer.initializer, environment.clone())?,
                });
            }

            Ok(Value::Struct(Struct {
                type_name: type_annotation.to_key(),
                fields,
            }))
        }
        ValueLiteral::Enum {
            type_annotation,
            field_initializers,
            ..
        } => {
            let mut fields = vec![];

            for FieldInitializer {
                identifier,
                initializer,
            } in field_initializers
            {
                fields.push(value::StructField {
                    identifier,
                    value: evaluate_expression(initializer, environment.clone())?,
                });
            }

            Ok(Value::Enum(Enum {
                type_name: type_annotation.to_key(),
                enum_member: Struct {
                    type_name: type_annotation.to_key(),
                    fields,
                },
            }))
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
        body: FunctionBody::Expr(body),
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

    let Value::Function {
        param_name,
        body,
        environment,
    } = callee_value
    else {
        return Err(format!("Cannot call non-function value '{}'", callee_value));
    };

    let function_environment = Rc::new(RefCell::new(Environment::new_scope(
        environment.clone(),
        ScopeType::Return,
    )));

    if let Some(evaluated_arg) = &evaluated_arg {
        function_environment.borrow_mut().add_variable(
            param_name.clone().unwrap(),
            evaluated_arg.clone(),
            false,
        );
    }

    let mut value = match body {
        FunctionBody::Expr(body) => evaluate_expression(body, function_environment.clone())?,
        FunctionBody::Fn(function) => function(evaluated_arg),
    };

    if let Some(Scope::Return(v)) = function_environment.borrow().get_scope(&ScopeType::Return) {
        match v {
            Some(v) => {
                if type_ == Type::Void {
                    return Err("Cannot return a value from a void function".to_string());
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

fn evaluate_unary(
    operator: UnaryOperator,
    expression: Box<TypedExpression>,
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

fn evaluate_binary(
    left: Box<TypedExpression>,
    operator: BinaryOperator,
    right: Box<TypedExpression>,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    evaluate_binop::evaluate_binop(*left, operator, *right, environment)
}

fn evaluate_block(
    statements: Vec<TypedStatement>,
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

fn evaluate_loop(
    body: Box<TypedExpression>,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let loop_environment = Rc::new(RefCell::new(Environment::new_scopes(
        environment,
        [Scope::Break(None), Scope::Continue],
    )));

    let break_value;

    'outer: loop {
        evaluate_expression(*body.clone(), loop_environment.clone())?;

        if let Some(Scope::Break(v)) = loop_environment.borrow().get_scope(&ScopeType::Break) {
            break_value = match v {
                Some(v) => v.clone(),
                None => Value::Void,
            };

            break 'outer;
        }

        if let Some(Scope::Continue) = loop_environment.borrow().get_scope(&ScopeType::Continue) {
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

    Ok(break_value)
}

fn evaluate_while(
    condition: Box<TypedExpression>,
    body: Box<TypedExpression>,
    else_body: Option<Box<TypedExpression>>,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let while_environment = Rc::new(RefCell::new(Environment::new_scopes(
        environment,
        [ScopeType::Break, ScopeType::Continue],
    )));

    let break_value;

    loop {
        let value = evaluate_expression(*condition.clone(), while_environment.clone())?;

        match value {
            Value::Bool(v) => {
                if !v {
                    break_value = match else_body {
                        Some(else_body) => {
                            let mut value =
                                evaluate_expression(*else_body.clone(), while_environment.clone())?;

                            if while_environment
                                .borrow()
                                .get_scope(&ScopeType::Return)
                                .is_some()
                            {
                                value = Value::Void;
                            }

                            value
                        }
                        None => Value::Void,
                    };

                    break;
                }
            }
            _ => return Err(format!("While condition must be boolean '{}'", value)),
        }

        evaluate_expression(*body.clone(), while_environment.clone())?;

        if let Some(Scope::Break(v)) = while_environment.borrow().get_scope(&ScopeType::Break) {
            break_value = match v {
                Some(v) => match else_body {
                    None => Err("Cannot break with a value in a while loop without an else block (add an else block with 'else {}')".to_string()),
                    Some(_) => Ok(v.clone())
                },
                None => Ok(Value::Void)
            }?;

            break;
        }

        if let Some(Scope::Continue) = while_environment.borrow().get_scope(&ScopeType::Continue) {
            continue;
        }

        if while_environment
            .borrow()
            .get_scope(&ScopeType::Return)
            .is_some()
        {
            break_value = Value::Void;
            break;
        }
    }

    Ok(break_value)
}

fn evaluate_for(
    pattern: Pattern,
    iterable: Box<TypedExpression>,
    body: Box<TypedExpression>,
    else_body: Option<Box<TypedExpression>>,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let for_environment = Rc::new(RefCell::new(Environment::new_scopes(
        environment,
        [ScopeType::Break, ScopeType::Continue],
    )));

    let value = evaluate_expression(*iterable.clone(), for_environment.clone())?;
    let array = match value {
        Value::Array(array) => array,
        _ => return Err(format!("For iterable must be an array '{}'", value)),
    };

    if array.is_empty() {
        match else_body {
            Some(else_body) => {
                let mut value = evaluate_expression(*else_body.clone(), for_environment.clone())?;

                if for_environment
                    .borrow()
                    .get_scope(&ScopeType::Return)
                    .is_some()
                {
                    value = Value::Void;
                }

                return Ok(value);
            }
            None => return Ok(Value::Void),
        }
    }

    let mut index = 0;

    let break_value;

    loop {
        if index >= array.len() {
            break_value = match else_body {
                Some(else_body) => {
                    let mut value =
                        evaluate_expression(*else_body.clone(), for_environment.clone())?;

                    if for_environment
                        .borrow()
                        .get_scope(&ScopeType::Return)
                        .is_some()
                    {
                        value = Value::Void;
                    }

                    value
                }
                None => Value::Void,
            };

            break;
        }

        let value = array
            .get(index)
            .cloned()
            .unwrap_or_else(|| panic!("Index out of bounds: {}", index));

        index += 1;

        if let Some(bindings) = evaluate_pattern(&pattern, &value, for_environment.clone())? {
            for (identifier, value) in bindings {
                for_environment
                    .borrow_mut()
                    .add_variable(identifier.clone(), value.clone(), false);
            }
        }

        evaluate_expression(*body.clone(), for_environment.clone())?;

        if let Some(Scope::Break(v)) = for_environment.borrow().get_scope(&ScopeType::Break) {
            break_value = match v {
                    Some(v) => match else_body {
                        None => Err("Cannot break with a value in a for loop without an else block (add an else block with 'else {}')".to_string()),
                        Some(_) => Ok(v.clone())
                    },
                    None => Ok(Value::Void)
                }?;

            break;
        }

        if let Some(Scope::Continue) = for_environment.borrow().get_scope(&ScopeType::Continue) {
            continue;
        }

        if for_environment
            .borrow()
            .get_scope(&ScopeType::Return)
            .is_some()
        {
            break_value = Value::Void;
            break;
        }
    }

    Ok(break_value)
}

fn evaluate_break(
    expression: Option<Box<TypedExpression>>,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    if !environment.borrow().has_scope(&ScopeType::Break) {
        return Err("Cannot break outside of a loop".to_string());
    };

    match expression {
        Some(expression) => {
            let value = evaluate_expression(*expression, environment.clone())?;
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
        return Err("Cannot continue outside of a loop".to_string());
    };

    environment.borrow_mut().activate_scope(Scope::Continue)?;
    Ok(Value::Void)
}

fn evaluate_return(
    expression: Option<Box<TypedExpression>>,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    if !environment.borrow().has_scope(&ScopeType::Return) {
        return Err("Cannot return outside of a function".to_string());
    };

    match expression {
        Some(expression) => {
            let value = evaluate_expression(*expression, environment.clone())?;
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

fn evaluate_built_in_function(
    built_in_function: BuiltInFunction,
    environment: Rc<RefCell<Environment>>,
) -> Value {
    get_built_in_function_value(built_in_function.function_type, environment)
}

fn get_built_in_function(
    key: impl ToKey,
    environment: Rcrc<Environment>,
) -> Option<Rcrc<Variable>> {
    BuiltInFunction::new(&key.to_key()).map(|b| {
        let value = get_built_in_function_value(b.function_type, environment);
        Rc::new(RefCell::new(Variable::new(
            b.type_identifier.to_key(),
            value,
            false,
        )))
    })
}

fn evaluate_index(
    object: Box<TypedExpression>,
    index: Box<TypedExpression>,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let object_value = evaluate_expression(*object, environment.clone())?;

    let index_value = evaluate_expression(*index, environment.clone())?;

    let Value::Number(Number::UInt(index)) = index_value else {
        unreachable!("Type is known after type checking, this should never happen")
    };

    match object_value {
        Value::Array(values) => {
            let value = values
                .get(index as usize)
                .cloned()
                .ok_or(format!("Index out of bounds '{}'", index))?;

            Ok(value)
        }
        _ => Err(format!("Cannot index non-array value '{}'", object_value)),
    }
}
