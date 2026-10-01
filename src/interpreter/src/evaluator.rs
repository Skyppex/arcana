use shared::ast::PatternKind;
use shared::diagnostic::Diagnostic;
use std::{cell::RefCell, ops::Deref, rc::Rc};

use shared::{
    ast::{pattern::Pattern, ComparisonOperator, ModPath, UseItem},
    built_in::BuiltInFunction,
    lexer::token::IdentifierType,
    type_checker::{
        decision_tree::{AccessPath, Decision, Test},
        model::*,
        overloaded_member_name,
        pattern::CheckedBound,
        Type,
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
) -> Result<Value, Diagnostic> {
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
) -> Result<Vec<Rcrc<Variable>>, Diagnostic> {
    match use_item {
        UseItem::Item(item_name) => {
            if item_name.validate_type_identifier_name().is_ok() {
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
) -> Result<Value, Diagnostic> {
    for (function_name, function) in functions {
        let parameter_type = match &function {
            TypedStatement::FunctionDeclaration { param, .. } => {
                param.as_ref().map(|param| param.type_.clone())
            }
            _ => None,
        };

        let _ = evaluate(function, environment.clone())?;

        let variable = environment
            .borrow()
            .get_function(&function_name)
            .ok_or(format!("Function '{}' not found", function_name))?
            .clone();

        // Registered under both the plain and the parameter-qualified name:
        // the plain one serves the ordinary case, the qualified one lets a
        // resolved overload reach the implementation that was chosen.
        let qualified = overloaded_member_name(&function_name, parameter_type.as_deref());

        environment.borrow_mut().add_static_member(
            &type_annotation,
            function_name,
            variable.clone(),
        );

        environment
            .borrow_mut()
            .add_static_member(&type_annotation, qualified, variable);
    }

    Ok(Value::Void)
}

fn evaluate_function_declaration(
    environment: Rc<RefCell<Environment>>,
    identifier: TypeIdentifier,
    param: Option<TypedParameter>,
    body: Option<TypedExpression>,
) -> Result<Value, Diagnostic> {
    let Some(body) = body else {
        return Err(Diagnostic::error(format!("Function '{}' must have a body", identifier)));
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
) -> Result<Value, Diagnostic> {
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
            wraps_true_branch,
            ..
        } => evaluate_if(
            condition,
            true_expression,
            false_expression,
            wraps_true_branch,
            environment,
        ),
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
        TypedExpression::Literal { literal, .. } => evaluate_literal(literal, environment),
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
) -> Result<Value, Diagnostic> {
    let mut tuple = Vec::new();

    for element in elements {
        tuple.push(evaluate_expression(element, environment.clone())?);
    }

    Ok(Value::Tuple(tuple))
}

fn evaluate_program(
    statements: Vec<TypedStatement>,
    environment: Rcrc<Environment>,
) -> Result<Value, Diagnostic> {
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
) -> Result<Value, Diagnostic> {
    let value = match initializer {
        Some(initializer) => evaluate_expression(*initializer, environment.clone())?,
        None => Value::Uninitialized,
    };

    let mut bindings = vec![];
    destructure_irrefutable(&pattern, &value, &mut bindings)?;

    for (identifier, value) in bindings {
        environment
            .borrow_mut()
            .add_variable(identifier, value, mutable);
    }

    Ok(Value::Bool(true))
}

fn evaluate_if(
    condition: Box<TypedExpression>,
    true_expression: Box<TypedExpression>,
    false_expression: Option<Box<TypedExpression>>,
    wraps_true_branch: bool,
    environment: Rcrc<Environment>,
) -> Result<Value, Diagnostic> {
    let if_environment = Rc::new(RefCell::new(Environment::new_parent(environment.clone())));
    let condition = evaluate_expression(*condition, if_environment.clone())?;

    let Value::Bool(condition) = condition else {
        return Err(Diagnostic::error(format!("If condition must be boolean '{}'", condition)));
    };

    if !condition {
        // No else at all means the expression is optional and this path is the
        // one that produces nothing.
        let Some(false_expression) = false_expression else {
            return Ok(Value::option_none());
        };

        // An else is never wrapped: either it is already optional, or nothing
        // here is wrapped at all.
        return evaluate_expression(*false_expression, if_environment);
    }

    let value = evaluate_expression(*true_expression, if_environment)?;

    // Whether this has to be wrapped was settled while type checking, which is
    // the only place that can see whether the expression as a whole is
    // optional.
    if wraps_true_branch {
        return Ok(Value::option_some(value));
    }

    Ok(value)
}

fn evaluate_match(
    expression: Box<TypedExpression>,
    decision_tree: Decision,
    environment: Rcrc<Environment>,
) -> Result<Value, Diagnostic> {
    // Evaluated once. Everything the tree tests is projected out of this value.
    let value = evaluate_expression(*expression, environment.clone())?;
    let match_environment = Rc::new(RefCell::new(Environment::new_parent(environment)));

    evaluate_decision_tree(decision_tree, &value, match_environment)
}

fn evaluate_decision_tree(
    decision_tree: Decision,
    value: &Value,
    environment: Rcrc<Environment>,
) -> Result<Value, Diagnostic> {
    let mut decision = decision_tree;

    loop {
        match decision {
            Decision::Success { bindings, body, .. } => {
                let arm_environment =
                    Rc::new(RefCell::new(Environment::new_parent(environment.clone())));

                for binding in bindings {
                    let bound = project(value, &binding.occurrence.path)?;

                    arm_environment
                        .borrow_mut()
                        .add_variable(binding.identifier, bound, false);
                }

                return evaluate_expression(*body, arm_environment);
            }
            Decision::Failure { witness } => {
                return Err(Diagnostic::error(format!("No match found for '{}' {}", value, witness)))
            }
            Decision::Switch {
                occurrence,
                cases,
                default,
                ..
            } => {
                let tested = project(value, &occurrence.path)?;

                let matched = cases
                    .into_iter()
                    .find(|case| test_matches(&case.test, &tested, &environment).unwrap_or(false))
                    .map(|case| case.decision);

                decision = match matched.or(default.map(|d| *d)) {
                    Some(decision) => decision,
                    None => {
                        return Err(Diagnostic::error(format!(
                            "No case matched '{}' and there is no default",
                            tested
                        )))
                    }
                };
            }
        }
    }
}

/// Reads the value at `path` out of the value being matched.
fn project(value: &Value, path: &AccessPath) -> Result<Value, Diagnostic> {
    match path {
        AccessPath::Root => Ok(value.clone()),
        // Narrowing to a nested enum is a change of static type only; the
        // runtime value is the same one.
        AccessPath::Narrow(parent, _) => project(value, parent),
        AccessPath::Field(parent, name) => {
            let parent = project(value, parent)?;
            let fields = match &parent {
                Value::Struct(Struct { fields, .. }) => fields,
                // Shared fields live on the variant like any other field.
                Value::Enum(Enum {
                    enum_member: Struct { fields, .. },
                    ..
                }) => fields,
                other => return Err(Diagnostic::error(format!("Expected a struct or enum, found '{}'", other))),
            };

            fields
                .iter()
                .find(|field| &field.identifier == name)
                .map(|field| field.value.clone())
                .ok_or(Diagnostic::error(format!("Field '{}' not found on '{}'", name, parent)))
        }
        AccessPath::TupleIndex(parent, index) => {
            let parent = project(value, parent)?;

            let Value::Tuple(values) = &parent else {
                return Err(Diagnostic::error(format!("Expected a tuple, found '{}'", parent)));
            };

            values
                .get(*index)
                .cloned()
                .ok_or(Diagnostic::error(format!("Tuple has no element {}", index)))
        }
    }
}

fn test_matches(
    test: &Test,
    value: &Value,
    environment: &Rcrc<Environment>,
) -> Result<bool, Diagnostic> {
    Ok(match (test, value) {
        (Test::Bool(expected), Value::Bool(actual)) => expected == actual,
        (Test::Int(expected), Value::Number(Number::Int(actual))) => expected == actual,
        (Test::UInt(expected), Value::Number(Number::UInt(actual))) => expected == actual,
        (Test::Float(expected), Value::Number(Number::Float(actual))) => expected == actual,
        (Test::Rune(expected), Value::Rune(actual)) => expected == actual,
        (Test::String(expected), Value::String(actual)) => expected == actual,
        (
            Test::Variant(qualified_name),
            Value::Enum(Enum {
                enum_member: Struct { type_name, .. },
                ..
            }),
        ) => {
            // A value carries the full path of the variant it holds, so a test
            // for a nested enum matches every variant declared inside it. The
            // tests at any one occurrence are disjoint prefixes, so this stays
            // unambiguous.
            type_name == qualified_name || type_name.starts_with(&format!("{}::", qualified_name))
        }
        (Test::Comparison { operator, bound }, value) => {
            let bound = resolve_bound(bound, environment)?;

            match compare(value, &bound)? {
                Some(ordering) => match operator {
                    ComparisonOperator::LessThan => ordering.is_lt(),
                    ComparisonOperator::GreaterThan => ordering.is_gt(),
                    ComparisonOperator::LessThanOrEqual => ordering.is_le(),
                    ComparisonOperator::GreaterThanOrEqual => ordering.is_ge(),
                },
                None => false,
            }
        }
        (
            Test::Range {
                lower,
                upper,
                inclusive,
            },
            value,
        ) => {
            let lower = resolve_bound(lower, environment)?;
            let upper = resolve_bound(upper, environment)?;

            let at_least_lower = compare(value, &lower)?.is_some_and(|o| o.is_ge());
            let within_upper =
                compare(value, &upper)?.is_some_and(
                    |o| {
                        if *inclusive {
                            o.is_le()
                        } else {
                            o.is_lt()
                        }
                    },
                );

            at_least_lower && within_upper
        }
        _ => false,
    })
}

fn resolve_bound(bound: &CheckedBound, environment: &Rcrc<Environment>) -> Result<Value, Diagnostic> {
    Ok(match bound {
        CheckedBound::Int(v) => Value::Number(Number::Int(*v)),
        CheckedBound::UInt(v) => Value::Number(Number::UInt(*v)),
        CheckedBound::Float(v) => Value::Number(Number::Float(*v)),
        CheckedBound::Rune(v) => Value::Rune(*v),
        CheckedBound::Variable(identifier) => {
            let Some(variable) = environment.borrow().get_variable(identifier) else {
                return Err(Diagnostic::error(format!(
                    "Variable '{}' not found in environment",
                    identifier
                )));
            };

            let value = variable.borrow().value.clone();
            value
        }
    })
}

fn compare(left: &Value, right: &Value) -> Result<Option<std::cmp::Ordering>, Diagnostic> {
    Ok(match (left, right) {
        (Value::Number(Number::Int(l)), Value::Number(Number::Int(r))) => l.partial_cmp(r),
        (Value::Number(Number::UInt(l)), Value::Number(Number::UInt(r))) => l.partial_cmp(r),
        (Value::Number(Number::Float(l)), Value::Number(Number::Float(r))) => l.partial_cmp(r),
        (Value::Rune(l), Value::Rune(r)) => l.partial_cmp(r),
        _ => None,
    })
}

/// Binds the names in a pattern that cannot fail. Declarations and loops use
/// these; the type checker has already rejected anything refutable.
fn destructure_irrefutable(
    pattern: &Pattern,
    value: &Value,
    bindings: &mut Vec<(String, Value)>,
) -> Result<(), Diagnostic> {
    match &pattern.kind {
        PatternKind::Wildcard | PatternKind::Unit => Ok(()),
        PatternKind::Binding(identifier) => {
            bindings.push((identifier.clone(), value.clone()));
            Ok(())
        }
        PatternKind::Tuple(patterns) => {
            let Value::Tuple(values) = value else {
                return Err(Diagnostic::error(format!("Expected a tuple, found '{}'", value)));
            };

            for (pattern, value) in patterns.iter().zip(values) {
                destructure_irrefutable(pattern, value, bindings)?;
            }

            Ok(())
        }
        // `x @ p` binds and keeps matching the same value.
        PatternKind::Bound {
            identifier,
            pattern,
        } => {
            bindings.push((identifier.clone(), value.clone()));
            destructure_irrefutable(pattern, value, bindings)
        }
        PatternKind::Struct { fields, .. } => {
            for field in fields {
                let field_value = project(
                    value,
                    &AccessPath::Field(Box::new(AccessPath::Root), field.identifier.clone()),
                )?;

                destructure_irrefutable(&field.pattern, &field_value, bindings)?;
            }

            Ok(())
        }
        // The variant is statically known here, so the test is a no-op and only
        // the inner pattern binds anything.
        PatternKind::EnumVariant { inner, .. } => match inner {
            Some(inner) => destructure_irrefutable(inner, value, bindings),
            None => Ok(()),
        },
        _ => Err(Diagnostic::error(format!("Pattern '{}' is refutable", pattern))),
    }
}

fn evaluate_assignment(
    member: Box<Member>,
    initializer: Box<TypedExpression>,
    environment: Rcrc<Environment>,
) -> Result<Value, Diagnostic> {
    let value = evaluate_expression(*initializer, environment.clone())?;
    environment
        .borrow_mut()
        .set_variable(*member, value.clone())?;
    Ok(value)
}

fn evaluate_member(member: Member, environment: Rcrc<Environment>) -> Result<Value, Diagnostic> {
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
) -> Result<Value, Diagnostic> {
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

/// Helper to extract a field from a value (struct, enum, or recursively from arrays)
fn extract_field_from_value(value: Value, symbol: &str) -> Result<Value, Diagnostic> {
    match value {
        Value::Struct(Struct { type_name, fields }) => fields
            .iter()
            .find(|f| f.identifier == symbol)
            .map(|f| f.value.clone())
            .ok_or(Diagnostic::error(format!(
                "Field '{}' not found in struct '{}'",
                symbol, type_name
            ))),
        Value::Enum(Enum {
            enum_member: Struct { type_name, fields },
            ..
        }) => fields
            .iter()
            .find(|f| f.identifier == symbol)
            .map(|f| f.value.clone())
            .ok_or(Diagnostic::error(format!(
                "Field '{}' not found in enum member '{}'",
                symbol, type_name
            ))),
        Value::Array(elements) => {
            let results: Result<Vec<Value>, Diagnostic> = elements
                .into_iter()
                .map(|el| extract_field_from_value(el, symbol))
                .collect();
            Ok(Value::Array(results?))
        }
        other => Err(Diagnostic::error(format!(
            "Cannot access field '{}' on value: '{}'",
            symbol, other
        ))),
    }
}

fn evaluate_member_access(
    object: Box<TypedExpression>,
    environment: Rcrc<Environment>,
    member: Box<Member>,
) -> Result<Value, Diagnostic> {
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
            Member::StaticMemberAccess { .. } => Err(Diagnostic::error(format!(
                "Cannot access static member on an instance of a struct '{}'",
                type_name
            ))),
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
            Member::StaticMemberAccess { .. } => Err(Diagnostic::error(format!(
                "Cannot access static member on an instance of a enum '{}'",
                type_name
            ))),
            Member::MemberAccess { object, member, .. } => {
                evaluate_member_access(object, environment, member)
            }
            Member::BuiltInFunction(_) => {
                panic!("Cannot access members on a built-in function")
            }
            Member::Index { .. } => todo!("members on an indexed expression"),
        },
        Value::Array(elements) => match *member.clone() {
            Member::Identifier { symbol, .. } => {
                let results: Result<Vec<Value>, Diagnostic> = elements
                    .into_iter()
                    .map(|el| extract_field_from_value(el, &symbol))
                    .collect();
                Ok(Value::Array(results?))
            }
            Member::StaticMemberAccess { .. } => {
                Err(Diagnostic::error("Cannot access static member on an array"))
            }
            Member::MemberAccess { object, member, .. } => {
                // For chained access like arr.foo.bar, evaluate the inner access first
                evaluate_member_access(object, environment, member)
            }
            Member::BuiltInFunction(_) => {
                panic!("Cannot access members on a built-in function")
            }
            Member::Index { .. } => todo!("members on an indexed expression"),
        },
        _ => Err(Diagnostic::error(format!("Cannot access member value: '{}'", value))),
    }
}

fn evaluate_literal(
    literal: ValueLiteral,
    environment: Rcrc<Environment>,
) -> Result<Value, Diagnostic> {
    match literal {
        ValueLiteral::Void => panic!("Void literals should never be evaluated"),
        ValueLiteral::Unit => Ok(Value::Unit),
        ValueLiteral::Int(v) => Ok(Value::Number(Number::Int(v))),
        ValueLiteral::UInt(v) => Ok(Value::Number(Number::UInt(v))),
        ValueLiteral::Float(v) => Ok(Value::Number(Number::Float(v))),
        ValueLiteral::String(v) => Ok(Value::String(v)),
        ValueLiteral::Rune(v) => Ok(Value::Rune(v)),
        ValueLiteral::Bool(v) => Ok(Value::Bool(v)),
        ValueLiteral::Array { items, .. } => {
            let mut array = Vec::new();

            for item in items {
                match item {
                    ArrayItem::Expression(expression) => {
                        array.push(evaluate_expression(expression, environment.clone())?)
                    }
                    ArrayItem::Spread(expression) => {
                        let values = evaluate_expression(expression, environment.clone())?;

                        let Value::Array(values) = values else {
                            return Err(Diagnostic::error(format!("Expected to spread an array, but got {}", values)));
                        };

                        for value in values {
                            array.push(value);
                        }
                    }
                }
            }

            Ok(Value::Array(array))
        }
        ValueLiteral::Struct {
            field_initializers,
            type_,
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
                type_name: type_.unsubstitute().to_key(),
                fields,
            }))
        }
        ValueLiteral::Enum {
            field_initializers,
            type_,
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

            // The member's key is qualified (`O::S`); the enum itself is the
            // part before the last separator.
            let member_name = type_.unsubstitute().to_key();
            let enum_name = member_name
                .rsplit_once("::")
                .map(|(enum_name, _)| enum_name.to_owned())
                .unwrap_or_else(|| member_name.clone());

            Ok(Value::Enum(Enum {
                type_name: enum_name,
                enum_member: Struct {
                    type_name: member_name,
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
) -> Result<Value, Diagnostic> {
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
) -> Result<Value, Diagnostic> {
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
        return Err(Diagnostic::error(format!("Cannot call non-function value '{}'", callee_value)));
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
                    return Err(Diagnostic::error("Cannot return a value from a void function"));
                }

                value = v.clone();
            }
            None => {
                if type_ != Type::Void {
                    return Err(Diagnostic::error(format!(
                        "Cannot return void from a non-void function. Expected type '{}', found type 'void'",
                        type_
                    )));
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
) -> Result<Value, Diagnostic> {
    let value = evaluate_expression(*expression, environment)?;

    match operator {
        UnaryOperator::Identity => match value {
            Value::Number(number) => Ok(Value::Number(number)),
            _ => Err(Diagnostic::error(format!(
                "Cannot apply unary operator '+' to non-number value '{}'",
                value
            ))),
        },
        UnaryOperator::Negate => match value {
            Value::Number(number) => match number {
                Number::Int(v) => Ok(Value::Number(Number::Int(-v))),
                Number::Float(v) => Ok(Value::Number(Number::Float(-v))),
                other => Err(Diagnostic::error(format!(
                    "Cannot apply unary operator '-' to unsigned integers '{}'",
                    other
                ))),
            },
            _ => Err(Diagnostic::error(format!(
                "Cannot apply unary operator '-' to non-number value '{}'",
                value
            ))),
        },
        UnaryOperator::BitwiseNot => match value {
            Value::Number(number) => match number {
                Number::Int(v) => Ok(Value::Number(Number::Int(!v))),
                Number::UInt(v) => Ok(Value::Number(Number::UInt(!v))),
                other => Err(Diagnostic::error(format!(
                    "Cannot apply unary operator '~' to floating point numbers '{}'",
                    other
                ))),
            },
            _ => Err(Diagnostic::error(format!(
                "Cannot apply unary operator '~' to non-number value '{}'",
                value
            ))),
        },
        UnaryOperator::LogicalNot => match value {
            Value::Bool(v) => Ok(Value::Bool(!v)),
            _ => Err(Diagnostic::error(format!(
                "Cannot apply unary operator '!' to non-boolean value '{}'",
                value
            ))),
        },
    }
}

fn evaluate_binary(
    left: Box<TypedExpression>,
    operator: BinaryOperator,
    right: Box<TypedExpression>,
    environment: Rcrc<Environment>,
) -> Result<Value, Diagnostic> {
    evaluate_binop::evaluate_binop(*left, operator, *right, environment)
}

fn evaluate_block(
    statements: Vec<TypedStatement>,
    environment: Rcrc<Environment>,
) -> Result<Value, Diagnostic> {
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
) -> Result<Value, Diagnostic> {
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
) -> Result<Value, Diagnostic> {
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
            _ => return Err(Diagnostic::error(format!("While condition must be boolean '{}'", value))),
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
) -> Result<Value, Diagnostic> {
    let for_environment = Rc::new(RefCell::new(Environment::new_scopes(
        environment,
        [ScopeType::Break, ScopeType::Continue],
    )));

    let value = evaluate_expression(*iterable.clone(), for_environment.clone())?;
    let array = match value {
        Value::Array(array) => array,
        _ => return Err(Diagnostic::error(format!("For iterable must be an array '{}'", value))),
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

        let mut bindings = vec![];
        destructure_irrefutable(&pattern, &value, &mut bindings)?;

        for (identifier, value) in bindings {
            for_environment
                .borrow_mut()
                .add_variable(identifier.clone(), value.clone(), false);
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
) -> Result<Value, Diagnostic> {
    if !environment.borrow().has_scope(&ScopeType::Break) {
        return Err(Diagnostic::error("Cannot break outside of a loop"));
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

fn evaluate_continue(environment: Rcrc<Environment>) -> Result<Value, Diagnostic> {
    if !environment.borrow().has_scope(&ScopeType::Continue) {
        return Err(Diagnostic::error("Cannot continue outside of a loop"));
    };

    environment.borrow_mut().activate_scope(Scope::Continue)?;
    Ok(Value::Void)
}

fn evaluate_return(
    expression: Option<Box<TypedExpression>>,
    environment: Rcrc<Environment>,
) -> Result<Value, Diagnostic> {
    if !environment.borrow().has_scope(&ScopeType::Return) {
        return Err(Diagnostic::error("Cannot return outside of a function"));
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
    index: Index,
    environment: Rcrc<Environment>,
) -> Result<Value, Diagnostic> {
    let object_value = evaluate_expression(*object, environment.clone())?;

    match index {
        Index::Value(index) => {
            let index_value = evaluate_expression(*index, environment.clone())?;

            let index = match index_value {
                Value::Number(Number::UInt(index)) => index as usize,
                Value::Number(Number::Int(index)) => index as usize,
                _ => unreachable!("Type is known after type checking, this should never happen"),
            };

            let Value::Array(values) = object_value else {
                return Err(Diagnostic::error(format!("Cannot index non-array value '{}'", object_value)));
            };

            let value = values
                .get(index as usize)
                .cloned()
                .ok_or(format!("Index out of bounds '{}'", index))?;

            Ok(value)
        }
        Index::Range {
            start,
            end,
            inclusive,
        } => {
            let start_value = start
                .map(|start| evaluate_expression(*start, environment.clone()))
                .transpose()?;

            let end_value = end
                .map(|end| evaluate_expression(*end, environment.clone()))
                .transpose()?;

            // slice array allowing for start and/or end to be None
            let Value::Array(values) = object_value else {
                return Err(Diagnostic::error(format!("Cannot index non-array value '{}'", object_value)));
            };

            let start_index = match start_value {
                Some(Value::Number(Number::UInt(index))) => index as usize,
                Some(Value::Number(Number::Int(index))) => index as usize,
                Some(_) => {
                    return Err(Diagnostic::error(format!(
                        "Expected unsigned integer for start index, found '{}'",
                        start_value.unwrap()
                    )))
                }
                None => 0, // default to 0 if no start is provided
            };

            let end_index = match end_value {
                Some(Value::Number(Number::UInt(index))) => index as usize,
                Some(Value::Number(Number::Int(index))) => index as usize,
                Some(_) => {
                    return Err(Diagnostic::error(format!(
                        "Expected unsigned integer for end index, found '{}'",
                        end_value.unwrap()
                    )))
                }
                None => values.len(), // default to length of array if no end is provided
            };

            if start_index > end_index {
                return Err(Diagnostic::error(format!(
                    "Start index '{}' cannot be greater than end index '{}'",
                    start_index, end_index
                )));
            }

            if inclusive {
                if end_index >= values.len() {
                    return Err(Diagnostic::error(format!(
                        "End index '{}' is out of bounds for array of length '{}'",
                        end_index,
                        values.len()
                    )));
                }

                Ok(Value::Array(values[start_index..=end_index].to_vec()))
            } else {
                if end_index > values.len() {
                    return Err(Diagnostic::error(format!(
                        "End index '{}' is out of bounds for array of length '{}'",
                        end_index,
                        values.len()
                    )));
                }

                Ok(Value::Array(values[start_index..end_index].to_vec()))
            }
        }
    }
}
