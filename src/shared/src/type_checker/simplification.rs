//! Replaces expressions whose value the type system already knows.
//!
//! Nothing here analyses values: because a literal carries its value in its own
//! type, an expression that has a value-carrying literal type *is* that value,
//! and the pass only has to read it off and rewrite the node. There is no
//! environment, no propagation and no fixpoint — the work was done while type
//! checking.
//!
//! The pass runs bottom up, so a rewritten child is already a literal by the
//! time its parent is considered: `let x = 100; let y = x + 1;` reduces `x` to
//! `100` and then has an ordinary constant to fold.

use crate::type_checker::{
    model::{Block, Typed, TypedExpression, TypedMatchArm, TypedStatement, ValueLiteral},
    purity::{escapes, purity_of},
    Type,
};

/// Simplifies a whole program.
pub fn simplify(statement: TypedStatement) -> TypedStatement {
    simplify_statement(statement).unwrap_or(TypedStatement::None)
}

/// Whether this expression can be thrown away — replaced by its value, or
/// dropped outright.
///
/// Three independent things have to hold, and only the first is about purity:
///
/// 1. **Nothing observable happens.** [`purity_of`] answers this, which is why
///    `fun f(): #1` now folds to `1` where the old shape-based test could never
///    allow it.
/// 2. **Control does not leave.** `{ return 1; 2 }` is pure by every rule and
///    has type `#2`; replacing it with `2` would delete the `return`. This is
///    not a purity question and must not be folded into one.
/// 3. **It is not a loop.** A pure loop may never finish, and deleting it would
///    make the program terminate. Proving otherwise is out of scope.
fn is_discardable(expression: &TypedExpression) -> bool {
    purity_of(expression).is_pure() && !escapes(expression) && !is_loop(expression)
}

/// Whether evaluating this may never finish.
///
/// A pure loop that does terminate still cannot be deleted: proving it does is
/// out of scope, and deleting a non-terminating loop would make the program
/// terminate.
fn is_loop(expression: &TypedExpression) -> bool {
    matches!(expression, TypedExpression::Loop { .. })
}

/// The literal an expression's type says it is, when there is one.
///
/// A literal type that names only a kind — `#Int` — carries no value and yields
/// nothing here, which is what keeps `let mut x = 100` alone: a mutable binding
/// widens to `Int` and so is never substituted.
fn known_value(type_: &Type) -> Option<ValueLiteral> {
    ValueLiteral::try_from(type_.clone().unsubstitute()).ok()
}

/// Rewrites an expression to the literal its type names, where that is safe.
fn substitute(expression: TypedExpression) -> TypedExpression {
    if !is_discardable(&expression) {
        return expression;
    }

    let type_ = expression.get_type();

    match known_value(&type_) {
        Some(literal) => TypedExpression::Literal { literal, type_ },
        None => expression,
    }
}

fn simplify_statement(statement: TypedStatement) -> Option<TypedStatement> {
    let statement = match statement {
        TypedStatement::Program { statements } => TypedStatement::Program {
            statements: simplify_statements(statements),
        },
        TypedStatement::Semi(inner) => TypedStatement::Semi(Box::new(simplify_statement(*inner)?)),
        TypedStatement::Expression(expression) => {
            let expression = simplify_expression(expression);

            // A binding whose value the type system knows has already been
            // substituted everywhere it was used, so the binding itself is
            // dead — as long as working out the value did not also do
            // something, which is exactly the case where the initializer did
            // not reduce to a literal.
            if let TypedExpression::VariableDeclaration {
                mutable: false,
                initializer: Some(initializer),
                bound_type,
                ..
            } = &expression
            {
                if known_value(bound_type).is_some()
                    && matches!(**initializer, TypedExpression::Literal { .. })
                {
                    return None;
                }
            }

            TypedStatement::Expression(expression)
        }
        TypedStatement::FunctionDeclaration {
            type_identifier,
            param,
            return_type,
            body,
            type_,
        } => TypedStatement::FunctionDeclaration {
            type_identifier,
            param,
            return_type,
            body: body.map(simplify_expression),
            type_,
        },
        TypedStatement::ProtocolDeclaration {
            type_identifier,
            associated_types,
            functions,
            type_,
        } => TypedStatement::ProtocolDeclaration {
            type_identifier,
            associated_types,
            functions: simplify_statements(functions),
            type_,
        },
        TypedStatement::ImplementationDeclaration {
            scoped_generics,
            protocol_annotation,
            type_annotation,
            associated_types,
            functions,
            type_,
        } => TypedStatement::ImplementationDeclaration {
            scoped_generics,
            protocol_annotation,
            type_annotation,
            associated_types,
            functions: functions
                .into_iter()
                .filter_map(|(name, f)| simplify_statement(f).map(|f| (name, f)))
                .collect(),
            type_,
        },
        other => other,
    };

    Some(statement)
}

fn simplify_statements(statements: Vec<TypedStatement>) -> Vec<TypedStatement> {
    // Which statement, if any, the block takes its value from. Positions are
    // taken before anything is removed, because that is what decides whether a
    // statement's value is used.
    let last = statements.len().saturating_sub(1);

    statements
        .into_iter()
        .enumerate()
        .filter_map(|(index, statement)| {
            let statement = simplify_statement(statement)?;

            if is_deletable(&statement, index == last) {
                return None;
            }

            Some(statement)
        })
        .collect()
}

/// Whether a statement can be removed from the block it is in.
///
/// `used` says the block takes its value from this statement — the final one,
/// unless a semicolon threw that value away.
fn is_deletable(statement: &TypedStatement, used: bool) -> bool {
    let expression = match statement {
        // A semicolon discards the value, so even a final statement is unused.
        TypedStatement::Semi(inner) => return is_deletable(inner, false),
        TypedStatement::Expression(expression) => expression,
        // Declarations run nothing, but other statements refer to them.
        // Removing one is not a question about effects.
        _ => return false,
    };

    if used {
        return false;
    }

    // Bindings are left to the rule in `simplify_statement`, which deletes
    // exactly the ones whose value was substituted everywhere it was used.
    // Whether a binding is dead is a question about references, not effects.
    if matches!(expression, TypedExpression::VariableDeclaration { .. }) {
        return false;
    }

    is_discardable(expression)
}

fn simplify_expression(expression: TypedExpression) -> TypedExpression {
    // Children first, so that a parent sees whatever its parts reduced to.
    let expression = match expression {
        TypedExpression::VariableDeclaration {
            mutable,
            pattern,
            initializer,
            bound_type,
            type_,
        } => TypedExpression::VariableDeclaration {
            mutable,
            pattern,
            initializer: initializer.map(|i| Box::new(simplify_expression(*i))),
            bound_type,
            type_,
        },
        TypedExpression::If {
            condition,
            true_expression,
            false_expression,
            wraps_true_branch,
            type_,
        } => {
            let condition = Box::new(simplify_expression(*condition));
            let true_expression = Box::new(simplify_expression(*true_expression));
            let false_expression = false_expression.map(|e| Box::new(simplify_expression(*e)));

            return collapse_if(
                condition,
                true_expression,
                false_expression,
                wraps_true_branch,
                type_,
            );
        }
        TypedExpression::Match {
            expression,
            arms,
            decision_tree,
            type_,
        } => TypedExpression::Match {
            expression: Box::new(simplify_expression(*expression)),
            arms: arms
                .into_iter()
                .map(|arm| TypedMatchArm {
                    expression: simplify_expression(arm.expression),
                    ..arm
                })
                .collect(),
            decision_tree,
            type_,
        },
        TypedExpression::Assignment {
            member,
            initializer,
            type_,
        } => TypedExpression::Assignment {
            member,
            initializer: Box::new(simplify_expression(*initializer)),
            type_,
        },
        TypedExpression::Tuple { elements, type_ } => TypedExpression::Tuple {
            elements: elements.into_iter().map(simplify_expression).collect(),
            type_,
        },
        TypedExpression::Closure {
            param,
            return_type,
            body,
            type_,
        } => TypedExpression::Closure {
            param,
            return_type,
            body: Box::new(simplify_expression(*body)),
            type_,
        },
        TypedExpression::Call {
            callee,
            argument,
            type_,
        } => TypedExpression::Call {
            callee: Box::new(simplify_expression(*callee)),
            argument: argument.map(|a| Box::new(simplify_expression(*a))),
            type_,
        },
        TypedExpression::Unary {
            operator,
            expression,
            type_,
        } => TypedExpression::Unary {
            operator,
            expression: Box::new(simplify_expression(*expression)),
            type_,
        },
        TypedExpression::Binary {
            left,
            operator,
            right,
            type_,
        } => TypedExpression::Binary {
            left: Box::new(simplify_expression(*left)),
            operator,
            right: Box::new(simplify_expression(*right)),
            type_,
        },
        TypedExpression::Block(Block { statements, type_ }) => TypedExpression::Block(Block {
            statements: simplify_statements(statements),
            type_,
        }),
        TypedExpression::Loop { body, type_ } => TypedExpression::Loop {
            body: Box::new(simplify_expression(*body)),
            type_,
        },
        TypedExpression::While {
            condition,
            body,
            else_body,
            type_,
        } => TypedExpression::While {
            condition: Box::new(simplify_expression(*condition)),
            body: Box::new(simplify_expression(*body)),
            else_body: else_body.map(|e| Box::new(simplify_expression(*e))),
            type_,
        },
        TypedExpression::For {
            pattern,
            iterable,
            body,
            else_body,
            type_,
        } => TypedExpression::For {
            pattern,
            iterable: Box::new(simplify_expression(*iterable)),
            body: Box::new(simplify_expression(*body)),
            else_body: else_body.map(|e| Box::new(simplify_expression(*e))),
            type_,
        },
        TypedExpression::Break(value) => {
            TypedExpression::Break(value.map(|v| Box::new(simplify_expression(*v))))
        }
        TypedExpression::Return(value) => {
            TypedExpression::Return(value.map(|v| Box::new(simplify_expression(*v))))
        }
        other => other,
    };

    substitute(expression)
}

/// Drops the branch a known condition never takes.
///
/// The `if`'s own type does not say which branch wins — it is the two joined,
/// so `if c { "yup" } else { "nope" }` is `#String` either way. The condition is
/// what settles it.
///
/// An `if` that wraps its true branch, or that has no else, is left alone: both
/// produce an `Option`, and collapsing them would mean building that value
/// here rather than reading one off the tree.
fn collapse_if(
    condition: Box<TypedExpression>,
    true_expression: Box<TypedExpression>,
    false_expression: Option<Box<TypedExpression>>,
    wraps_true_branch: bool,
    type_: Type,
) -> TypedExpression {
    let known_condition = match known_value(&condition.get_type()) {
        Some(ValueLiteral::Bool(taken)) if !wraps_true_branch && false_expression.is_some() => {
            Some(taken)
        }
        _ => None,
    };

    match known_condition {
        Some(true) => *true_expression,
        Some(false) => *false_expression.expect("checked just above"),
        _ => TypedExpression::If {
            condition,
            true_expression,
            false_expression,
            wraps_true_branch,
            type_,
        },
    }
}
