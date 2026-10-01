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
    model::{
        Block, Member, Typed, TypedExpression, TypedMatchArm, TypedStatement, ValueLiteral,
    },
    Type,
};

/// Simplifies a whole program.
pub fn simplify(statement: TypedStatement) -> TypedStatement {
    simplify_statement(statement).unwrap_or(TypedStatement::None)
}

/// Whether replacing this expression with its value would lose anything.
///
/// This is deliberately a question about the *shape* of the node rather than
/// about purity: a call may well have a literal type — `fun f(): #1` — and
/// replacing the call would throw away whatever else it did. Only nodes that
/// plainly do nothing but produce a value qualify, and a compound node only
/// qualifies when its parts do.
fn is_inert(expression: &TypedExpression) -> bool {
    match expression {
        TypedExpression::Literal { .. } => true,
        // A plain name. Field access and indexing are left out: the thing being
        // accessed may be anything at all, including a call.
        TypedExpression::Member(Member::Identifier { .. }) => true,
        TypedExpression::Unary { expression, .. } => is_inert(expression),
        TypedExpression::Binary { left, right, .. } => is_inert(left) && is_inert(right),
        _ => false,
    }
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
    if !is_inert(&expression) {
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
        TypedStatement::Semi(inner) => {
            TypedStatement::Semi(Box::new(simplify_statement(*inner)?))
        }
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
    statements.into_iter().filter_map(simplify_statement).collect()
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
