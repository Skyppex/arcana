//! Whether evaluating something can be observed.
//!
//! Every effect in the language originates at a built-in — printing, reading
//! input, dropping a binding — or at an assignment. Nothing else is impure
//! except by reaching one of those, so purity is computed structurally: a node
//! is pure when its parts are, and a call is pure when the thing it calls is.
//!
//! Purity is never required of anything. It is inferred, never written, and
//! `type_equals` does not look at it, so an impure function stays assignable
//! wherever a pure one is. It exists so the simplification pass can tell what it
//! is allowed to throw away.
//!
//! Two things that are *pure* and still must not be deleted are deliberately
//! not handled here, because they are not purity questions:
//!
//! - a region that transfers control out of itself — see [`escapes`];
//! - a loop, which may not terminate. The simplification pass refuses those
//!   outright.

use crate::built_in::BuiltInFunction;

use crate::type_checker::model::{
    BinaryOperator, Block, Index, Member, Typed, TypedExpression, TypedStatement, ValueLiteral,
};
use crate::type_checker::{Function, Purity, Type};

/// Whether evaluating `expression` can be observed.
pub fn purity_of(expression: &TypedExpression) -> Purity {
    match expression {
        TypedExpression::Literal { .. } => Purity::Pure,
        TypedExpression::Continue => Purity::Pure,

        TypedExpression::Member(member) => purity_of_member(member),

        // Writing to a binding is the one effect that does not come from a
        // built-in.
        //
        // This is an under-approximation on purpose: writing to a binding that
        // was *declared inside* the region being deleted cannot be observed
        // from outside it, so some of these are deletable. Telling the two
        // apart needs a write set rather than a bit, which is a later change —
        // it only ever adds deletions, never removes them. Do not "fix" this to
        // `Pure`.
        TypedExpression::Assignment { .. } => Purity::Impure,

        TypedExpression::Unary { expression, .. } => purity_of(expression),

        TypedExpression::Binary {
            left,
            operator,
            right,
            ..
        } => purity_of(left)
            .and(purity_of(right))
            .and(purity_of_operator(operator, right)),

        TypedExpression::Tuple { elements, .. } => elements.iter().map(purity_of).collect(),

        // Building a closure runs nothing, so it is pure however dirty the body
        // is. The body's purity belongs to the closure's *type*, where the
        // eventual call reads it.
        TypedExpression::Closure { .. } => Purity::Pure,

        // Evaluating the callee and the argument, and then whatever the callee
        // does when called — which its type carries.
        TypedExpression::Call {
            callee, argument, ..
        } => purity_of(callee)
            .and(argument.as_ref().map_or(Purity::Pure, |a| purity_of(a)))
            .and(purity_of_callee(&callee.get_type())),

        TypedExpression::VariableDeclaration { initializer, .. } => {
            initializer.as_ref().map_or(Purity::Pure, |i| purity_of(i))
        }

        TypedExpression::If {
            condition,
            true_expression,
            false_expression,
            ..
        } => purity_of(condition).and(purity_of(true_expression)).and(
            false_expression
                .as_ref()
                .map_or(Purity::Pure, |e| purity_of(e)),
        ),

        TypedExpression::Match {
            expression, arms, ..
        } => purity_of(expression).and(arms.iter().map(|arm| purity_of(&arm.expression)).collect()),

        TypedExpression::Block(Block { statements, .. }) => {
            statements.iter().map(purity_of_statement).collect()
        }

        // A loop's purity is its parts'. That a pure loop may never terminate
        // is a separate question, and one the simplification pass answers by
        // refusing to delete loops at all.
        TypedExpression::Loop { body, .. } => purity_of(body),

        TypedExpression::While {
            condition,
            body,
            else_body,
            ..
        } => purity_of(condition)
            .and(purity_of(body))
            .and(else_body.as_ref().map_or(Purity::Pure, |e| purity_of(e))),

        TypedExpression::For {
            iterable,
            body,
            else_body,
            ..
        } => purity_of(iterable)
            .and(purity_of(body))
            .and(else_body.as_ref().map_or(Purity::Pure, |e| purity_of(e))),

        // Pure as *values*: computing what to break or return with does not
        // itself do anything. That control leaves the region is [`escapes`].
        TypedExpression::Break(value) | TypedExpression::Return(value) => {
            value.as_ref().map_or(Purity::Pure, |v| purity_of(v))
        }
    }
}

/// Whether evaluating `statement` can be observed.
pub fn purity_of_statement(statement: &TypedStatement) -> Purity {
    match statement {
        TypedStatement::Expression(expression) => purity_of(expression),
        TypedStatement::Semi(inner) => purity_of_statement(inner),
        TypedStatement::Program { statements } => {
            statements.iter().map(purity_of_statement).collect()
        }

        // Declaring something runs nothing. A function's *body* may be filthy,
        // but that only matters where it is called, and the function's type
        // carries it there.
        TypedStatement::None
        | TypedStatement::ModuleDeclaration { .. }
        | TypedStatement::Use { .. }
        | TypedStatement::StructDeclaration(..)
        | TypedStatement::EnumDeclaration { .. }
        | TypedStatement::UnionDeclaration { .. }
        | TypedStatement::TypeAliasDeclaration { .. }
        | TypedStatement::ProtocolDeclaration { .. }
        | TypedStatement::ImplementationDeclaration { .. }
        | TypedStatement::FunctionDeclaration { .. } => Purity::Pure,
    }
}

fn purity_of_member(member: &Member) -> Purity {
    match member {
        // Reading a binding is not an effect, and neither is naming a static
        // member — naming a function is not calling it.
        Member::Identifier { .. } | Member::StaticMemberAccess { .. } => Purity::Pure,

        Member::MemberAccess { object, .. } => purity_of(object),

        Member::BuiltInFunction(BuiltInFunction { function_type, .. }) => function_type.purity(),

        Member::Index { object, index, .. } => {
            if index_is_in_bounds(object, index) {
                purity_of(object)
            } else {
                Purity::Impure
            }
        }
    }
}

/// Whether an index is provably within the bounds of a literal array.
///
/// Both the array and the index must be literals for this to be decidable.
/// A literal array is a `Tuple` whose type is `Array`; a literal index is a
/// `Literal` whose type carries an integer value. When both are known, the
/// comparison is exact.
fn index_is_in_bounds(object: &TypedExpression, index: &Index) -> bool {
    let TypedExpression::Tuple {
        elements, type_, ..
    } = object
    else {
        return false;
    };

    if !matches!(type_.clone().unsubstitute(), Type::Array(_)) {
        return false;
    }

    let len = elements.len() as u64;

    match index {
        Index::Value(idx) => {
            let Some(ValueLiteral::UInt(idx_val)) =
                ValueLiteral::try_from(idx.get_type().unsubstitute()).ok()
            else {
                return false;
            };
            idx_val < len
        }
        Index::Range {
            start,
            end,
            inclusive,
        } => {
            let start_ok = start
                .as_ref()
                .map(|s| {
                    ValueLiteral::try_from(s.get_type().unsubstitute())
                        .ok()
                        .and_then(|v| match v {
                            ValueLiteral::UInt(u) => Some(u),
                            ValueLiteral::Int(i) if i >= 0 => Some(i as u64),
                            _ => None,
                        })
                        .map(|s| s < len)
                        .unwrap_or(false)
                })
                .unwrap_or(true);

            let end_ok = end
                .as_ref()
                .map(|e| {
                    ValueLiteral::try_from(e.get_type().unsubstitute())
                        .ok()
                        .and_then(|v| match v {
                            ValueLiteral::UInt(u) => Some(u),
                            ValueLiteral::Int(i) if i >= 0 => Some(i as u64),
                            _ => None,
                        })
                        .map(|e| if *inclusive { e < len } else { e <= len })
                        .unwrap_or(false)
                })
                .unwrap_or(true);

            start_ok && end_ok
        }
    }
}

/// The purity a call gets from the thing being called.
///
/// Anything that is not statically known to be a function type answers
/// `Impure`, which is the safe direction: an unknown callee may do anything.
fn purity_of_callee(callee_type: &Type) -> Purity {
    match callee_type.clone().unsubstitute() {
        Type::Function(Function { purity, .. }) => purity,
        _ => Purity::Impure,
    }
}

/// Whether an operator can trap on these operands.
///
/// Only division and remainder can, and only by zero — which the right-hand
/// side's literal type settles outright whenever it is known.
fn purity_of_operator(operator: &BinaryOperator, right: &TypedExpression) -> Purity {
    if !matches!(operator, BinaryOperator::Divide | BinaryOperator::Modulo) {
        return Purity::Pure;
    }

    let divisor = ValueLiteral::try_from(right.get_type().unsubstitute()).ok();

    Purity::of(
        matches!(
        divisor,
        Some(ValueLiteral::Int(d)) if d != 0)
            | matches!(divisor, Some(ValueLiteral::UInt(d)) if d != 0)
            | matches!(divisor, Some(ValueLiteral::Float(d)) if d != 0.0),
    )
}

/// Whether evaluating this can transfer control out of it.
///
/// Not a purity question: `{ return 1; }` is pure by every rule above, and
/// deleting it still changes which value the enclosing function returns. The
/// simplification pass requires *both* before it deletes anything.
pub fn escapes(expression: &TypedExpression) -> bool {
    escapes_within(expression, false)
}

/// Whether `statement` transfers control out of itself.
pub fn statement_escapes(statement: &TypedStatement) -> bool {
    statement_escapes_within(statement, false)
}

/// `in_loop` says whether a `break` or `continue` found here would be caught
/// before leaving the region being asked about. A `return` is never caught — not
/// by a loop, only by a closure, which starts a region of its own.
fn escapes_within(expression: &TypedExpression, in_loop: bool) -> bool {
    let escapes = |e: &TypedExpression| escapes_within(e, in_loop);
    let optional = |e: &Option<Box<TypedExpression>>| e.as_ref().is_some_and(|e| escapes(e));

    match expression {
        TypedExpression::Return(_) => true,
        TypedExpression::Break(_) | TypedExpression::Continue => !in_loop,

        // A closure's body is not run here, and its `return` belongs to it.
        TypedExpression::Closure { .. } => false,

        // The loop catches `break` and `continue` from its body; `return` still
        // passes through. The else body is *not* inside the loop.
        TypedExpression::Loop { body, .. } => escapes_within(body, true),
        TypedExpression::While {
            condition,
            body,
            else_body,
            ..
        } => escapes(condition) || escapes_within(body, true) || optional(else_body),
        TypedExpression::For {
            iterable,
            body,
            else_body,
            ..
        } => escapes(iterable) || escapes_within(body, true) || optional(else_body),

        TypedExpression::Literal { .. } => false,
        TypedExpression::Member(member) => member_escapes(member, in_loop),
        TypedExpression::Unary { expression, .. } => escapes(expression),
        TypedExpression::Binary { left, right, .. } => escapes(left) || escapes(right),
        TypedExpression::Tuple { elements, .. } => elements.iter().any(escapes),
        TypedExpression::Call {
            callee, argument, ..
        } => escapes(callee) || optional(argument),
        TypedExpression::VariableDeclaration { initializer, .. } => optional(initializer),
        TypedExpression::Assignment { initializer, .. } => escapes(initializer),
        TypedExpression::If {
            condition,
            true_expression,
            false_expression,
            ..
        } => escapes(condition) || escapes(true_expression) || optional(false_expression),
        TypedExpression::Match {
            expression, arms, ..
        } => escapes(expression) || arms.iter().any(|arm| escapes(&arm.expression)),
        TypedExpression::Block(Block { statements, .. }) => statements
            .iter()
            .any(|s| statement_escapes_within(s, in_loop)),
    }
}

fn member_escapes(member: &Member, in_loop: bool) -> bool {
    match member {
        Member::Identifier { .. }
        | Member::StaticMemberAccess { .. }
        | Member::BuiltInFunction(..) => false,
        Member::MemberAccess { object, .. } => escapes_within(object, in_loop),
        Member::Index { object, .. } => escapes_within(object, in_loop),
    }
}

fn statement_escapes_within(statement: &TypedStatement, in_loop: bool) -> bool {
    match statement {
        TypedStatement::Expression(expression) => escapes_within(expression, in_loop),
        TypedStatement::Semi(inner) => statement_escapes_within(inner, in_loop),
        TypedStatement::Program { statements } => statements
            .iter()
            .any(|s| statement_escapes_within(s, in_loop)),
        // A declaration runs nothing.
        _ => false,
    }
}

/// Copies the purity of a desugared body onto a declared return type.
///
/// A multi-parameter function is nested single-parameter closures, so its
/// declared return type is itself a function type — built from the annotation,
/// which knows nothing about any body and so says `Impure`. The body's type does
/// know: each nested closure carries the purity of what it wraps.
///
/// Walking the two arrow chains in step puts the right answer on each arrow.
/// Without it every call past the first argument would look impure, and since
/// every multi-argument call in this language is curried, that is nearly all of
/// them.
pub fn carry_body_purity(declared: Type, body: Option<Type>) -> Type {
    let (Type::Function(declared), Some(Type::Function(body))) = (&declared, &body) else {
        return declared;
    };

    Type::Function(Function {
        purity: body.purity,
        identifier: declared.identifier.clone(),
        param: declared.param.clone(),
        return_type: Box::new(carry_body_purity(
            *declared.return_type.clone(),
            Some(*body.return_type.clone()),
        )),
    })
}
