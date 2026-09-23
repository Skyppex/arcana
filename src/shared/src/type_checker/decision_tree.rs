//! Compiles match arms into a decision tree.
//!
//! The algorithm is the usual clause-matrix one (Maranget, *Compiling Pattern
//! Matching to Good Decision Trees*), with rows carrying a list of outstanding
//! *obligations* — "the value at this path must match this pattern" — rather
//! than a fixed column per position. That generalisation is what lets two arms
//! name different subsets of a struct's fields, and what makes an enum matched
//! over its shared fields fall out for free: such a row simply has no
//! obligation on the value being discriminated, so it belongs to every branch.

use std::fmt::Display;

use crate::{
    ast::pattern::ComparisonOperator,
    type_checker::{
        model::{Typed, TypedExpression},
        pattern::{CheckedBound, CheckedFieldPattern, CheckedPattern},
        Enum, Type,
    },
    types::ToKey,
};

/// Where a value lives, relative to the value being matched.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AccessPath {
    Root,
    Field(Box<AccessPath>, String),
    TupleIndex(Box<AccessPath>, usize),
}

impl Display for AccessPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AccessPath::Root => write!(f, "<matched value>"),
            AccessPath::Field(parent, name) => write!(f, "{}.{}", parent, name),
            AccessPath::TupleIndex(parent, index) => write!(f, "{}.{}", parent, index),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Occurrence {
    pub path: AccessPath,
    pub type_: Type,
}

impl Occurrence {
    pub fn root(type_: Type) -> Self {
        Occurrence {
            path: AccessPath::Root,
            type_,
        }
    }

    fn field(&self, name: &str, type_: Type) -> Self {
        Occurrence {
            path: AccessPath::Field(Box::new(self.path.clone()), name.to_owned()),
            type_,
        }
    }

    fn tuple_index(&self, index: usize, type_: Type) -> Self {
        Occurrence {
            path: AccessPath::TupleIndex(Box::new(self.path.clone()), index),
            type_,
        }
    }
}

/// A single test against one value.
#[derive(Debug, Clone, PartialEq)]
pub enum Test {
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    Rune(char),
    String(String),
    /// Fully qualified variant name, as carried by the runtime value.
    Variant(String),
    Comparison {
        operator: ComparisonOperator,
        bound: CheckedBound,
    },
    Range {
        lower: CheckedBound,
        upper: CheckedBound,
        inclusive: bool,
    },
}

impl Test {
    /// Constructor tests carve the type into disjoint cases, so a value
    /// matching one cannot match another. Predicate tests overlap freely and
    /// never contribute to exhaustiveness.
    fn is_constructor(&self) -> bool {
        !matches!(self, Test::Comparison { .. } | Test::Range { .. })
    }
}

impl Display for Test {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Test::Bool(v) => write!(f, "{}", v),
            Test::Int(v) => write!(f, "{}", v),
            Test::UInt(v) => write!(f, "{}u", v),
            Test::Float(v) => write!(f, "{}f", v),
            Test::Rune(v) => write!(f, "'{}'", v),
            Test::String(v) => write!(f, "\"{}\"", v),
            Test::Variant(name) => write!(f, "{}", name),
            Test::Comparison { operator, bound } => write!(f, "{} {}", operator, bound),
            Test::Range {
                lower,
                upper,
                inclusive,
            } => write!(
                f,
                "{}..{}{}",
                lower,
                if *inclusive { "=" } else { "" },
                upper
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub identifier: String,
    pub occurrence: Occurrence,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Case {
    pub test: Test,
    pub decision: Decision,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decision {
    /// An arm matched. Its bindings are projected from the matched value, then
    /// its body is evaluated.
    Success {
        arm: usize,
        bindings: Vec<Binding>,
        body: Box<TypedExpression>,
        type_: Type,
    },
    /// Nothing matched. Exhaustiveness checking rejects any match that can
    /// reach this, so it only survives as a defensive internal error.
    Failure { witness: String },
    /// Test one value, take the first case whose test succeeds, else the
    /// default. A missing default means the cases are exhaustive.
    Switch {
        occurrence: Occurrence,
        cases: Vec<Case>,
        default: Option<Box<Decision>>,
        type_: Type,
    },
}

impl Typed for Decision {
    fn get_type(&self) -> Type {
        match self {
            Decision::Success { type_, .. } => type_.clone(),
            Decision::Failure { .. } => Type::Unknown,
            Decision::Switch { type_, .. } => type_.clone(),
        }
    }

    fn get_deep_type(&self) -> Type {
        self.get_type()
    }
}

/// One arm, ready to compile: its pattern resolved against the matched type and
/// its body already checked in a scope holding the pattern's bindings.
#[derive(Debug, Clone)]
pub struct CompilableArm {
    pub pattern: CheckedPattern,
    pub body: TypedExpression,
}

#[derive(Debug, Clone)]
struct Row {
    /// Outstanding "value at this occurrence must match this pattern" pairs.
    obligations: Vec<(Occurrence, CheckedPattern)>,
    bindings: Vec<Binding>,
    arm: usize,
}

impl Row {
    /// Expands everything irrefutable — bindings, tuples, field projections —
    /// until only real tests remain.
    fn normalize(&mut self) {
        let mut pending = std::mem::take(&mut self.obligations);

        while let Some((occurrence, pattern)) = pending.pop() {
            match pattern {
                CheckedPattern::Wildcard => {}
                CheckedPattern::Binding(identifier) => self.bindings.push(Binding {
                    identifier,
                    occurrence,
                }),
                CheckedPattern::Tuple(patterns) => {
                    let element_types = match occurrence.type_.clone().unsubstitute() {
                        Type::Tuple(types) => types,
                        // Checking has already rejected this; nothing to expand.
                        _ => continue,
                    };

                    for (index, element) in patterns.into_iter().enumerate().rev() {
                        let element_type =
                            element_types.get(index).cloned().unwrap_or(Type::Unknown);

                        pending.push((occurrence.tuple_index(index, element_type), element));
                    }
                }
                CheckedPattern::Fields(fields) => {
                    for field in fields.into_iter().rev() {
                        pending.push(field_obligation(&occurrence, field));
                    }
                }
                test => self.obligations.push((occurrence, test)),
            }
        }

        self.obligations.reverse();
    }

    fn obligation_at(&self, occurrence: &Occurrence) -> Option<&CheckedPattern> {
        self.obligations
            .iter()
            .find(|(o, _)| o.path == occurrence.path)
            .map(|(_, pattern)| pattern)
    }

    /// The same row with its obligation at `occurrence` removed, and `extra`
    /// added in its place.
    fn without_obligation_at(
        &self,
        occurrence: &Occurrence,
        extra: Vec<(Occurrence, CheckedPattern)>,
    ) -> Row {
        let mut row = self.clone();
        row.obligations.retain(|(o, _)| o.path != occurrence.path);
        row.obligations.extend(extra);
        row.normalize();
        row
    }
}

fn field_obligation(
    occurrence: &Occurrence,
    field: CheckedFieldPattern,
) -> (Occurrence, CheckedPattern) {
    (
        occurrence.field(&field.identifier, field.type_),
        field.pattern,
    )
}

pub struct CompiledMatch {
    pub decision: Decision,
    /// Arms that no value can reach.
    pub unreachable_arms: Vec<usize>,
}

/// Compiles `arms` into a decision tree over `matchee_type`.
///
/// Fails if the arms are not exhaustive. Unreachable arms are reported rather
/// than rejected here, so the caller can attach them to source patterns.
pub fn compile_match(
    matchee_type: Type,
    arms: &[CompilableArm],
    body_type: Type,
) -> Result<CompiledMatch, String> {
    let root = Occurrence::root(matchee_type);

    let rows = arms
        .iter()
        .enumerate()
        .map(|(arm, compilable)| {
            let mut row = Row {
                obligations: vec![(root.clone(), compilable.pattern.clone())],
                bindings: vec![],
                arm,
            };

            row.normalize();
            row
        })
        .collect::<Vec<_>>();

    let mut reached = vec![false; arms.len()];
    let decision = compile_rows(rows, arms, &body_type, &mut reached)?;

    Ok(CompiledMatch {
        decision,
        unreachable_arms: reached
            .into_iter()
            .enumerate()
            .filter(|(_, reached)| !reached)
            .map(|(arm, _)| arm)
            .collect(),
    })
}

fn compile_rows(
    rows: Vec<Row>,
    arms: &[CompilableArm],
    body_type: &Type,
    reached: &mut [bool],
) -> Result<Decision, String> {
    let Some(first) = rows.first() else {
        return Ok(Decision::Failure {
            witness: String::new(),
        });
    };

    // Every obligation discharged: this arm matches whatever got us here.
    if first.obligations.is_empty() {
        reached[first.arm] = true;

        return Ok(Decision::Success {
            arm: first.arm,
            bindings: first.bindings.clone(),
            body: Box::new(arms[first.arm].body.clone()),
            type_: body_type.clone(),
        });
    }

    // Leftmost outstanding obligation of the first unsatisfied arm.
    let (occurrence, head) = first.obligations[0].clone();
    let head_test = pattern_test(&head);

    if head_test.is_constructor() {
        compile_constructor_switch(rows, &occurrence, arms, body_type, reached)
    } else {
        compile_predicate_switch(rows, &occurrence, head_test, arms, body_type, reached)
    }
}

/// Switches on a value's constructor: every distinct constructor named at this
/// occurrence becomes a case, and rows that don't constrain it ride along into
/// all of them.
fn compile_constructor_switch(
    rows: Vec<Row>,
    occurrence: &Occurrence,
    arms: &[CompilableArm],
    body_type: &Type,
    reached: &mut [bool],
) -> Result<Decision, String> {
    let mut tests: Vec<Test> = vec![];

    for row in &rows {
        if let Some(pattern) = row.obligation_at(occurrence) {
            let test = pattern_test(pattern);

            if test.is_constructor() && !tests.contains(&test) {
                tests.push(test);
            }
        }
    }

    let mut cases = vec![];

    for test in &tests {
        let mut case_rows = vec![];

        for row in &rows {
            match row.obligation_at(occurrence) {
                // Unconstrained here, so it survives whatever this value is.
                None => case_rows.push(row.clone()),
                Some(pattern) => {
                    let row_test = pattern_test(pattern);

                    if &row_test == test {
                        let extra = match pattern {
                            CheckedPattern::Variant { fields, .. } => fields
                                .iter()
                                .map(|field| field_obligation(occurrence, field.clone()))
                                .collect(),
                            _ => vec![],
                        };

                        case_rows.push(row.without_obligation_at(occurrence, extra));
                    } else if !row_test.is_constructor() {
                        // A predicate can still hold for this constructor, so
                        // keep the row and its obligation.
                        case_rows.push(row.clone());
                    }
                }
            }
        }

        cases.push(Case {
            test: test.clone(),
            decision: compile_rows(case_rows, arms, body_type, reached)?,
        });
    }

    let default_rows = rows
        .iter()
        .filter(|row| match row.obligation_at(occurrence) {
            None => true,
            Some(pattern) => !pattern_test(pattern).is_constructor(),
        })
        .cloned()
        .collect::<Vec<_>>();

    let missing = missing_constructors(&occurrence.type_, &tests);

    // A complete signature needs no default: the cases cover every value.
    if let Some(missing) = &missing {
        if missing.is_empty() {
            return Ok(Decision::Switch {
                occurrence: occurrence.clone(),
                cases,
                default: None,
                type_: body_type.clone(),
            });
        }
    }

    if default_rows.is_empty() {
        return Err(non_exhaustive_error(occurrence, &missing));
    }

    let default = compile_rows(default_rows, arms, body_type, reached)?;

    if let Decision::Failure { .. } = default {
        return Err(non_exhaustive_error(occurrence, &missing));
    }

    Ok(Decision::Switch {
        occurrence: occurrence.clone(),
        cases,
        default: Some(Box::new(default)),
        type_: body_type.clone(),
    })
}

/// Switches on a predicate — a range or comparison. These never partition the
/// type, so there is exactly one case and a default is always required.
fn compile_predicate_switch(
    rows: Vec<Row>,
    occurrence: &Occurrence,
    test: Test,
    arms: &[CompilableArm],
    body_type: &Type,
    reached: &mut [bool],
) -> Result<Decision, String> {
    let mut case_rows = vec![];

    for row in &rows {
        match row.obligation_at(occurrence) {
            None => case_rows.push(row.clone()),
            Some(pattern) => {
                if pattern_test(pattern) == test {
                    case_rows.push(row.without_obligation_at(occurrence, vec![]));
                } else {
                    // Another test on the same value may still hold once this
                    // predicate has; keep it to be tested inside the branch.
                    case_rows.push(row.clone());
                }
            }
        }
    }

    let default_rows = rows
        .iter()
        .filter(|row| match row.obligation_at(occurrence) {
            None => true,
            Some(pattern) => pattern_test(pattern) != test,
        })
        .cloned()
        .collect::<Vec<_>>();

    if default_rows.is_empty() {
        return Err(non_exhaustive_error(occurrence, &None));
    }

    let default = compile_rows(default_rows, arms, body_type, reached)?;

    if let Decision::Failure { .. } = default {
        return Err(non_exhaustive_error(occurrence, &None));
    }

    Ok(Decision::Switch {
        occurrence: occurrence.clone(),
        cases: vec![Case {
            test,
            decision: compile_rows(case_rows, arms, body_type, reached)?,
        }],
        default: Some(Box::new(default)),
        type_: body_type.clone(),
    })
}

fn pattern_test(pattern: &CheckedPattern) -> Test {
    match pattern {
        CheckedPattern::Bool(v) => Test::Bool(*v),
        CheckedPattern::Int(v) => Test::Int(*v),
        CheckedPattern::UInt(v) => Test::UInt(*v),
        CheckedPattern::Float(v) => Test::Float(*v),
        CheckedPattern::Rune(v) => Test::Rune(*v),
        CheckedPattern::String(v) => Test::String(v.clone()),
        CheckedPattern::Variant { qualified_name, .. } => Test::Variant(qualified_name.clone()),
        CheckedPattern::Comparison { operator, bound } => Test::Comparison {
            operator: *operator,
            bound: bound.clone(),
        },
        CheckedPattern::Range {
            lower,
            upper,
            inclusive,
        } => Test::Range {
            lower: lower.clone(),
            upper: upper.clone(),
            inclusive: *inclusive,
        },
        // Normalization removes these before a test is ever asked for.
        CheckedPattern::Wildcard
        | CheckedPattern::Binding(_)
        | CheckedPattern::Tuple(_)
        | CheckedPattern::Fields(_) => {
            unreachable!("irrefutable patterns are expanded before testing")
        }
    }
}

/// The constructors of `type_` that `covered` leaves out, or `None` when the
/// type has no finite set of constructors.
fn missing_constructors(type_: &Type, covered: &[Test]) -> Option<Vec<String>> {
    match type_.clone().unsubstitute() {
        Type::Bool => {
            let mut missing = vec![];

            for value in [true, false] {
                if !covered.contains(&Test::Bool(value)) {
                    missing.push(value.to_string());
                }
            }

            Some(missing)
        }
        Type::Enum(Enum { members, .. }) => {
            let mut missing = vec![];

            for member_type in members.values() {
                let name = member_type.clone().unsubstitute().to_key();

                if !covered.contains(&Test::Variant(name.clone())) {
                    missing.push(name);
                }
            }

            missing.sort();
            Some(missing)
        }
        _ => None,
    }
}

fn non_exhaustive_error(occurrence: &Occurrence, missing: &Option<Vec<String>>) -> String {
    let at = match occurrence.path {
        AccessPath::Root => String::new(),
        _ => format!(" at {}", occurrence.path),
    };

    match missing {
        Some(missing) if !missing.is_empty() => format!(
            "Match is not exhaustive{}: no arm covers {}",
            at,
            missing
                .iter()
                .map(|m| format!("`{}`", m))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        _ => format!(
            "Match is not exhaustive{}: {} is not fully covered, add a catch-all arm",
            at, occurrence.type_
        ),
    }
}
