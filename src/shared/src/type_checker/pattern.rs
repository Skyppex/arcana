use crate::ast::pattern::PatternKind;
use crate::diagnostic::{Diagnostic, Spanned};
use std::fmt::Display;

use crate::{
    ast::pattern::{Bound, ComparisonOperator, Pattern},
    type_checker::{
        get_enum_member, get_field_by_name, type_annotation_equals, type_equals, Enum, Struct,
        Type, TypeEnvironment,
    },
    types::{ToKey, TypeAnnotation, TypeIdentifier},
};

use super::Rcrc;

/// A pattern that has been resolved against the type of the value it matches.
///
/// Where the source form leaves things to inference — an untyped `{ .. }`, an
/// unqualified `::First` — this form has them settled, and anything that turned
/// out to be irrefutable has collapsed into a plain projection.
#[derive(Debug, Clone, PartialEq)]
pub enum CheckedPattern {
    /// Matches anything, binds nothing.
    Wildcard,
    /// Matches anything, binds the value.
    Binding(String),
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    Rune(char),
    String(String),
    Comparison {
        operator: ComparisonOperator,
        bound: CheckedBound,
    },
    Range {
        lower: CheckedBound,
        upper: CheckedBound,
        inclusive: bool,
    },
    Tuple(Vec<CheckedPattern>),
    /// An irrefutable projection into fields: a struct pattern, an enum matched
    /// over its shared fields, or a variant pattern whose variant was already
    /// statically known.
    Fields(Vec<CheckedFieldPattern>),
    /// Binds the value and also matches it against `inner`.
    Bound {
        identifier: String,
        inner: Box<CheckedPattern>,
    },
    /// A variant test, with the pattern to apply once it succeeds.
    Variant {
        /// Fully qualified, e.g. `MyEnum::First` — matches the name carried by
        /// the runtime value.
        qualified_name: String,
        variant: String,
        /// Applied to the value once the test succeeds, at the variant's own
        /// type. The value itself does not change, so this needs no projection.
        inner: Box<CheckedPattern>,
    },
    /// A test for a variant that is itself an enum, narrowing the value to that
    /// enum before applying `inner`.
    ///
    /// The narrowing is only a change of static type: the runtime value carries
    /// its full path, so nothing is projected out of it.
    NestedVariant {
        /// Fully qualified, e.g. `E1::E2` — the prefix carried by the runtime
        /// value of any variant declared inside it.
        qualified_name: String,
        variant: String,
        /// The nested enum's type, which the narrowed value is matched at.
        type_: Type,
        inner: Box<CheckedPattern>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct CheckedFieldPattern {
    pub identifier: String,
    pub type_: Type,
    pub pattern: CheckedPattern,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CheckedBound {
    Int(i64),
    UInt(u64),
    Float(f64),
    Rune(char),
    Variable(String),
}

impl Display for CheckedBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CheckedBound::Int(v) => write!(f, "{}", v),
            CheckedBound::UInt(v) => write!(f, "{}u", v),
            CheckedBound::Float(v) => write!(f, "{}f", v),
            CheckedBound::Rune(v) => write!(f, "'{}'", v),
            CheckedBound::Variable(v) => write!(f, "{}", v),
        }
    }
}

impl Display for CheckedPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CheckedPattern::Wildcard => write!(f, "_"),
            CheckedPattern::Binding(name) => write!(f, "{}", name),
            CheckedPattern::Bool(v) => write!(f, "{}", v),
            CheckedPattern::Int(v) => write!(f, "{}", v),
            CheckedPattern::UInt(v) => write!(f, "{}u", v),
            CheckedPattern::Float(v) => write!(f, "{}f", v),
            CheckedPattern::Rune(v) => write!(f, "'{}'", v),
            CheckedPattern::String(v) => write!(f, "\"{}\"", v),
            CheckedPattern::Comparison { operator, bound } => write!(f, "{} {}", operator, bound),
            CheckedPattern::Range {
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
            CheckedPattern::Tuple(patterns) => write!(
                f,
                "({})",
                patterns
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            CheckedPattern::Fields(fields) => write_fields(f, fields),
            CheckedPattern::NestedVariant {
                qualified_name,
                inner,
                ..
            } => write!(f, "{} {}", qualified_name, inner),
            CheckedPattern::Bound { identifier, inner } => write!(f, "{} @ {}", identifier, inner),
            CheckedPattern::Variant {
                qualified_name,
                inner,
                ..
            } => write!(f, "{} {}", qualified_name, inner),
        }
    }
}

fn write_fields(
    f: &mut std::fmt::Formatter<'_>,
    fields: &[CheckedFieldPattern],
) -> std::fmt::Result {
    write!(
        f,
        "{{ {} }}",
        fields
            .iter()
            .map(|field| format!("{}: {}", field.identifier, field.pattern))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

/// A name the pattern binds, and the type it binds at.
pub type PatternBinding = (String, Type);

/// Resolves `pattern` against the type of the value it will match, returning
/// the checked form and every name it binds.
pub fn check_pattern(
    pattern: &Pattern,
    type_: &Type,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<(CheckedPattern, Vec<PatternBinding>), Diagnostic> {
    pattern.check_no_duplicate_bindings()?;

    let mut bindings = vec![];
    let checked = check_pattern_inner(pattern, type_, &type_environment, &mut bindings)?;
    Ok((checked, bindings))
}

/// Checks one pattern, pointing any error at it.
///
/// Same arrangement as the expression and statement checkers: attaching the
/// span once here, and only to an empty primary, leaves an error from a
/// sub-pattern pointing at that sub-pattern.
fn check_pattern_inner(
    pattern: &Pattern,
    type_: &Type,
    type_environment: &Rcrc<TypeEnvironment>,
    bindings: &mut Vec<PatternBinding>,
) -> Result<CheckedPattern, Diagnostic> {
    check_pattern_of(pattern, type_, type_environment, bindings).at(pattern.span)
}

fn check_pattern_of(
    pattern: &Pattern,
    type_: &Type,
    type_environment: &Rcrc<TypeEnvironment>,
    bindings: &mut Vec<PatternBinding>,
) -> Result<CheckedPattern, Diagnostic> {
    match &pattern.kind {
        PatternKind::Wildcard => Ok(CheckedPattern::Wildcard),
        PatternKind::Binding(identifier) => {
            bindings.push((identifier.clone(), type_.clone()));
            Ok(CheckedPattern::Binding(identifier.clone()))
        }
        // `x @ p` binds the value and goes on matching it, so both apply at the
        // same type.
        PatternKind::Bound {
            identifier,
            pattern: inner,
        } => {
            bindings.push((identifier.clone(), type_.clone()));

            Ok(CheckedPattern::Bound {
                identifier: identifier.clone(),
                inner: Box::new(check_pattern_inner(
                    inner,
                    type_,
                    type_environment,
                    bindings,
                )?),
            })
        }
        // Unit has exactly one value, so matching it tests nothing.
        PatternKind::Unit => {
            expect_type(&Type::Unit, type_, pattern)?;
            Ok(CheckedPattern::Wildcard)
        }
        PatternKind::Bool(v) => {
            expect_type(&Type::Bool, type_, pattern)?;
            Ok(CheckedPattern::Bool(*v))
        }
        PatternKind::Int(v) => {
            expect_type(&Type::Int, type_, pattern)?;
            Ok(CheckedPattern::Int(*v))
        }
        PatternKind::UInt(v) => {
            expect_type(&Type::UInt, type_, pattern)?;
            Ok(CheckedPattern::UInt(*v))
        }
        PatternKind::Float(v) => {
            expect_type(&Type::Float, type_, pattern)?;
            Ok(CheckedPattern::Float(*v))
        }
        PatternKind::Rune(v) => {
            expect_type(&Type::Rune, type_, pattern)?;
            Ok(CheckedPattern::Rune(*v))
        }
        PatternKind::String(v) => {
            expect_type(&Type::String, type_, pattern)?;
            Ok(CheckedPattern::String(v.clone()))
        }
        PatternKind::Comparison { operator, bound } => Ok(CheckedPattern::Comparison {
            operator: *operator,
            bound: check_bound(bound, type_, type_environment)?,
        }),
        PatternKind::Range {
            lower,
            upper,
            inclusive,
        } => Ok(CheckedPattern::Range {
            lower: check_bound(lower, type_, type_environment)?,
            upper: check_bound(upper, type_, type_environment)?,
            inclusive: *inclusive,
        }),
        PatternKind::Tuple(patterns) => {
            let Type::Tuple(element_types) = type_.clone().unsubstitute() else {
                return Err(Diagnostic::error(format!(
                    "Pattern `{}` expects a tuple but the matched value is {}",
                    pattern, type_
                )));
            };

            if element_types.len() != patterns.len() {
                return Err(Diagnostic::error(format!(
                    "Pattern `{}` has {} elements but the matched tuple has {}",
                    pattern,
                    patterns.len(),
                    element_types.len()
                )));
            }

            let mut checked = vec![];

            for (element_pattern, element_type) in patterns.iter().zip(element_types.iter()) {
                checked.push(check_pattern_inner(
                    element_pattern,
                    element_type,
                    type_environment,
                    bindings,
                )?);
            }

            Ok(CheckedPattern::Tuple(checked))
        }
        PatternKind::Struct {
            type_annotation,
            fields,
        } => {
            let (available_fields, owner, shared_only) = struct_pattern_fields(type_, pattern)?;

            if let Some(type_annotation) = type_annotation {
                if !type_annotation_equals(type_annotation, &owner) {
                    return Err(Diagnostic::error(format!(
                        "Pattern `{}` names {} but the matched value is {}",
                        pattern, type_annotation, owner
                    )));
                }
            }

            let checked = check_field_patterns(
                fields,
                &available_fields,
                &owner,
                shared_only,
                type_environment,
                bindings,
            )?;

            Ok(CheckedPattern::Fields(checked))
        }
        PatternKind::EnumVariant {
            enum_annotation,
            path,
            inner,
        } => check_variant_pattern(
            pattern,
            enum_annotation.as_ref(),
            path,
            inner.as_deref(),
            type_,
            type_environment,
            bindings,
        ),
    }
}

/// The fields a `::`-free pattern can see, the type annotation naming their
/// owner, and whether the set is restricted to an enum's shared fields.
fn struct_pattern_fields(
    type_: &Type,
    pattern: &Pattern,
) -> Result<(Vec<crate::type_checker::StructField>, TypeAnnotation, bool), Diagnostic> {
    match type_.clone().unsubstitute() {
        Type::Struct(Struct {
            type_identifier,
            fields,
            ..
        }) => Ok((fields, TypeAnnotation::from(&type_identifier), false)),
        // A shared field exists on every variant, so it can be read without
        // knowing which variant is held.
        Type::Enum(Enum {
            type_identifier,
            shared_fields,
            ..
        }) => Ok((shared_fields, TypeAnnotation::from(&type_identifier), true)),
        other => Err(Diagnostic::error(format!(
            "Pattern `{}` expects a struct but the matched value is {}",
            pattern, other
        ))),
    }
}

fn check_field_patterns(
    fields: &[crate::ast::pattern::FieldPattern],
    available_fields: &[crate::type_checker::StructField],
    owner: &TypeAnnotation,
    shared_only: bool,
    type_environment: &Rcrc<TypeEnvironment>,
    bindings: &mut Vec<PatternBinding>,
) -> Result<Vec<CheckedFieldPattern>, Diagnostic> {
    let mut checked = vec![];

    for field in fields {
        let Some(struct_field) = get_field_by_name(available_fields, &field.identifier) else {
            return if shared_only {
                Err(Diagnostic::error(format!(
                    "Field `{}` is not shared by all variants of {}; match on a variant first",
                    field.identifier, owner
                )))
            } else {
                Err(Diagnostic::error(format!(
                    "Field `{}` does not exist on {}",
                    field.identifier, owner
                )))
            };
        };

        let field_type = struct_field.field_type.clone();

        checked.push(CheckedFieldPattern {
            identifier: field.identifier.clone(),
            pattern: check_pattern_inner(&field.pattern, &field_type, type_environment, bindings)?,
            type_: field_type,
        });
    }

    Ok(checked)
}

fn check_variant_pattern(
    pattern: &Pattern,
    enum_annotation: Option<&TypeAnnotation>,
    path: &[String],
    inner: Option<&Pattern>,
    type_: &Type,
    type_environment: &Rcrc<TypeEnvironment>,
    bindings: &mut Vec<PatternBinding>,
) -> Result<CheckedPattern, Diagnostic> {
    match type_.clone().unsubstitute() {
        Type::Enum(enum_) => {
            let enum_annotation_of_type = TypeAnnotation::from(&enum_.type_identifier);

            if let Some(enum_annotation) = enum_annotation {
                if !type_annotation_equals(enum_annotation, &enum_annotation_of_type) {
                    return Err(Diagnostic::error(format!(
                        "Pattern `{}` names enum {} but the matched value is {}",
                        pattern, enum_annotation, enum_annotation_of_type
                    )));
                }
            }

            walk_variant_path(pattern, &enum_, path, inner, type_environment, bindings)
        }
        // The matched value is already narrowed to one variant, so the variant
        // is known statically: no test is needed, and a different variant can
        // never be held.
        Type::Struct(Struct {
            type_identifier: TypeIdentifier::MemberType(enum_identifier, member_name),
            ..
        }) => {
            let enum_annotation_of_type = TypeAnnotation::from(enum_identifier.as_ref());

            if let Some(enum_annotation) = enum_annotation {
                if !type_annotation_equals(enum_annotation, &enum_annotation_of_type) {
                    return Err(Diagnostic::error(format!(
                        "Pattern `{}` names enum {} but the matched value is {}",
                        pattern, enum_annotation, enum_annotation_of_type
                    )));
                }
            }

            match path {
                [variant] if *variant == member_name => {}
                _ => {
                    return Err(Diagnostic::error(format!(
                        "Pattern `{}` can never match: the value is always {}::{}",
                        pattern, enum_annotation_of_type, member_name
                    )))
                }
            }

            check_variant_inner(inner, type_, type_environment, bindings)
        }
        other => Err(Diagnostic::error(format!(
            "Pattern `{}` expects an enum but the matched value is {}",
            pattern, other
        ))),
    }
}

/// Resolves a variant path one segment at a time, against the enum each segment
/// belongs to.
///
/// Every segment but the last has to name a nested enum; the last may name
/// either a nested enum or a struct variant, and is where the pattern's inner
/// binding or field patterns apply.
fn walk_variant_path(
    pattern: &Pattern,
    enum_: &Enum,
    path: &[String],
    inner: Option<&Pattern>,
    type_environment: &Rcrc<TypeEnvironment>,
    bindings: &mut Vec<PatternBinding>,
) -> Result<CheckedPattern, Diagnostic> {
    let enum_annotation_of_type = TypeAnnotation::from(&enum_.type_identifier);

    let Some((variant, rest)) = path.split_first() else {
        return Err(Diagnostic::error(format!(
            "Pattern `{}` names no variant",
            pattern
        )));
    };

    let Some(member_type) = get_enum_member(&enum_.members, &enum_.type_identifier, variant) else {
        return Err(Diagnostic::error(format!(
            "{} has no variant named `{}`",
            enum_annotation_of_type, variant
        )));
    };

    let qualified_name = member_type.clone().unsubstitute().to_key();

    // A segment in the middle of the path has to be an enum for the next
    // segment to mean anything.
    if !rest.is_empty() {
        let Type::Enum(nested) = member_type.clone().unsubstitute() else {
            return Err(Diagnostic::error(format!(
                "`{}` is not an enum, so it has no variant `{}`",
                qualified_name, rest[0]
            )));
        };

        return Ok(CheckedPattern::NestedVariant {
            qualified_name,
            variant: variant.clone(),
            type_: member_type.clone(),
            inner: Box::new(walk_variant_path(
                pattern,
                &nested,
                rest,
                inner,
                type_environment,
                bindings,
            )?),
        });
    }

    let checked_inner = check_variant_inner(inner, member_type, type_environment, bindings)?;

    // A nested enum keeps its own occurrence, so that matching it further is
    // checked against its own set of variants.
    if matches!(member_type.clone().unsubstitute(), Type::Enum(_)) {
        return Ok(CheckedPattern::NestedVariant {
            qualified_name,
            variant: variant.clone(),
            type_: member_type.clone(),
            inner: Box::new(checked_inner),
        });
    }

    Ok(CheckedPattern::Variant {
        qualified_name,
        variant: variant.clone(),
        inner: Box::new(checked_inner),
    })
}

/// The pattern applied to a value once its variant is known, checked at that
/// variant's own type.
///
/// Nothing written means nothing more to match, which is why a bare `::S1`
/// matches any `S1`.
fn check_variant_inner(
    inner: Option<&Pattern>,
    member_type: &Type,
    type_environment: &Rcrc<TypeEnvironment>,
    bindings: &mut Vec<PatternBinding>,
) -> Result<CheckedPattern, Diagnostic> {
    let Some(inner) = inner else {
        return Ok(CheckedPattern::Wildcard);
    };

    check_pattern_inner(inner, member_type, type_environment, bindings)
}

fn check_bound(
    bound: &Bound,
    type_: &Type,
    type_environment: &Rcrc<TypeEnvironment>,
) -> Result<CheckedBound, Diagnostic> {
    let (checked, bound_type) = match bound {
        Bound::Int(v) => (CheckedBound::Int(*v), Type::Int),
        Bound::UInt(v) => (CheckedBound::UInt(*v), Type::UInt),
        Bound::Float(v) => (CheckedBound::Float(*v), Type::Float),
        Bound::Rune(v) => (CheckedBound::Rune(*v), Type::Rune),
        Bound::Variable(identifier) => {
            let Some(variable_type) = type_environment.borrow().get_variable(identifier) else {
                return Err(Diagnostic::error(format!(
                    "Variable `{}` not found",
                    identifier
                )));
            };

            (
                CheckedBound::Variable(identifier.clone()),
                variable_type.unsubstitute(),
            )
        }
    };

    if !type_equals(&bound_type, type_) && !type_equals(type_, &bound_type) {
        return Err(Diagnostic::error(format!(
            "Bound `{}` is {} but the matched value is {}",
            bound, bound_type, type_
        )));
    }

    Ok(checked)
}

fn expect_type(expected: &Type, actual: &Type, pattern: &Pattern) -> Result<(), Diagnostic> {
    if type_equals(expected, actual) {
        return Ok(());
    }

    Err(Diagnostic::error(format!(
        "Pattern `{}` is {} but the matched value is {}",
        pattern, expected, actual
    )))
}
