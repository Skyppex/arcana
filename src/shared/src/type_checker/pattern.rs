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
    /// A variant test, with the patterns to apply once it succeeds.
    Variant {
        /// Fully qualified, e.g. `MyEnum::First` — matches the name carried by
        /// the runtime value.
        qualified_name: String,
        variant: String,
        fields: Vec<CheckedFieldPattern>,
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
            CheckedPattern::Variant {
                qualified_name,
                fields,
                ..
            } => {
                write!(f, "{}", qualified_name)?;

                if fields.is_empty() {
                    return Ok(());
                }

                write!(f, " ")?;
                write_fields(f, fields)
            }
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
) -> Result<(CheckedPattern, Vec<PatternBinding>), String> {
    let mut bindings = vec![];
    let checked = check_pattern_inner(pattern, type_, &type_environment, &mut bindings)?;
    Ok((checked, bindings))
}

fn check_pattern_inner(
    pattern: &Pattern,
    type_: &Type,
    type_environment: &Rcrc<TypeEnvironment>,
    bindings: &mut Vec<PatternBinding>,
) -> Result<CheckedPattern, String> {
    match pattern {
        Pattern::Wildcard => Ok(CheckedPattern::Wildcard),
        Pattern::Binding(identifier) => {
            bindings.push((identifier.clone(), type_.clone()));
            Ok(CheckedPattern::Binding(identifier.clone()))
        }
        // Unit has exactly one value, so matching it tests nothing.
        Pattern::Unit => {
            expect_type(&Type::Unit, type_, pattern)?;
            Ok(CheckedPattern::Wildcard)
        }
        Pattern::Bool(v) => {
            expect_type(&Type::Bool, type_, pattern)?;
            Ok(CheckedPattern::Bool(*v))
        }
        Pattern::Int(v) => {
            expect_type(&Type::Int, type_, pattern)?;
            Ok(CheckedPattern::Int(*v))
        }
        Pattern::UInt(v) => {
            expect_type(&Type::UInt, type_, pattern)?;
            Ok(CheckedPattern::UInt(*v))
        }
        Pattern::Float(v) => {
            expect_type(&Type::Float, type_, pattern)?;
            Ok(CheckedPattern::Float(*v))
        }
        Pattern::Rune(v) => {
            expect_type(&Type::Rune, type_, pattern)?;
            Ok(CheckedPattern::Rune(*v))
        }
        Pattern::String(v) => {
            expect_type(&Type::String, type_, pattern)?;
            Ok(CheckedPattern::String(v.clone()))
        }
        Pattern::Comparison { operator, bound } => Ok(CheckedPattern::Comparison {
            operator: *operator,
            bound: check_bound(bound, type_, type_environment)?,
        }),
        Pattern::Range {
            lower,
            upper,
            inclusive,
        } => Ok(CheckedPattern::Range {
            lower: check_bound(lower, type_, type_environment)?,
            upper: check_bound(upper, type_, type_environment)?,
            inclusive: *inclusive,
        }),
        Pattern::Tuple(patterns) => {
            let Type::Tuple(element_types) = type_.clone().unsubstitute() else {
                return Err(format!(
                    "Pattern `{}` expects a tuple but the matched value is {}",
                    pattern, type_
                ));
            };

            if element_types.len() != patterns.len() {
                return Err(format!(
                    "Pattern `{}` has {} elements but the matched tuple has {}",
                    pattern,
                    patterns.len(),
                    element_types.len()
                ));
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
        Pattern::Struct {
            type_annotation,
            fields,
        } => {
            let (available_fields, owner, shared_only) = struct_pattern_fields(type_, pattern)?;

            if let Some(type_annotation) = type_annotation {
                if !type_annotation_equals(type_annotation, &owner) {
                    return Err(format!(
                        "Pattern `{}` names {} but the matched value is {}",
                        pattern, type_annotation, owner
                    ));
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
        Pattern::EnumVariant {
            enum_annotation,
            variant,
            fields,
        } => check_variant_pattern(
            pattern,
            enum_annotation.as_ref(),
            variant,
            fields,
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
) -> Result<(Vec<crate::type_checker::StructField>, TypeAnnotation, bool), String> {
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
        other => Err(format!(
            "Pattern `{}` expects a struct but the matched value is {}",
            pattern, other
        )),
    }
}

fn check_field_patterns(
    fields: &[crate::ast::pattern::FieldPattern],
    available_fields: &[crate::type_checker::StructField],
    owner: &TypeAnnotation,
    shared_only: bool,
    type_environment: &Rcrc<TypeEnvironment>,
    bindings: &mut Vec<PatternBinding>,
) -> Result<Vec<CheckedFieldPattern>, String> {
    let mut checked = vec![];

    for field in fields {
        let Some(struct_field) = get_field_by_name(available_fields, &field.identifier) else {
            return if shared_only {
                Err(format!(
                    "Field `{}` is not shared by all variants of {}; match on a variant first",
                    field.identifier, owner
                ))
            } else {
                Err(format!(
                    "Field `{}` does not exist on {}",
                    field.identifier, owner
                ))
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

#[allow(clippy::too_many_arguments)]
fn check_variant_pattern(
    pattern: &Pattern,
    enum_annotation: Option<&TypeAnnotation>,
    variant: &str,
    fields: &[crate::ast::pattern::FieldPattern],
    type_: &Type,
    type_environment: &Rcrc<TypeEnvironment>,
    bindings: &mut Vec<PatternBinding>,
) -> Result<CheckedPattern, String> {
    match type_.clone().unsubstitute() {
        Type::Enum(Enum {
            type_identifier,
            members,
            ..
        }) => {
            let enum_annotation_of_type = TypeAnnotation::from(&type_identifier);

            if let Some(enum_annotation) = enum_annotation {
                if !type_annotation_equals(enum_annotation, &enum_annotation_of_type) {
                    return Err(format!(
                        "Pattern `{}` names enum {} but the matched value is {}",
                        pattern, enum_annotation, enum_annotation_of_type
                    ));
                }
            }

            let Some(member_type) = get_enum_member(&members, &type_identifier, variant) else {
                return Err(format!(
                    "{} has no variant named `{}`",
                    enum_annotation_of_type, variant
                ));
            };

            let member_fields = variant_fields(member_type)?;
            let owner = TypeAnnotation::Type(format!("{}::{}", enum_annotation_of_type, variant));

            Ok(CheckedPattern::Variant {
                qualified_name: member_type.clone().unsubstitute().to_key(),
                variant: variant.to_owned(),
                fields: check_field_patterns(
                    fields,
                    &member_fields,
                    &owner,
                    false,
                    type_environment,
                    bindings,
                )?,
            })
        }
        // The matched value is already narrowed to one variant, so the variant
        // is known statically: no test is needed, and a different variant can
        // never be held.
        Type::Struct(Struct {
            type_identifier: TypeIdentifier::MemberType(enum_identifier, member_name),
            fields: member_fields,
            ..
        }) => {
            let enum_annotation_of_type = TypeAnnotation::from(enum_identifier.as_ref());

            if let Some(enum_annotation) = enum_annotation {
                if !type_annotation_equals(enum_annotation, &enum_annotation_of_type) {
                    return Err(format!(
                        "Pattern `{}` names enum {} but the matched value is {}",
                        pattern, enum_annotation, enum_annotation_of_type
                    ));
                }
            }

            if member_name != variant {
                return Err(format!(
                    "Pattern `{}` can never match: the value is always {}::{}",
                    pattern, enum_annotation_of_type, member_name
                ));
            }

            let owner = TypeAnnotation::Type(format!("{}::{}", enum_annotation_of_type, variant));

            Ok(CheckedPattern::Fields(check_field_patterns(
                fields,
                &member_fields,
                &owner,
                false,
                type_environment,
                bindings,
            )?))
        }
        other => Err(format!(
            "Pattern `{}` expects an enum but the matched value is {}",
            pattern, other
        )),
    }
}

fn variant_fields(member_type: &Type) -> Result<Vec<crate::type_checker::StructField>, String> {
    match member_type.clone().unsubstitute() {
        Type::Struct(Struct { fields, .. }) => Ok(fields),
        other => Err(format!("Expected an enum variant but found {}", other)),
    }
}

fn check_bound(
    bound: &Bound,
    type_: &Type,
    type_environment: &Rcrc<TypeEnvironment>,
) -> Result<CheckedBound, String> {
    let (checked, bound_type) = match bound {
        Bound::Int(v) => (CheckedBound::Int(*v), Type::Int),
        Bound::UInt(v) => (CheckedBound::UInt(*v), Type::UInt),
        Bound::Float(v) => (CheckedBound::Float(*v), Type::Float),
        Bound::Rune(v) => (CheckedBound::Rune(*v), Type::Rune),
        Bound::Variable(identifier) => {
            let Some(variable_type) = type_environment.borrow().get_variable(identifier) else {
                return Err(format!("Variable `{}` not found", identifier));
            };

            (
                CheckedBound::Variable(identifier.clone()),
                variable_type.unsubstitute(),
            )
        }
    };

    if !type_equals(&bound_type, type_) && !type_equals(type_, &bound_type) {
        return Err(format!(
            "Bound `{}` is {} but the matched value is {}",
            bound, bound_type, type_
        ));
    }

    Ok(checked)
}

fn expect_type(expected: &Type, actual: &Type, pattern: &Pattern) -> Result<(), String> {
    if type_equals(expected, actual) {
        return Ok(());
    }

    Err(format!(
        "Pattern `{}` is {} but the matched value is {}",
        pattern, expected, actual
    ))
}
