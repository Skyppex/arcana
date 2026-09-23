use std::fmt::Display;

use crate::types::TypeAnnotation;

/// A syntactic pattern, as written in source.
///
/// Constructor patterns are split by spelling, not by inference: a pattern
/// containing `::` is always an enum variant and never a struct, and a pattern
/// without `::` is always a struct and never an enum variant. An enum variant
/// *is* a struct, so once the variant is statically known the struct forms
/// apply to it directly.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// `_`
    Wildcard,
    /// `unit`
    Unit,
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    Rune(char),
    String(String),
    /// `x` — binds the matched value.
    Binding(String),
    /// `< 5`, `>= x`
    Comparison {
        operator: ComparisonOperator,
        bound: Bound,
    },
    /// `1..10`, `1..=10`
    Range {
        lower: Bound,
        upper: Bound,
        inclusive: bool,
    },
    /// `(a, 1, _)`
    Tuple(Vec<Pattern>),
    /// `{ x, y: 1 }` or `Point { x, y: 1 }`.
    ///
    /// Over an enum-typed value this sees the enum's shared fields, which exist
    /// on every variant, and nothing else.
    Struct {
        type_annotation: Option<TypeAnnotation>,
        fields: Vec<FieldPattern>,
    },
    /// `MyEnum::First { x }` or `::First { x }`. The annotation is absent for
    /// the unqualified form, where the enum comes from the matched value.
    EnumVariant {
        enum_annotation: Option<TypeAnnotation>,
        variant: String,
        fields: Vec<FieldPattern>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonOperator {
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

impl Display for ComparisonOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ComparisonOperator::LessThan => write!(f, "<"),
            ComparisonOperator::GreaterThan => write!(f, ">"),
            ComparisonOperator::LessThanOrEqual => write!(f, "<="),
            ComparisonOperator::GreaterThanOrEqual => write!(f, ">="),
        }
    }
}

/// The endpoint of a range or comparison pattern: a numeric literal, or a
/// variable resolved from the enclosing scope.
#[derive(Debug, Clone, PartialEq)]
pub enum Bound {
    Int(i64),
    UInt(u64),
    Float(f64),
    Rune(char),
    Variable(String),
}

impl Display for Bound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Bound::Int(v) => write!(f, "{}", v),
            Bound::UInt(v) => write!(f, "{}u", v),
            Bound::Float(v) => write!(f, "{}f", v),
            Bound::Rune(v) => write!(f, "'{}'", v),
            Bound::Variable(v) => write!(f, "{}", v),
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
        match &self.pattern {
            // `{ x }` rather than `{ x: x }`
            Pattern::Binding(name) if name == &self.identifier => write!(f, "{}", name),
            pattern => write!(f, "{}: {}", self.identifier, pattern),
        }
    }
}

impl Pattern {
    /// Whether this pattern matches every value of its type, so that no test
    /// needs to be emitted for it.
    ///
    /// Constructor patterns are only irrefutable relative to a type — a struct
    /// pattern always is, an enum variant pattern is when the enum has a single
    /// variant — so those answer `false` here and are settled during checking.
    pub fn is_unconditionally_irrefutable(&self) -> bool {
        match self {
            Pattern::Wildcard | Pattern::Binding(_) | Pattern::Unit => true,
            Pattern::Struct { fields, .. } => fields
                .iter()
                .all(|f| f.pattern.is_unconditionally_irrefutable()),
            Pattern::Tuple(patterns) => patterns.iter().all(|p| p.is_unconditionally_irrefutable()),
            _ => false,
        }
    }

    /// Every name this pattern binds, in source order.
    pub fn bindings(&self) -> Vec<String> {
        let mut names = vec![];
        self.collect_bindings(&mut names);
        names
    }

    fn collect_bindings(&self, names: &mut Vec<String>) {
        match self {
            Pattern::Binding(name) => names.push(name.clone()),
            Pattern::Tuple(patterns) => {
                for pattern in patterns {
                    pattern.collect_bindings(names);
                }
            }
            Pattern::Struct { fields, .. } | Pattern::EnumVariant { fields, .. } => {
                for field in fields {
                    field.pattern.collect_bindings(names);
                }
            }
            _ => {}
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Wildcard => write!(f, "_"),
            Pattern::Unit => write!(f, "unit"),
            Pattern::Bool(v) => write!(f, "{}", v),
            Pattern::Int(v) => write!(f, "{}", v),
            Pattern::UInt(v) => write!(f, "{}u", v),
            Pattern::Float(v) => write!(f, "{}f", v),
            Pattern::Rune(v) => write!(f, "'{}'", v),
            Pattern::String(v) => write!(f, "\"{}\"", v),
            Pattern::Binding(v) => write!(f, "{}", v),
            Pattern::Comparison { operator, bound } => write!(f, "{} {}", operator, bound),
            Pattern::Range {
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
            Pattern::Tuple(patterns) => write!(
                f,
                "({})",
                patterns
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Pattern::Struct {
                type_annotation,
                fields,
            } => {
                if let Some(type_annotation) = type_annotation {
                    write!(f, "{} ", type_annotation)?;
                }

                write_fields(f, fields)
            }
            Pattern::EnumVariant {
                enum_annotation,
                variant,
                fields,
            } => {
                match enum_annotation {
                    Some(enum_annotation) => write!(f, "{}::{}", enum_annotation, variant)?,
                    None => write!(f, "::{}", variant)?,
                }

                if fields.is_empty() {
                    return Ok(());
                }

                write!(f, " ")?;
                write_fields(f, fields)
            }
        }
    }
}

fn write_fields(f: &mut std::fmt::Formatter<'_>, fields: &[FieldPattern]) -> std::fmt::Result {
    write!(
        f,
        "{{ {} }}",
        fields
            .iter()
            .map(|field| field.to_string())
            .collect::<Vec<_>>()
            .join(", ")
    )
}
