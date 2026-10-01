use crate::diagnostic::{Diagnostic, Span};
use std::collections::HashMap;
use std::fmt::Display;

use crate::types::TypeAnnotation;

/// A syntactic pattern, as written in source.
///
/// Constructor patterns are split by spelling, not by inference: a pattern
/// containing `::` is always an enum variant and never a struct, and a pattern
/// without `::` is always a struct and never an enum variant. An enum variant
/// *is* a struct, so once the variant is statically known the struct forms
/// apply to it directly.
#[derive(Debug, Clone)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

impl Pattern {
    pub fn new(kind: PatternKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// Where a pattern was written is not part of what it is.
///
/// This matters more here than anywhere else: patterns are compared to find
/// duplicate and unreachable arms, and to deduplicate decision-tree branches.
/// A derived `PartialEq` would make two identical patterns written on different
/// lines compare unequal, and those checks would quietly stop firing.
impl PartialEq for Pattern {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
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
    /// `x @ p` — binds the matched value *and* matches it against `p`.
    Bound {
        identifier: String,
        pattern: Box<Pattern>,
    },
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
    /// `MyEnum::First { x }`, `::First { x }`, `::E2::S3 { x }`, `::E2 e2`.
    ///
    /// The annotation is absent for the unqualified form, where the enum comes
    /// from the matched value. `path` is the chain of variant names below that
    /// enum, outermost first, so a variant of a nested enum is just a longer
    /// path — the first segment is always resolved against the matched enum
    /// itself and never searched for.
    ///
    /// Once the path has narrowed the value, `inner` is what it is matched
    /// against, at the variant's own type: a binding, a binding with `@`, or a
    /// struct pattern over its fields. A variant may bind the value or look
    /// into its fields, never both, so the two cannot appear side by side —
    /// `@` is how you ask for both.
    EnumVariant {
        enum_annotation: Option<TypeAnnotation>,
        path: Vec<String>,
        inner: Option<Box<Pattern>>,
    },
}

impl Pattern {
    /// The path as written, for error messages: `E1::E2::S3` or `::E2::S3`.
    pub fn variant_path_name(enum_annotation: &Option<TypeAnnotation>, path: &[String]) -> String {
        match enum_annotation {
            Some(annotation) => format!("{}::{}", annotation, path.join("::")),
            None => format!("::{}", path.join("::")),
        }
    }
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
        match &self.pattern.kind {
            // `{ x }` rather than `{ x: x }`
            PatternKind::Binding(name) if name == &self.identifier => write!(f, "{}", name),
            _ => write!(f, "{}: {}", self.identifier, self.pattern),
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
        match &self.kind {
            PatternKind::Wildcard | PatternKind::Binding(_) | PatternKind::Unit => true,
            PatternKind::Struct { fields, .. } => fields
                .iter()
                .all(|f| f.pattern.is_unconditionally_irrefutable()),
            PatternKind::Tuple(patterns) => {
                patterns.iter().all(|p| p.is_unconditionally_irrefutable())
            }
            _ => false,
        }
    }

    /// Rejects a pattern that binds one name twice.
    ///
    /// The two would name different values and only the last would survive, so
    /// it is always a mistake rather than a shorthand for equality.
    pub fn check_no_duplicate_bindings(&self) -> Result<(), Diagnostic> {
        let mut seen: HashMap<&str, Span> = HashMap::new();

        for (name, span) in self.bindings_with_spans() {
            if let Some(first) = seen.get(name) {
                return Err(Diagnostic::error(format!(
                    "Pattern `{}` binds `{}` more than once",
                    self, name
                ))
                .labelled(span, "bound again here")
                .and(*first, "first bound here"));
            }

            seen.insert(name, span);
        }

        Ok(())
    }

    /// Every name this pattern binds, in source order.
    pub fn bindings(&self) -> Vec<String> {
        self.bindings_with_spans()
            .into_iter()
            .map(|(name, _)| name.to_owned())
            .collect()
    }

    /// Every name this pattern binds, with where it was bound, in source order.
    fn bindings_with_spans(&self) -> Vec<(&str, Span)> {
        let mut names = vec![];
        self.collect_bindings(&mut names);
        names
    }

    fn collect_bindings<'a>(&'a self, names: &mut Vec<(&'a str, Span)>) {
        match &self.kind {
            PatternKind::Binding(name) => names.push((name, self.span)),
            PatternKind::Tuple(patterns) => {
                for pattern in patterns {
                    pattern.collect_bindings(names);
                }
            }
            PatternKind::Struct { fields, .. } => {
                for field in fields {
                    field.pattern.collect_bindings(names);
                }
            }
            PatternKind::Bound {
                identifier,
                pattern,
            } => {
                names.push((identifier, self.span));
                pattern.collect_bindings(names);
            }
            PatternKind::EnumVariant {
                inner: Some(inner), ..
            } => inner.collect_bindings(names),
            _ => {}
        }
    }
}

impl PatternKind {
    /// Places this pattern at `span`.
    pub fn at(self, span: Span) -> Pattern {
        Pattern { kind: self, span }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            PatternKind::Wildcard => write!(f, "_"),
            PatternKind::Unit => write!(f, "unit"),
            PatternKind::Bool(v) => write!(f, "{}", v),
            PatternKind::Int(v) => write!(f, "{}", v),
            PatternKind::UInt(v) => write!(f, "{}u", v),
            PatternKind::Float(v) => write!(f, "{}f", v),
            PatternKind::Rune(v) => write!(f, "'{}'", v),
            PatternKind::String(v) => write!(f, "\"{}\"", v),
            PatternKind::Binding(v) => write!(f, "{}", v),
            PatternKind::Comparison { operator, bound } => write!(f, "{} {}", operator, bound),
            PatternKind::Range {
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
            PatternKind::Tuple(patterns) => write!(
                f,
                "({})",
                patterns
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            PatternKind::Struct {
                type_annotation,
                fields,
            } => {
                if let Some(type_annotation) = type_annotation {
                    write!(f, "{} ", type_annotation)?;
                }

                write_fields(f, fields)
            }
            PatternKind::Bound {
                identifier,
                pattern,
            } => write!(f, "{} @ {}", identifier, pattern),
            PatternKind::EnumVariant {
                enum_annotation,
                path,
                inner,
            } => {
                write!(f, "{}", Pattern::variant_path_name(enum_annotation, path))?;

                match inner {
                    Some(inner) => write!(f, " {}", inner),
                    None => Ok(()),
                }
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
