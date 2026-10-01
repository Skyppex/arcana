//! Compiler diagnostics: what went wrong, and where.
//!
//! The compiler never formats a message for a human. It builds a [`Diagnostic`]
//! — a message, a primary [`Label`] marking the code that failed, any number of
//! secondary labels giving context, and free-standing notes — and the driver
//! renders it once, at the edge, against the [`SourceFile`] it came from.

mod emitter;
mod source_file;
mod span;

pub use emitter::render;
pub use source_file::{Location, SourceFile};
pub use span::Span;

/// A span plus a short message, drawn under the source line it points at.
///
/// The diagnostic's message says *what* went wrong; labels say *where*, and
/// what each place contributes. An empty message means "just point here".
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    pub span: Span,
    pub message: String,
}

impl Label {
    pub fn new(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
}

/// One compiler error.
///
/// `primary` is optional because not everything has a source location: the
/// injected prelude, anything synthesised, and — while spans are still being
/// threaded through — any site that has not been given one yet. A diagnostic
/// with no primary renders as a bare `error: {message}`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub message: String,
    pub primary: Option<Label>,
    pub secondary: Vec<Label>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            primary: None,
            secondary: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Point at `span`, with no label text.
    ///
    /// Only fills an *empty* primary, so an inner site that knew something more
    /// precise keeps it and an outer frame can attach blindly without
    /// clobbering it.
    pub fn at(mut self, span: Span) -> Self {
        if self.primary.is_none() && !span.is_synthetic() {
            self.primary = Some(Label::new(span, ""));
        }

        self
    }

    /// Point at `span` and say something about it. Same no-clobber rule as
    /// [`Diagnostic::at`].
    pub fn labelled(mut self, span: Span, message: impl Into<String>) -> Self {
        match &mut self.primary {
            Some(primary) if primary.message.is_empty() => {
                primary.message = message.into();
            }
            Some(_) => {}
            primary @ None if !span.is_synthetic() => *primary = Some(Label::new(span, message)),
            None => {}
        }

        self
    }

    /// Add a secondary label: context that explains the failure, and which is
    /// itself legal code.
    pub fn and(mut self, span: Span, message: impl Into<String>) -> Self {
        self.secondary.push(Label::new(span, message));
        self
    }

    pub fn note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn span(&self) -> Option<Span> {
        self.primary.as_ref().map(|primary| primary.span)
    }

    pub fn render(&self, file: &SourceFile) -> String {
        render(self, file)
    }
}

/// Lets every existing `Err(format!(...))` keep working unchanged: a diagnostic
/// built this way has no location and renders exactly like the old string did.
impl From<String> for Diagnostic {
    fn from(message: String) -> Self {
        Diagnostic::error(message)
    }
}

impl From<&str> for Diagnostic {
    fn from(message: &str) -> Self {
        Diagnostic::error(message)
    }
}

/// The message alone — never the snippet. Rendering is the emitter's job, and
/// callers that stringify a diagnostic (tests asserting on the text, `{e}` in a
/// driver message) want the sentence, not a block of source.
impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for Diagnostic {}

/// Attaches a location to an error from the outside.
///
/// Helpers that can fail — binary operator typing, joins, scope lookups — have
/// no business knowing about source positions, and threading a `Span` parameter
/// through all of them would touch roughly a hundred signatures. Instead they
/// return a span-less diagnostic and the nearest caller holding an AST node
/// attaches the location:
///
/// ```ignore
/// get_binop_type(operator, &left, &right).at(expression.span)?
/// ```
pub trait Spanned {
    /// Point the error at `span`, if it is not already pointing somewhere.
    fn at(self, span: Span) -> Self;

    /// Point the error at `span` and label it, if it is not already pointing
    /// somewhere.
    fn labelled(self, span: Span, message: impl Into<String>) -> Self;
}

impl<T> Spanned for Result<T, Diagnostic> {
    fn at(self, span: Span) -> Self {
        self.map_err(|diagnostic| diagnostic.at(span))
    }

    fn labelled(self, span: Span, message: impl Into<String>) -> Self {
        self.map_err(|diagnostic| diagnostic.labelled(span, message))
    }
}
