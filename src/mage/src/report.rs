use std::fmt::Display;

use shared::diagnostic::{Diagnostic, SourceFile};

/// A diagnostic that has already been turned into text.
///
/// A span is an offset into one particular file, so a diagnostic can only be
/// rendered where that file is known. `Report` is what comes back out of such a
/// step: the compiler's error, already laid out against its source, ready for
/// the driver to print without knowing anything more about it.
pub struct Report(String);

impl Report {
    /// An error that is not about any source at all — a missing file, a
    /// malformed `spell.toml`.
    pub fn fatal(message: impl Into<String>) -> Self {
        Report(format!("error: {}", message.into()))
    }

    /// Renders diagnostics against `file`.
    ///
    /// Returns a closure so it reads naturally at the call site that knows the
    /// source: `tokenize(&file.source).map_err(Report::against(file))?`.
    pub fn against(file: &SourceFile) -> impl Fn(Diagnostic) -> Report + '_ {
        move |diagnostic| Report(diagnostic.render(file))
    }
}

/// A diagnostic that escapes without ever meeting its source still has to
/// print. It renders as the bare message, which is what every error in the
/// compiler looked like before spans existed.
impl From<Diagnostic> for Report {
    fn from(diagnostic: Diagnostic) -> Self {
        Report(format!("error: {diagnostic}"))
    }
}

impl Display for Report {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
