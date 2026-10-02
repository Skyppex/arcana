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
/// print. It renders as the message and its notes, which is what every error in
/// the compiler looked like before spans existed.
///
/// The notes are not optional here: an error about the core library being
/// missing says *what* is wrong in the message and *which path was tried, and
/// why* in the notes, and the second half is the useful half.
impl From<Diagnostic> for Report {
    fn from(diagnostic: Diagnostic) -> Self {
        let mut out = format!("error: {diagnostic}");

        for note in &diagnostic.notes {
            out.push_str(&format!("\nnote: {note}"));
        }

        Report(out)
    }
}

/// Prints as the report itself, not as `Report("…")`.
///
/// A `Report` only ever shows up in `Debug` when something unwrapped it — a
/// test, or a panic — and that is exactly when the rendered error with its
/// snippet is what someone needs to read.
impl std::fmt::Debug for Report {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for Report {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
