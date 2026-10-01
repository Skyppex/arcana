/// A byte range into a single source file.
///
/// Offsets, not line and column: they are cheap, exact, and joinable, which is
/// what lets a node take its span from its first token through its last. Line
/// and column are computed only when something is about to be printed, by
/// [`SourceFile::location`](super::SourceFile::location).
///
/// Offsets are *file relative*. There is no global interned address space, so a
/// span means nothing without knowing which file produced it; the driver knows,
/// because it compiles one file at a time.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    /// The span of something that was never written: a node the compiler
    /// synthesised, or one recovered from an environment rather than from
    /// source.
    ///
    /// [`Diagnostic::at`](super::Diagnostic::at) ignores it, so such a node
    /// never pulls a caret to the start of the file — the error keeps looking
    /// outwards for a place that was actually written.
    pub const SYNTHETIC: Span = Span::new(0, 0);

    pub const fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub fn is_synthetic(self) -> bool {
        self == Span::SYNTHETIC
    }

    /// The span covering both, and everything between them.
    ///
    /// Takes the outermost bounds rather than `self.start..other.end`, so
    /// joining is order independent — a parser that collects sub-spans out of
    /// source order still gets a sane answer.
    pub fn to(self, other: Span) -> Span {
        Span::new(self.start.min(other.start), self.end.max(other.end))
    }

    pub fn len(self) -> u32 {
        self.end.saturating_sub(self.start)
    }

    pub fn is_empty(self) -> bool {
        self.len() == 0
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}
