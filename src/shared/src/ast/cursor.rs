use crate::diagnostic::Diagnostic;
use crate::diagnostic::Span;
use crate::lexer::token::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Cursor {
    tokens: Vec<Token>,
    prev: Token,
    /// The token handed out past the end of the input. Built once, so that it
    /// can carry an empty span *at the end of the file* — an unexpected end of
    /// input is reported there rather than at offset zero.
    end_of_file: Token,
    verbose: bool,
}

impl Cursor {
    pub fn new(mut tokens: Vec<Token>, verbose: bool) -> Cursor {
        let end = tokens.last().map(|token| token.span.end).unwrap_or(0);

        let end_of_file = Token {
            kind: TokenKind::EndOfFile,
            span: Span::new(end, end),
        };

        tokens.reverse();

        Cursor {
            tokens,
            prev: end_of_file.clone(),
            end_of_file,
            verbose,
        }
    }

    pub fn prev(&self) -> Token {
        self.prev.clone()
    }

    /// The span of the next significant token.
    ///
    /// Walks the stream rather than going through [`Cursor::first`], which
    /// clones every remaining token to peek at one.
    pub(crate) fn span(&self) -> Span {
        self.tokens
            .iter()
            .rev()
            .find(|token| {
                !matches!(
                    token.kind,
                    TokenKind::WhiteSpace | TokenKind::LineComment | TokenKind::BlockComment
                )
            })
            .map(|token| token.span)
            .unwrap_or(self.end_of_file.span)
    }

    /// The span of everything parsed since `start`: from there through the last
    /// token consumed. This is how a node gets the span of its whole subtree.
    pub(crate) fn span_from(&self, start: Span) -> Span {
        start.to(self.prev.span)
    }

    pub(crate) fn first(&self) -> Token {
        let mut clone = self.tokens.clone();

        loop {
            let current = clone.pop().unwrap_or(self.end_of_file.clone());

            if matches!(
                current.kind,
                TokenKind::WhiteSpace | TokenKind::LineComment | TokenKind::BlockComment
            ) {
                continue;
            }

            return current;
        }
    }

    pub(crate) fn first_no_skip(&self) -> Token {
        self.tokens.clone().pop().unwrap_or(self.end_of_file.clone())
    }

    pub(crate) fn second(&self) -> Token {
        let mut clone = self.tokens.clone();

        for i in 0..2 {
            loop {
                let current = clone.pop().unwrap_or(self.end_of_file.clone());

                if matches!(
                    current.kind,
                    TokenKind::WhiteSpace | TokenKind::LineComment | TokenKind::BlockComment
                ) {
                    continue;
                }

                if i == 1 {
                    return current;
                }

                break;
            }
        }

        unreachable!()
    }

    pub(crate) fn third(&self) -> Token {
        let mut clone = self.tokens.clone();

        for i in 0..3 {
            loop {
                let current = clone.pop().unwrap_or(self.end_of_file.clone());

                if matches!(
                    current.kind,
                    TokenKind::WhiteSpace | TokenKind::LineComment | TokenKind::BlockComment
                ) {
                    continue;
                }

                if i == 2 {
                    return current;
                }

                break;
            }
        }

        unreachable!()
    }

    pub(crate) fn is_end_of_file(&self) -> bool {
        self.tokens.is_empty() || self.first().kind == TokenKind::EndOfFile
    }

    pub(crate) fn bump(&mut self) -> Result<Token, Diagnostic> {
        loop {
            if matches!(
                self.first_no_skip().kind,
                TokenKind::WhiteSpace | TokenKind::LineComment | TokenKind::BlockComment
            ) {
                if self.verbose {
                    println!("Skipping: {:?}", self.first_no_skip());
                }

                self.tokens.pop();
                continue;
            }

            if self.verbose {
                println!("Bumping: {:?}", self.first_no_skip());
            }

            let token = self
                .tokens
                .pop()
                .ok_or("Unexpected end of file".to_string())?;

            self.prev = token.clone();

            return Ok(token);
        }
    }

    pub(crate) fn optional_bump(&mut self, optional: TokenKind) -> Result<Option<Token>, Diagnostic> {
        loop {
            if matches!(
                self.first_no_skip().kind,
                TokenKind::WhiteSpace | TokenKind::LineComment | TokenKind::BlockComment
            ) {
                if self.verbose {
                    println!("Skipping: {:?}", self.first_no_skip());
                }

                self.tokens.pop();
                continue;
            }

            if self.first_no_skip().kind != optional {
                return Ok(None);
            }

            if self.verbose {
                println!("Bumping: {:?}", self.first_no_skip());
            }

            let token = self
                .tokens
                .pop()
                .ok_or("Unexpected end of file".to_string())?;
            self.prev = token.clone();

            return Ok(Some(token));
        }
    }

    pub(crate) fn expect(&mut self, expected: TokenKind) -> Result<Token, Diagnostic> {
        loop {
            if matches!(
                self.first_no_skip().kind,
                TokenKind::WhiteSpace | TokenKind::LineComment | TokenKind::BlockComment
            ) {
                if self.verbose {
                    println!("Skipping: {:?}", self.first_no_skip());
                }

                self.tokens.pop();
                continue;
            }

            if self.verbose {
                println!("Expecting: {:?}", self.first_no_skip());
            }

            return if self.first_no_skip().kind == expected {
                self.bump()
            } else {
                Err(Diagnostic::error(format!(
                    "Expected {:?}, but found {:?}",
                    expected,
                    self.first_no_skip().kind
                ))
                .at(self.first_no_skip().span))
            };
        }
    }
}
