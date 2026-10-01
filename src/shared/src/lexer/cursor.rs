use std::str::Chars;

use crate::diagnostic::Span;

#[derive(Debug, Clone)]
pub struct Cursor<'a> {
    chars: Chars<'a>,
    length_remaining: usize,
    /// The length of the whole input, which is what turns the remaining length
    /// into an absolute offset.
    source_length: u32,
}

pub(crate) const END_OF_FILE_CHAR: char = '\0';

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            length_remaining: input.len(),
            source_length: input.len() as u32,
            chars: input.chars(),
        }
    }

    /// The span of the token being lexed: from the last
    /// [`reset_position_within_token`](Cursor::reset_position_within_token) to
    /// wherever the cursor has reached.
    pub(crate) fn token_span(&self) -> Span {
        Span::new(
            self.source_length - self.length_remaining as u32,
            self.source_length - self.chars.as_str().len() as u32,
        )
    }

    pub(crate) fn first(&self) -> char {
        self.chars.clone().next().unwrap_or(END_OF_FILE_CHAR)
    }

    pub(crate) fn second(&self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or(END_OF_FILE_CHAR)
    }

    pub(crate) fn is_end_of_file(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    pub(crate) fn reset_position_within_token(&mut self) {
        self.length_remaining = self.chars.as_str().len();
    }

    pub(crate) fn bump(&mut self) -> Option<char> {
        self.chars.next()
    }

    pub(crate) fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.first()) && !self.is_end_of_file() {
            self.bump();
        }
    }
}
