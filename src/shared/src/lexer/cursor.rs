use std::str::Chars;

#[derive(Debug, Clone)]
pub struct Cursor<'a> {
    chars: Chars<'a>,
    length_remaining: usize,
}

pub(crate) const END_OF_FILE_CHAR: char = '\0';

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            length_remaining: input.len(),
            chars: input.chars(),
        }
    }

    pub fn as_str(&self) -> &'a str {
        self.chars.as_str()
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

    pub(crate) fn position_within_token(&self) -> u32 {
        (self.length_remaining - self.chars.as_str().len()) as u32
    }

    pub(crate) fn reset_position_within_token(&mut self) {
        self.length_remaining = self.chars.as_str().len();
    }

    pub(crate) fn bump(&mut self) -> Option<char> {
        Some(self.chars.next()?)
    }

    pub(crate) fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.first()) && !self.is_end_of_file() {
            self.bump();
        }
    }
}