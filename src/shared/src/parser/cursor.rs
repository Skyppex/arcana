use crate::lexer::token::Token;

#[derive(Debug, Clone)]
pub struct Cursor {
    tokens: Vec<Token>,
    length_remaining: usize,
}

const END_OF_FILE_CHAR: Token = Token {
    kind: crate::lexer::token::TokenKind::EndOfFile,
    length: 0,
};

impl Cursor {
    pub fn new(mut tokens: Vec<Token>) -> Cursor {
        tokens.reverse();

        Cursor {
            length_remaining: tokens.len(),
            tokens,
        }
    }

    pub(crate) fn first(&self) -> Token {
        self.tokens.clone().pop().unwrap_or(END_OF_FILE_CHAR)
    }

    pub(crate) fn second(&self) -> Token {
        let mut iter = self.tokens.clone();
        iter.pop();
        iter.pop().unwrap_or(END_OF_FILE_CHAR)
    }

    pub(crate) fn is_end_of_file(&self) -> bool {
        self.tokens.is_empty()
    }

    pub(crate) fn position_within_token(&self) -> u32 {
        (self.length_remaining - self.tokens.len()) as u32
    }

    pub(crate) fn reset_position_within_token(&mut self) {
        self.length_remaining = self.tokens.len();
    }

    pub(crate) fn bump(&mut self) -> Option<Token> {
        Some(self.tokens.pop()?)
    }

    pub(crate) fn eat_while(&mut self, mut predicate: impl FnMut(Token) -> bool) {
        while predicate(self.first()) && !self.is_end_of_file() {
            self.bump();
        }
    }
}