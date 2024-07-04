use crate::lexer::token::Token;

#[derive(Debug, Clone)]
pub struct Cursor {
    tokens: Vec<Token>,
    prev: Token,
}

const END_OF_FILE_TOKEN: Token = Token {
    kind: crate::lexer::token::TokenKind::EndOfFile,
    length: 0,
};

impl Cursor {
    pub fn new(mut tokens: Vec<Token>) -> Cursor {
        tokens.reverse();

        Cursor {
            tokens,
            prev: END_OF_FILE_TOKEN,
        }
    }

    pub fn prev(&self) -> Token {
        self.prev.clone()
    }

    pub(crate) fn first(&self) -> Token {
        self.tokens.clone().pop().unwrap_or(END_OF_FILE_TOKEN)
    }

    pub(crate) fn second(&self) -> Token {
        let mut iter = self.tokens.clone();
        iter.pop();
        iter.pop().unwrap_or(END_OF_FILE_TOKEN)
    }

    pub(crate) fn is_end_of_file(&self) -> bool {
        self.tokens.is_empty() || self.first().kind == crate::lexer::token::TokenKind::EndOfFile
    }

    pub(crate) fn bump(&mut self) -> Result<Token, String> {
        // println!("Bumping: {:?}", self.first());
        let token = self
            .tokens
            .pop()
            .ok_or("Unexpected end of file".to_string())?;
        self.prev = token.clone();
        Ok(token)
    }
}
