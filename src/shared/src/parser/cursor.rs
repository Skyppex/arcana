use crate::lexer::token::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Cursor {
    tokens: Vec<Token>,
    prev: Token,
    verbose: bool,
}

const END_OF_FILE_TOKEN: Token = Token {
    kind: crate::lexer::token::TokenKind::EndOfFile,
    length: 0,
};

impl Cursor {
    pub fn new(mut tokens: Vec<Token>, verbose: bool) -> Cursor {
        tokens.reverse();

        Cursor {
            tokens,
            prev: END_OF_FILE_TOKEN,
            verbose,
        }
    }

    pub fn prev(&self) -> Token {
        self.prev.clone()
    }

    pub(crate) fn first(&self) -> Token {
        let mut clone = self.tokens.clone();

        loop {
            let current = clone.pop().unwrap_or(END_OF_FILE_TOKEN);

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
        self.tokens.clone().pop().unwrap_or(END_OF_FILE_TOKEN)
    }

    pub(crate) fn second(&self) -> Token {
        let mut clone = self.tokens.clone();

        loop {
            let current = clone.pop().unwrap_or(END_OF_FILE_TOKEN);

            if matches!(
                current.kind,
                TokenKind::WhiteSpace | TokenKind::LineComment | TokenKind::BlockComment
            ) {
                continue;
            }

            break;
        }

        loop {
            let current = clone.pop().unwrap_or(END_OF_FILE_TOKEN);

            if matches!(
                current.kind,
                TokenKind::WhiteSpace | TokenKind::LineComment | TokenKind::BlockComment
            ) {
                continue;
            }

            return current;
        }
    }

    pub(crate) fn is_end_of_file(&self) -> bool {
        self.tokens.is_empty() || self.first().kind == crate::lexer::token::TokenKind::EndOfFile
    }

    pub(crate) fn bump(&mut self) -> Result<Token, String> {
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

    // pub(crate) fn optional_bump(&mut self, optional: TokenKind) -> Result<Option<Token>, String> {
    //     loop {
    //         if matches!(
    //             self.first_no_skip().kind,
    //             TokenKind::WhiteSpace | TokenKind::LineComment | TokenKind::BlockComment
    //         ) {
    //             if self.verbose {
    //                 println!("Skipping: {:?}", self.first_no_skip());
    //             }
    //
    //             self.bump()?;
    //             continue;
    //         }
    //
    //         if self.first_no_skip().kind != optional {
    //             return Ok(None);
    //         }
    //
    //         if self.verbose {
    //             println!("Bumping: {:?}", self.first_no_skip());
    //         }
    //
    //         let token = self
    //             .tokens
    //             .pop()
    //             .ok_or("Unexpected end of file".to_string())?;
    //         self.prev = token.clone();
    //
    //         return Ok(Some(token));
    //     }
    // }

    pub(crate) fn expect(&mut self, expected: TokenKind) -> Result<Token, String> {
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
                Err(format!(
                    "Expected {:?}, but found {:?}",
                    expected,
                    self.first_no_skip().kind
                ))
            };
        }
    }
}
