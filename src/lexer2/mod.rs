//! Convert text into tokens
use fallible_iterator::FallibleIterator;

#[macro_use]
mod macros;

mod chars;
mod number;
mod token;

#[cfg(test)]
mod token_test;

pub use self::number::{Num, NumberToken};
use self::token::{TokenErrorClass, next_token};
pub use self::token::{TokenizerError, Token};
use self::chars::Chars;

/// A stream of tokens
pub struct Tokens<I: Iterator<Item = char>> {
    source: Chars<I>,
}

impl<I: Iterator<Item = char>> FallibleIterator for Tokens<I> {
    type Item = Token;
    type Error = TokenizerError;

    fn next(&mut self) -> Result<Option<Token>, TokenizerError> {
        next_token(&mut self.source)
    }
}

impl<I: Iterator<Item = char>> Tokens<I> {
    pub fn new(source: I) -> Tokens<I> {
        Tokens {
            source: source.into(),
        }
    }
}
