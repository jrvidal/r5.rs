//! Convert tokens into "datums"
use fallible_iterator::{FallibleIterator, Peekable};

mod datum;

use self::datum::parse_datum;
pub use self::datum::{AbbreviationKind, Datum as DatumKind, DatumTree as Datum, ReaderError};
use crate::lexer::{Token, TokenizerError};

pub struct Datums<T: FallibleIterator> {
    tokens: Peekable<T>,
}

impl<T: FallibleIterator> Datums<T> {
  pub fn new(tokens: T) -> Datums<T> {
    Datums { tokens: tokens.peekable() }
  }
}

impl<T> FallibleIterator for Datums<T>
where
    T: FallibleIterator<Item = Token, Error = TokenizerError>
{
    type Item = Datum;
    type Error = ReaderError;

    fn next(&mut self) -> Result<Option<Datum>, ReaderError> {
        parse_datum(&mut self.tokens)
    }
}
