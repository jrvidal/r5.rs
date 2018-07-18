//! Convert tokens into "datums"
use fallible_iterator::{FallibleIterator, Peekable as FalliblePeekable};
use std::iter::Peekable;
use std::error::Error;

mod datum;

use lexer2::Token;
pub use self::datum::{AbbreviationKind, Datum, ReaderError};
use self::datum::parse_datum;

struct Datums<T: FallibleIterator> {
    tokens: FalliblePeekable<T>,
}

impl<T> FallibleIterator for Datums<T>
where
    T: FallibleIterator<Item = Token>,
    T::Error: Error,
{
    type Item = Datum;
    type Error = ReaderError;

    fn next(&mut self) -> Result<Option<Datum>, ReaderError> {
        unimplemented!()
    }
}
