//! Convert text into tokens

#[macro_use]
mod macros;

mod chars;
mod number;
mod token;

#[cfg(test)]
mod token_test;

pub use self::number::{Num, NumberToken};
use self::token::TokenErrorClass;
pub use self::token::{Token, Tokens};
