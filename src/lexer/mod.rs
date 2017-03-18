
#[macro_use]
mod macros;

mod chars;
mod token;
mod number;

#[cfg(test)]
mod token_test;


use self::token::TokenErrorClass;
pub use self::token::{Token, token_stream};
pub use self::number::NumberToken;
