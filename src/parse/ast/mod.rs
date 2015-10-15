#[cfg(test)]
use super::token::{Token};
#[cfg(test)]
use std::collections::VecDeque;

pub mod datum;
pub mod expression;

#[cfg(test)]
pub fn tokens(t: &[Token]) -> VecDeque<Token> {
    let mut stream = VecDeque::new();
    for token in t.iter().cloned() {
        stream.push_back(token);
    }
    stream
}
