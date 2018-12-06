//! A (hopefully) R5RS compliant Scheme interpreter
#[macro_use]
extern crate gc;
#[macro_use]
extern crate log;

#[macro_use]
mod helpers;

pub mod compiler;
#[cfg(test)]
mod interpreter;
pub mod lexer;
pub mod reader;
pub mod vm;
