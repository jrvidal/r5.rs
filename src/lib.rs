//! A (hopefully) R5RS compliant Scheme interpreter
extern crate env_logger;
#[macro_use]
extern crate gc;
#[macro_use]
extern crate log;
extern crate fallible_iterator;

#[macro_use]
mod helpers;

#[cfg(test)]
mod interpreter;
pub mod lexer;
pub mod reader;
pub mod compiler;
pub mod vm;
