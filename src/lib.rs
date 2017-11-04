//! A (hopefully) R5RS compliant Scheme interpreter
extern crate env_logger;
#[macro_use]
extern crate gc;
#[macro_use]
extern crate log;

#[macro_use]
mod helpers;

pub mod lexer;
pub mod reader;
pub mod compiler;
pub mod vm;
#[cfg(test)]
mod interpreter;
