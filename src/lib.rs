//! A (hopefully) R5RS compliant Scheme interpreter
#[macro_use]
extern crate gc;
#[macro_use]
extern crate log;
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
extern crate wasm_bindgen;

#[macro_use]
mod helpers;

pub mod compiler;
#[cfg(test)]
mod interpreter;
pub mod lexer;
pub mod reader;
pub mod vm;

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
mod web;
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub use web::*;