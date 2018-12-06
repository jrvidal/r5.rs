extern crate r5rs;
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
extern crate wasm_bindgen;

/*
mod web;
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub use web::*;
*/

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub use web::*;

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
mod web {
  use wasm_bindgen::prelude::*;

  use r5rs::*;

  #[wasm_bindgen]
  pub struct Environment(vm::GcShared<vm::Environment>);

  #[wasm_bindgen]
  impl Environment {
      #[wasm_bindgen(constructor)]
      pub fn new() -> Environment {
          Environment(vm::default_env())
      }

      pub fn process(&self, code: String) -> Option<String> {
          let mut tokens = match lexer::Tokens::new(code.chars()).collect() {
              Ok(tokens) => tokens,
              Err(e) => {
                  return Some(format!("Invalid input: {:?}", e));
              }
          };

          let mut last = None;
          loop {
              let datum = match reader::parse_datum(&mut tokens) {
                  Ok(Some(datum)) => datum,
                  Err(e) => break Some(format!("Invalid datum: {:?}", e)),
                  Ok(None) => break last,
              };

              let bytecode = match compiler::compile_expression(datum) {
                  Some(bytecode) => bytecode,
                  None => {
                      break Some("Invalid expression".to_owned());
                  }
              };

              let result = vm::exec(&bytecode, self.0.clone(), &mut vm::NoopProfiler);

              match result {
                  Ok(v) => {
                      last = Some(v.to_repl());
                  }
                  Err(e) => {
                      break Some(format!("Error: {:?}", e));
                  }
              };
          }
      }
  }
}
