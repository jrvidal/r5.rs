extern crate r5rs;
#[cfg(target_os = "emscripten")]
#[macro_use]
extern crate stdweb;

#[cfg(not(target_os = "emscripten"))]
fn main() {}


#[cfg(target_os = "emscripten")]
fn main() {
    web::main();
}

#[cfg(target_os = "emscripten")]
mod web {
    use stdweb;
    use r5rs::reader::*;
    use r5rs::lexer::*;
    use r5rs::vm::*;
    use r5rs::compiler::*;

    pub fn main() {
        stdweb::initialize();
        let environment = default_env();

        js!{
            Scheme.process(@{move |code: String| -> String { process(code, environment.clone()).unwrap_or("".to_owned()) } })
        }
    }

    fn process(code: String, environment: GcShared<Environment>) -> Option<String> {
        let mut tokens = match Tokens::new(code.chars()).collect() {
            Ok(tokens) => tokens,
            Err(e) => {
                return Some(format!("Invalid input: {:?}", e));
            }
        };


        let mut last = None;
        loop {
            let datum = match parse_datum(&mut tokens) {
                Ok(Some(datum)) => datum,
                Err(e) => break Some(format!("Invalid datum: {:?}", e)),
                Ok(None) => break last,
            };

            let bytecode = match compile_expression(datum) {
                Some(bytecode) => bytecode,
                None => {
                    break Some("Invalid expression".to_owned());
                }
            };

            let result = exec(&bytecode, environment.clone());

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
