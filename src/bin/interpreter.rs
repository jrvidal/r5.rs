extern crate env_logger;
#[cfg(not(target_os = "emscripten"))]
#[macro_use]
extern crate log;
extern crate r5rs;
#[cfg(not(target_os = "emscripten"))]
extern crate rustyline;

#[cfg(target_os = "emscripten")]
fn main() {}

#[cfg(not(target_os = "emscripten"))]
fn main() {
    not_web::main();
}

#[cfg(not(target_os = "emscripten"))]
mod not_web {

    use env_logger;
    use r5rs::reader::*;
    use r5rs::lexer::*;
    use r5rs::vm::*;
    use r5rs::compiler::*;

    use rustyline;
    use rustyline::error::ReadlineError;

    pub fn main() {
        env_logger::init().ok().expect("logger");
        let mut rl = rustyline::Editor::<()>::new();
        let environment = default_env();

        loop {
            let readline = rl.readline("> ");
            let line = match readline {
                Ok(line) => line,
                Err(ReadlineError::Eof) => break,
                Err(_) => continue,
            };

            rl.add_history_entry(&line);

            let mut tokens = match Tokens::new(line.chars()).collect() {
                Ok(tokens) => tokens,
                Err(e) => {
                    println!("Invalid input: {:?}", e);
                    continue;
                }
            };

            loop {
                let datum = match parse_datum(&mut tokens) {
                    Ok(Some(datum)) => datum,
                    Err(e) => {
                        println!("Invalid datum: {:?}", e);
                        break;
                    }
                    Ok(None) => {
                        break;
                    }
                };

                let bytecode = match compile_expression(datum) {
                    Some(bytecode) => bytecode,
                    None => {
                        println!("Invalid expression");
                        break;
                    }
                };

                debug!("Code:\n{:?}", bytecode);
                let result = exec(&bytecode, environment.clone());

                match result {
                    Ok(v) => println!("{}", v.to_repl()),
                    Err(e) => {
                        println!("Error: {:?}", e);
                        break;
                    }
                };
            }
        }
    }

}
