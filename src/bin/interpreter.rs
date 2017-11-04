extern crate env_logger;
#[macro_use]
extern crate log;
extern crate r5rs;


use std::io::{stdin, stdout, Write};

use r5rs::reader::*;
use r5rs::lexer::*;
use r5rs::vm::*;
use r5rs::compiler::*;

fn main() {
    env_logger::init().ok().expect("logger");
    let mut buffer = String::new();
    let mut chars: Vec<_>;
    let environment = default_env();

    loop {
        buffer.clear();

        let prompt = stdout().write("> ".as_bytes()).and(stdout().flush());

        if let Err(_) = prompt {
            panic!("IO error");
        }

        match stdin().read_line(&mut buffer) {
            Ok(_) => {}
            _ => panic!(""),
        }

        chars = buffer.clone().chars().collect();

        let mut tokens = match Tokens::new(chars.into_iter()).collect() {
            Ok(tokens) => tokens,
            Err(e) => {
                println!("Invalid input: {:?}", e);
                continue;
            }
        };

        let datum = match parse_datum(&mut tokens) {
            Ok(Some(datum)) => datum,
            Err(e) => {
                println!("Invalid datum: {:?}", e);
                continue;
            }
            Ok(None) => {
                continue;
            }
        };


        let bytecode = match compile_expression(datum) {
            Some(bytecode) => bytecode,
            None => {
                println!("Invalid expression");
                continue;
            }
        };

        debug!("Code:\n{:?}", bytecode);
        let result = exec(&bytecode, environment.clone());

        match result {
            Ok(v) => println!("{}", v.to_repl()),
            Err(e) => println!("Error: {:?}", e),
        };
    }
}
