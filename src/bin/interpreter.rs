extern crate gc;
extern crate r5rs;

use std::io::{stdin, stdout, Write};

use r5rs::reader::*;
use r5rs::lexer::*;
use r5rs::interpreter::*;
use r5rs::compiler::*;

fn main() {
    let mut buffer = String::new();
    let mut chars;
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

        let tokens = match token_stream(chars) {
            Ok(tokens) => tokens,
            Err(e) => {
                println!("Invalid input: {:?}", e);
                continue;
            }
        };

        let mut tokens = tokens.into_iter().collect();

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

        println!("Code:\n{:?}", bytecode);
        let result = exec(bytecode, environment.clone());

        match result {
            Ok(v) => println!("{}", v.to_repl()),
            Err(_) => println!("execution error"),
        };
    }
}
