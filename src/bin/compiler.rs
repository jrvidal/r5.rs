extern crate r5rs;

use std::io::{stdin, stdout, Write};

use r5rs::reader::*;
use r5rs::lexer::*;
use r5rs::compiler::*;

fn main() {
    let mut buffer = String::new();
    let mut chars: Vec<_>;

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

        println!("{:?}", compile_expression(datum));
    }
}
