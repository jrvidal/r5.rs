extern crate r5rs;

use std::io::{stdin, stdout, Write};

// use r5rs::values::*;
// use r5rs::parser::*;
use r5rs::reader::*;
use r5rs::lexer::*;
use r5rs::compiler::*;

fn main() {
    let mut buffer = String::new();
    let mut chars;
    // let mut environment = Environment::new(None);
    // let mut heap = Heap::new();

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

        // // println!("datum: {:?}", datum);
        // let expression = match parse_expression(datum) {
        //     Ok(exp) => exp,
        //     Err(_) => {
        //         println!("Invalid expression");
        //         continue;
        //     }
        // };

        println!("{:?}", compile_expression(datum));

    }
}
