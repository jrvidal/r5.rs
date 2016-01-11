extern crate r5rs;

use std::io::{stdin, stdout, Write};
use r5rs::lexer::*;
use r5rs::reader::*;

fn main() {
    let mut buffer = String::new();
    let mut chars;

    loop {
        buffer.clear();

        let prompt = stdout()
            .write("> ".as_bytes())
            .and(stdout().flush());

        if let Err(_) = prompt {
            panic!("IO error");
        }

        match stdin().read_line(&mut buffer) {
            Ok(_) => {},
            _ => panic!("")
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

        match parse_datum(&mut tokens) {
            Ok(Some(datum)) => println!("{:?}", datum),
            Err(e) => println!("Invalid datum: {:?}", e),
            Ok(None) => {}
        }
    }
}