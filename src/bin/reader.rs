extern crate r5rs;

use std::io::{stdin, stdout, Write};
use std::collections::VecDeque;
use r5rs::lexer::*;
use r5rs::reader::*;

fn main() {
    let mut buffer = String::new();
    let mut chars;

    loop {
        buffer.clear();

        stdout().write("> ".as_bytes());
        stdout().flush();

        match stdin().read_line(&mut buffer) {
            Ok(n) => {},
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