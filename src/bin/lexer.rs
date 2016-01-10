extern crate r5rs;

use std::io::{stdin, stdout, Write};
use r5rs::lexer::*;

fn main() {
    let mut buffer = String::new();
    let mut chars;
    loop {
        stdout().write("> ".as_bytes());
        stdout().flush();

        match stdin().read_line(&mut buffer) {
            Ok(n) => {},
            _ => panic!("")
        }

        chars = buffer.clone().chars().collect();

        match token_stream(chars) {
            Ok(tokens) => println!("{:?}", tokens),
            Err(e) => println!("Invalid input: {:?}", e)
        }

        buffer.clear();
    }
}