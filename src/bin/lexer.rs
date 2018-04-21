extern crate r5rs;

use r5rs::lexer::*;
use std::io::{stdin, stdout, Write};

fn main() {
    let mut buffer = String::new();
    let mut chars: Vec<_>;
    loop {
        buffer.clear();

        let prompt = stdout().write(b"> ").and(stdout().flush());

        if prompt.is_err() {
            panic!("IO error");
        }

        match stdin().read_line(&mut buffer) {
            Ok(_) => {}
            _ => panic!(""),
        }

        chars = buffer.clone().chars().collect();

        match Tokens::new(chars.into_iter()).collect::<Result<Vec<_>, _>>() {
            Ok(tokens) => println!("{:?}", tokens),
            Err(e) => println!("Invalid input: {:?}", e),
        }
    }
}
