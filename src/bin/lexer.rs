extern crate r5rs;

use std::io::{stdin, stdout, Write};
use r5rs::lexer::*;

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

        match Tokens::new(chars.into_iter()).collect::<Result<Vec<_>, _>>() {
            Ok(tokens) => println!("{:?}", tokens),
            Err(e) => println!("Invalid input: {:?}", e),
        }
    }
}
