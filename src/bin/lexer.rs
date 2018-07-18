use std::io::{stdin, stdout, Write};

use fallible_iterator::FallibleIterator;

use r5rs::lexer::*;

fn main() {
    let mut buffer = String::new();

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

        match Tokens::new(buffer.chars()).collect::<Vec<_>>() {
            Ok(tokens) => println!("{:?}", tokens),
            Err(e) => println!("Invalid input: {:?}", e),
        }
    }
}
