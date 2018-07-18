use std::io::{stdin, stdout, Write};

use fallible_iterator::FallibleIterator;

use r5rs::lexer::*;
use r5rs::reader::*;

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

        let tokens = Tokens::new(buffer.chars());
        let mut datums = Datums::new(tokens);


        loop {
            match datums.next() {
                Ok(None) => break,
                Err(e) => {
                    eprintln!("Invalid input: {:?}", e);
                    break;
                },
                Ok(Some(datum)) => println!("{:?}", datum)
            }
        }
    }
}
