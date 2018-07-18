use fallible_iterator::FallibleIterator;

use std::io::{stdin, stdout, Write};

use r5rs::compiler::*;
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

        let datum = match datums.next() {
            Ok(None) => continue,
            Err(e) => {
                println!("Invalid datum {:?}", e);
                continue;
            },
            Ok(Some(d)) => d
        };

        println!("{:?}", compile_expression(datum));
    }
}
