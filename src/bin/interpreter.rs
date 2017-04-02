extern crate r5rs;
extern crate gc;

use gc::Trace;
use std::io::{stdin, stdout, Write};

// use r5rs::values::*;
use r5rs::parser::*;
use r5rs::reader::*;
use r5rs::lexer::*;
use r5rs::interpret::*;
use r5rs::compiler::*;

fn main() {
    let mut buffer = String::new();
    let mut chars;
    let mut environment = null_env();
    // let mut heap = Heap::new();
    // let env_gc = heap.insert_env(environment);

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

        // println!("datum: {:?}", datum);
        let expression = match parse_expression(datum) {
            Ok(exp) => exp,
            Err(_) => {
                println!("Invalid expression");
                continue;
            }
        };


        let (bytecode, entry) = compile(expression);

        println!("Code:\n{:?}", bytecode);
        let result = exec(bytecode, environment.clone(), entry);

        match result {
            Ok(Value::Scalar(ref s)) => println!("{:?}", s),
            x @ _ => println!("{:?}", x),
            // Ok(Value::Reference(ref r)) => if let Ok(ref v) = r.borrow() {
            //     let ref_value : &Reference = &*v;
            //     println!("{:?}", ref_value);
            // } else {
            //     println!("heap error");
            // },
            Err(_) => println!("execution error"),
        };

        // println!("{:?}", expression);

    //     let object = match eval(&expression, &mut environment, &mut heap) {
    //         Ok(obj) => obj,
    //         Err(e) => {
    //             println!("{:?}", e);
    //             continue;
    //         }
    //     };

    //     println!("{}", object.to_repl());
    }
}
