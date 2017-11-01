
use lexer::token_stream;
use reader::parse_datum;
use compiler::compile_expression;
use super::vm::{exec, Environment, ExecutionError, GcShared, Value};

#[derive(Debug, PartialEq)]
pub enum InterpreterError {
    Exec(ExecutionError),
    Tokenizer,
    Reader,
    Compiler,
    EOF,
}

pub fn interpret(
    code: &str,
    environment: GcShared<Environment>,
) -> Result<Value, InterpreterError> {
    let tokens = token_stream(code.chars().collect()).map_err(|_| InterpreterError::Tokenizer)?;
    // lolwat
    let mut tokens = tokens.into_iter().collect();


    let mut value = None;

    loop {
        let datum = match parse_datum(&mut tokens) {
            Ok(Some(d)) => d,
            Ok(None) => break,
            _ => return Err(InterpreterError::Reader),
        };

        let bytecode = compile_expression(datum).ok_or(InterpreterError::Compiler)?;
        value = Some(exec(&bytecode, environment.clone())
            .map_err(|e| InterpreterError::Exec(e))?);
    }

    value.ok_or(InterpreterError::EOF)
}

#[cfg(test)]
mod test;
