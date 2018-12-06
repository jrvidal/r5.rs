use crate::compiler::compile_expression;
use crate::lexer::Tokens;
use crate::reader::parse_datum;
use crate::vm::{exec, Environment, ExecutionError, GcShared, NoopProfiler, Value};

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
    let mut tokens = Tokens::new(code.chars())
        .collect::<Result<_, _>>()
        .map_err(|_| InterpreterError::Tokenizer)?;

    let mut value = None;

    loop {
        let datum = match parse_datum(&mut tokens) {
            Ok(Some(d)) => d,
            Ok(None) => break,
            _ => return Err(InterpreterError::Reader),
        };

        let bytecode = compile_expression(datum).ok_or(InterpreterError::Compiler)?;
        value = Some(exec(&bytecode, environment.clone(), &mut NoopProfiler)
            .map_err(|e| InterpreterError::Exec(e))?);
    }

    value.ok_or(InterpreterError::EOF)
}

#[cfg(test)]
mod test;
