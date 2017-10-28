

use super::{shared, ExecutionError, Scalar, Value};

pub(super) fn list(mut values: Vec<Value>) -> Result<Value, ExecutionError> {
    if values.len() == 0 {
        return Ok(Value::Scalar(Scalar::EmptyList));
    }

    let mut pair = Value::Pair {
        car: shared(values.pop().unwrap()),
        cdr: shared(Value::Scalar(Scalar::EmptyList)),
    };

    while let Some(val) = values.pop() {
        pair = Value::Pair {
            car: shared(val),
            cdr: shared(pair),
        }
    }

    Ok(pair)
}

pub(super) fn cons(mut values: Vec<Value>) -> Result<Value, ExecutionError> {
    if values.len() != 2 {
        return Err(ExecutionError::BadArgc);
    }
    let cdr = values.pop().unwrap();
    Ok(Value::Pair {
        car: shared(values.pop().unwrap()),
        cdr: shared(cdr),
    })
}
