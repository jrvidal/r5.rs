use super::{shared, Branch, DeepEqual, ExecutionError, Pair, Value, VmState};

type NatFn = fn(&mut VmState, CallInfo, &Option<Branch>) -> Result<(), ExecutionError>;
type CallInfo = (usize, bool);

pub(super) const STDLIB: [(&str, NatFn, CallInfo); 22] = [
    ("list", list, (0, true)),
    ("cons", cons, (2, false)),
    ("apply", apply, (2, false)),
    ("car", car, (1, false)),
    ("cdr", cdr, (1, false)),
    ("force", force, (1, false)),
    ("eqv?", are_eqv, (2, false)),
    ("equal?", are_equal, (2, false)),
    ("+", addition, (0, true)),
    ("*", multiplication, (0, true)),
    ("-", substraction, (0, true)),
    ("<=", leq_than, (0, true)),
    ("null?", is_null, (1, false)),
    ("number?", is_number, (1, false)),
    ("list?", is_list, (1, false)),
    ("pair?", is_pair, (1, false)),
    ("boolean?", is_boolean, (1, false)),
    ("symbol?", is_symbol, (1, false)),
    ("char?", is_char, (1, false)),
    ("string?", is_string, (1, false)),
    ("vector?", is_vector, (1, false)),
    ("procedure?", is_procedure, (1, false)),
];

// By the time a native procedure is called:
// * The arity has been checked to be correct.
// * The arguments are in inverse order in the stack: arg_n, ... arg_1

fn list(
    vm: &mut VmState,
    call_info: (usize, bool),
    _: &Option<Branch>,
) -> Result<(), ExecutionError> {
    let list = vm.pop_as_list(call_info.0, false).expect("Bad argc");
    vm.stack.push(list);
    Ok(())
}

fn cons(vm: &mut VmState, _: (usize, bool), _: &Option<Branch>) -> Result<(), ExecutionError> {
    let cdr = vm.stack.pop().expect("bad cdr");
    let car = vm.stack.pop().expect("bad car");
    vm.stack.push(Value::Pair(shared(Pair(car, cdr))));
    Ok(())
}

fn apply(
    vm: &mut VmState,
    call_info: (usize, bool),
    branch: &Option<Branch>,
) -> Result<(), ExecutionError> {
    let arg_list = vm.stack.pop().unwrap();
    if !arg_list.is_list() {
        return Err(ExecutionError::BadArgType);
    }
    let args_passed = arg_list.list_len().unwrap();

    let procedure = vm.stack.pop().unwrap();

    match procedure {
        Value::Procedure { .. } | Value::NativeProcedure { .. } => {}
        _ => return Err(ExecutionError::NonCallable),
    }

    vm.push_list(arg_list);
    vm.stack.push(procedure);
    vm.call(call_info.1, args_passed, branch)
}

fn car(
    vm: &mut VmState,
    _call_info: (usize, bool),
    _: &Option<Branch>,
) -> Result<(), ExecutionError> {
    let pair = vm.stack
        .pop()
        .unwrap()
        .pair()
        .ok_or(ExecutionError::BadArgType)?;
    vm.stack.push(pair.borrow().0.clone());
    Ok(())
}

fn cdr(
    vm: &mut VmState,
    _call_info: (usize, bool),
    _: &Option<Branch>,
) -> Result<(), ExecutionError> {
    let pair = vm.stack
        .pop()
        .unwrap()
        .pair()
        .ok_or(ExecutionError::BadArgType)?;
    vm.stack.push(pair.borrow().1.clone());
    Ok(())
}

fn force(
    vm: &mut VmState,
    call_info: (usize, bool),
    branch: &Option<Branch>,
) -> Result<(), ExecutionError> {
    if let Value::Promise { .. } = *vm.stack.get(0).unwrap() {
        // Pass
    } else {
        return Ok(());
    }

    let (code, environment) = if let Value::Promise { code, environment } = vm.stack.pop().unwrap()
    {
        (code, environment)
    } else {
        unreachable!();
    };

    vm.non_native_call(call_info.1, environment, code, branch);

    Ok(())
}

fn are_eqv(
    vm: &mut VmState,
    _call_info: (usize, bool),
    _: &Option<Branch>,
) -> Result<(), ExecutionError> {
    let x = vm.stack.pop().unwrap();
    let y = vm.stack.pop().unwrap();

    vm.stack.push(Value::Boolean(x == y));

    Ok(())
}

fn are_equal(
    vm: &mut VmState,
    _call_info: (usize, bool),
    _: &Option<Branch>,
) -> Result<(), ExecutionError> {
    let x = vm.stack.pop().unwrap();
    let y = vm.stack.pop().unwrap();

    vm.stack.push(Value::Boolean(x.equal(&y)));

    Ok(())
}

fn addition(
    vm: &mut VmState,
    (n_of_args, _): (usize, bool),
    _: &Option<Branch>,
) -> Result<(), ExecutionError> {
    if n_of_args == 0 {
        vm.stack.push(Value::Integer(0));
        return Ok(());
    }

    let mut acc = vm.stack.pop().unwrap();
    if !acc.is_number() {
        return Err(ExecutionError::BadArgType);
    }

    let numbers = vm.pop_as_vec(n_of_args - 1).unwrap();

    for n in numbers {
        acc = match (acc, n) {
            (Value::Integer(n), Value::Integer(m)) => Value::Integer(m + n),
            (Value::Integer(n), Value::Float(f)) | (Value::Float(f), Value::Integer(n)) => {
                Value::Float((n as f32) + f)
            }
            (Value::Float(f), Value::Float(g)) => Value::Float(f + g),
            _ => return Err(ExecutionError::BadArgType),
        }
    }

    vm.stack.push(acc);
    Ok(())
}

fn multiplication(
    vm: &mut VmState,
    (n_of_args, _): (usize, bool),
    _: &Option<Branch>,
) -> Result<(), ExecutionError> {
    if n_of_args == 0 {
        vm.stack.push(Value::Integer(1));
        return Ok(());
    }

    let mut acc = vm.stack.pop().unwrap();
    if !acc.is_number() {
        return Err(ExecutionError::BadArgType);
    }

    let numbers = vm.pop_as_vec(n_of_args - 1).unwrap();

    for n in numbers {
        acc = match (acc, n) {
            (Value::Integer(n), Value::Integer(m)) => Value::Integer(m * n),
            (Value::Integer(n), Value::Float(f)) | (Value::Float(f), Value::Integer(n)) => {
                Value::Float((n as f32) * f)
            }
            (Value::Float(f), Value::Float(g)) => Value::Float(f * g),
            _ => return Err(ExecutionError::BadArgType),
        }
    }

    vm.stack.push(acc);
    Ok(())
}

fn substraction(
    vm: &mut VmState,
    (n_of_args, _): (usize, bool),
    _: &Option<Branch>,
) -> Result<(), ExecutionError> {
    if n_of_args == 0 {
        vm.stack.push(Value::Integer(0));
        return Ok(());
    }

    let (mut acc, numbers) = if n_of_args == 1 {
        (Value::Integer(0), vm.pop_as_vec(1).unwrap())
    } else {
        let numbers = vm.pop_as_vec(n_of_args - 1).unwrap();
        (vm.stack.pop().unwrap(), numbers)
    };

    if !acc.is_number() {
        return Err(ExecutionError::BadArgType);
    }

    for n in numbers {
        acc = match (acc, n) {
            (Value::Integer(n), Value::Integer(m)) => Value::Integer(n - m),
            (Value::Integer(n), Value::Float(f)) => Value::Float((n as f32) - f),
            (Value::Float(f), Value::Integer(n)) => Value::Float(f - (n as f32)),
            (Value::Float(f), Value::Float(g)) => Value::Float(f - g),
            _ => return Err(ExecutionError::BadArgType),
        }
    }

    vm.stack.push(acc);
    Ok(())
}

fn leq_than(
    vm: &mut VmState,
    (n_of_args, _): (usize, bool),
    _: &Option<Branch>,
) -> Result<(), ExecutionError> {
    if n_of_args == 0 {
        vm.stack.push(Value::Boolean(true));
        return Ok(());
    }

    let mut acc = true;
    let mut numbers = vm.pop_as_vec(n_of_args).unwrap();
    let mut last = numbers.pop().unwrap();

    if !last.is_number() {
        return Err(ExecutionError::BadArgType);
    }

    while let Some(n) = numbers.pop() {
        acc = acc && match (last, &n) {
            (Value::Integer(n), &Value::Integer(m)) => n <= m,
            (Value::Integer(n), &Value::Float(f)) => (n as f32) <= f,
            (Value::Float(f), &Value::Integer(n)) => f <= (n as f32),
            (Value::Float(f), &Value::Float(g)) => f <= g,
            _ => return Err(ExecutionError::BadArgType),
        };

        last = n;

        if !acc {
            break;
        }
    }

    vm.stack.push(Value::Boolean(acc));
    Ok(())
}

macro_rules! simple_type {
    ($name:ident) => (
        fn $name(
            vm: &mut VmState, _: (usize, bool), _: &Option<Branch>
        ) -> Result<(), ExecutionError> {
            let value = vm.stack.pop().unwrap();
            vm.stack.push(Value::Boolean(Value::$name(&value)));
            Ok(())
        }
    )
}

// TODO: change to brackets if/when rustfmt allows
simple_type!(is_vector);
simple_type!(is_symbol);
simple_type!(is_procedure);
simple_type!(is_string);
simple_type!(is_char);
simple_type!(is_boolean);
simple_type!(is_null);
simple_type!(is_pair);
simple_type!(is_number);
simple_type!(is_list);
