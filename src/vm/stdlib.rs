use super::{shared, Branch, DeepEqual, ExecutionError, Pair, Value, VmState};

type RunResult = Result<(), ExecutionError>;
type NatFn = fn(&mut VmState, CallInfo, &Option<Branch>) -> RunResult;
type CallInfo = (usize, bool);

pub(super) const STDLIB: &[(&str, NatFn, CallInfo)] = &[
    ("list", list, (0, true)),
    ("cons", cons, (2, false)),
    ("apply", apply, (2, false)),
    ("car", car, (1, false)),
    ("cdr", cdr, (1, false)),
    ("caar", caar, (1, false)),
    ("cadr", cadr, (1, false)),
    ("cdar", cdar, (1, false)),
    ("cddr", cddr, (1, false)),
    ("caaar", caaar, (1, false)),
    ("caadr", caadr, (1, false)),
    ("cadar", cadar, (1, false)),
    ("caddr", caddr, (1, false)),
    ("cdaar", cdaar, (1, false)),
    ("cdadr", cdadr, (1, false)),
    ("cddar", cddar, (1, false)),
    ("cdddr", cdddr, (1, false)),
    ("caaaar", caaaar, (1, false)),
    ("caaadr", caaadr, (1, false)),
    ("caadar", caadar, (1, false)),
    ("caaddr", caaddr, (1, false)),
    ("cadaar", cadaar, (1, false)),
    ("cadadr", cadadr, (1, false)),
    ("caddar", caddar, (1, false)),
    ("cadddr", cadddr, (1, false)),
    ("cdaaar", cdaaar, (1, false)),
    ("cdaadr", cdaadr, (1, false)),
    ("cdadar", cdadar, (1, false)),
    ("cdaddr", cdaddr, (1, false)),
    ("cddaar", cddaar, (1, false)),
    ("cddadr", cddadr, (1, false)),
    ("cdddar", cdddar, (1, false)),
    ("cddddr", cddddr, (1, false)),
    ("set-car!", set_car, (2, false)),
    ("set-cdr!", set_cdr, (2, false)),
    ("length", length, (1, false)),
    ("force", force, (1, false)),
    ("eqv?", are_eqv, (2, false)),
    // TODO ("eq?", are_eq, (2, false)),
    ("equal?", are_equal, (2, false)),
    ("+", addition, (0, true)),
    ("*", multiplication, (0, true)),
    ("-", substraction, (0, true)),
    ("<=", leq_than, (0, true)),
    ("<", less_than, (0, true)),
    (">", greater_than, (0, true)),
    (">=", geq_than, (0, true)),
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

fn list(vm: &mut VmState, call_info: (usize, bool), _: &Option<Branch>) -> RunResult {
    let list = vm.pop_as_list(call_info.0, false).expect("Bad argc");
    vm.stack.push(list);
    Ok(())
}

fn cons(vm: &mut VmState, _: (usize, bool), _: &Option<Branch>) -> RunResult {
    let cdr = vm.stack.pop().expect("bad cdr");
    let car = vm.stack.pop().expect("bad car");
    vm.stack.push(Value::Pair(shared(Pair(car, cdr))));
    Ok(())
}

macro_rules! set_pair {
    ($name:ident, $borrowed:ident, $part:expr) => {
        fn $name(vm: &mut VmState, _: (usize, bool), _: &Option<Branch>) -> RunResult {
            let car = vm.stack.pop().unwrap();
            let pair = if let Value::Pair(pair) = vm.stack.pop().unwrap() {
                pair
            } else {
                return Err(ExecutionError::BadArgType);
            };

            let mut $borrowed = pair.borrow_mut();
            $part = car;

            vm.stack.push(Value::Nil);
            Ok(())
        }
    };
}

set_pair!(set_car, borrowed, borrowed.0);
set_pair!(set_cdr, borrowed, borrowed.1);

fn length(vm: &mut VmState, _: (usize, bool), _: &Option<Branch>) -> RunResult {
    let mut n = 0;

    let mut value = vm.stack.pop().unwrap();

    loop {
        if value.is_null() {
            break;
        }

        let next = if let Value::Pair(ref x) = value {
            x.borrow().1.clone()
        } else {
            return Err(ExecutionError::BadArgType);
        };

        n += 1;

        value = next;
    }

    vm.stack.push(Value::Integer(n));
    Ok(())
}

fn apply(vm: &mut VmState, call_info: (usize, bool), branch: &Option<Branch>) -> RunResult {
    let arg_list = vm.stack.pop().unwrap();
    if !arg_list.is_list() {
        return Err(ExecutionError::BadArgType);
    }
    let args_passed = arg_list.list_len().unwrap();

    let procedure = vm.stack.pop().unwrap();

    if !procedure.is_procedure() {
        return Err(ExecutionError::NonCallable);
    }

    vm.push_list(arg_list);
    vm.stack.push(procedure);
    vm.call(call_info.1, args_passed, branch)
}

macro_rules! car {
    ($v:expr) => {{
        let pair = $v.pair().ok_or(ExecutionError::BadArgType)?;
        let borrowed = pair.borrow();
        borrowed.0.clone()
    }};
}

macro_rules! cdr {
    ($v:expr) => {{
        let pair = $v.pair().ok_or(ExecutionError::BadArgType)?;
        let borrowed = pair.borrow();
        borrowed.1.clone()
    }};
}

macro_rules! pair_getter_expr {
    ($v:expr) => { $v };
    ($v:expr,) => { $v };
    ($v:expr, a, $( $token:ident, )*) => { car![pair_getter_expr![$v, $($token,)*]] };
    ($v:expr, d, $( $token:ident, )*) => { cdr![pair_getter_expr![$v, $($token,)*]] };
}

macro_rules! pair_getter_fn {
    ($fun:ident, $( $token:ident, )*) => {
        fn $fun(vm: &mut VmState, _call_info: (usize, bool), _: &Option<Branch>) -> RunResult {
            let result = pair_getter_expr![vm.stack.pop().unwrap(), $( $token, )*];
            vm.stack.push(result);
            Ok(())
        }
    }
}

pair_getter_fn![car, a,];
pair_getter_fn![cdr, d,];
pair_getter_fn![caar, a, a,];
pair_getter_fn![cadr, a, d,];
pair_getter_fn![cdar, d, a,];
pair_getter_fn![cddr, d, d,];
pair_getter_fn![caaar, a, a, a,];
pair_getter_fn![caadr, a, a, d,];
pair_getter_fn![cadar, a, d, a,];
pair_getter_fn![caddr, a, d, d,];
pair_getter_fn![cdaar, d, a, a,];
pair_getter_fn![cdadr, d, a, d,];
pair_getter_fn![cddar, d, d, a,];
pair_getter_fn![cdddr, d, d, d,];
pair_getter_fn![caaaar, a, a, a, a,];
pair_getter_fn![caaadr, a, a, a, d,];
pair_getter_fn![caadar, a, a, d, a,];
pair_getter_fn![caaddr, a, a, d, d,];
pair_getter_fn![cadaar, a, d, a, a,];
pair_getter_fn![cadadr, a, d, a, d,];
pair_getter_fn![caddar, a, d, d, a,];
pair_getter_fn![cadddr, a, d, d, d,];
pair_getter_fn![cdaaar, d, a, a, a,];
pair_getter_fn![cdaadr, d, a, a, d,];
pair_getter_fn![cdadar, d, a, d, a,];
pair_getter_fn![cdaddr, d, a, d, d,];
pair_getter_fn![cddaar, d, d, a, a,];
pair_getter_fn![cddadr, d, d, a, d,];
pair_getter_fn![cdddar, d, d, d, a,];
pair_getter_fn![cddddr, d, d, d, d,];

fn force(vm: &mut VmState, call_info: (usize, bool), branch: &Option<Branch>) -> RunResult {
    if !vm.stack.get(0).unwrap().is_promise() {
        return Ok(());
    }

    let (code, environment) = if let Value::Promise { code, environment } = vm.stack.pop().unwrap()
    {
        (code, environment)
    } else {
        unreachable!();
    };

    vm.non_native_call(call_info.1, &environment, code, branch);

    Ok(())
}

fn are_eqv(vm: &mut VmState, _call_info: (usize, bool), _: &Option<Branch>) -> RunResult {
    let x = vm.stack.pop().unwrap();
    let y = vm.stack.pop().unwrap();

    vm.stack.push(Value::Boolean(x == y));

    Ok(())
}

fn are_equal(vm: &mut VmState, _call_info: (usize, bool), _: &Option<Branch>) -> RunResult {
    let x = vm.stack.pop().unwrap();
    let y = vm.stack.pop().unwrap();

    vm.stack.push(Value::Boolean(x.equal(&y)));

    Ok(())
}

fn addition(vm: &mut VmState, (n_of_args, _): (usize, bool), _: &Option<Branch>) -> RunResult {
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
) -> RunResult {
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

fn substraction(vm: &mut VmState, (n_of_args, _): (usize, bool), _: &Option<Branch>) -> RunResult {
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

macro_rules! comparator_impl {
    ($name:ident, $method:path) => {
        fn $name(vm: &mut VmState, (n_of_args, _): (usize, bool), _: &Option<Branch>) -> RunResult {
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
                    (Value::Integer(n), &Value::Integer(m)) => $method(&n, &m),
                    (Value::Integer(n), &Value::Float(f)) => $method(&(n as f32), &f),
                    (Value::Float(f), &Value::Integer(n)) => $method(&f, &(n as f32)),
                    (Value::Float(f), &Value::Float(g)) => $method(&f, &g),
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
    };
}

use std::cmp::PartialOrd;
comparator_impl!(less_than, PartialOrd::lt);
comparator_impl!(leq_than, PartialOrd::le);
comparator_impl!(greater_than, PartialOrd::gt);
comparator_impl!(geq_than, PartialOrd::ge);

macro_rules! simple_type {
    ($name:ident) => {
        fn $name(vm: &mut VmState, _: (usize, bool), _: &Option<Branch>) -> RunResult {
            let value = vm.stack.pop().unwrap();
            vm.stack.push(Value::Boolean(Value::$name(&value)));
            Ok(())
        }
    };
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
