
use super::{shared, ExecutionError, Value, VmState, Branch, Pair};

type NatFn = fn(&mut VmState, CallInfo, &Option<Branch>) -> Result<(), ExecutionError>;
type CallInfo = (usize, bool);

pub(super) const STDLIB: [(&'static str, NatFn, CallInfo); 8] = [
    ("list", list, (0, true)),
    ("cons", cons, (2, false)),
    ("apply", apply, (2, false)),
    ("car", car, (1, false)),
    ("cdr", cdr, (1, false)),
    ("force", force, (1, false)),
    ("eqv?", are_eqv, (2, false)),
    ("null?", is_null, (1, false)),
];

// By the time a native procedure is called:
// * The arity has been checked to be correct.
// * The arguments are in inverse order in the stack: arg_n, ... arg_1

// macro_rules! simple_native_function {
//     ($fn:ident, $arguments:ident, $body:block) => (
//         pub(super) fn $fn(vm: &mut VmState, call_info: (usize, bool), _: &Option<Branch>) -> Result<(), ExecutionError> {
//             let (passed_args, _is_tail) = call_info;

//             fn compute(mut $arguments: Vec<Value>) -> Result<Value, ExecutionError> {
//                 $body
//             }

//             let mut arguments = vec![];
//             for _ in 0..passed_args {
//                 arguments.push(vm.stack.pop().ok_or(ExecutionError::Internal("bad argc"))?);
//             }
//             arguments.reverse();

//             let ret = compute(arguments)?;
//             vm.stack.push(ret);
//             Ok(())
//         }
//     )
// }

fn list(vm: &mut VmState, call_info: (usize, bool), _: &Option<Branch>) -> Result<(), ExecutionError> {
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


fn apply(vm: &mut VmState, call_info: (usize, bool), branch: &Option<Branch>) -> Result<(), ExecutionError> {
    let arg_list = vm.stack.pop().unwrap();
    if !arg_list.is_list() {
        return Err(ExecutionError::BadArgType);
    }
    let args_passed = arg_list.list_len().unwrap();

    let procedure = vm.stack.pop().unwrap();

    match procedure {
      Value::Procedure { .. } | Value::NativeProcedure { .. } => {},
      _ => return Err(ExecutionError::NonCallable)
    }

    vm.push_list(arg_list);
    vm.stack.push(procedure);
    vm.call(call_info.1, args_passed, branch)
}

fn car(vm: &mut VmState, _call_info: (usize, bool), _: &Option<Branch>) -> Result<(), ExecutionError> {
    let pair = vm.stack.pop().unwrap().pair().ok_or(ExecutionError::BadArgType)?;
    vm.stack.push(pair.borrow().0.clone());
    Ok(())
}

fn cdr(vm: &mut VmState, _call_info: (usize, bool), _: &Option<Branch>) -> Result<(), ExecutionError> {
    let pair = vm.stack.pop().unwrap().pair().ok_or(ExecutionError::BadArgType)?;
    vm.stack.push(pair.borrow().1.clone());
    Ok(())
}

fn force(vm: &mut VmState, call_info: (usize, bool), branch: &Option<Branch>) -> Result<(), ExecutionError> {
    if let &Value::Promise { .. } = vm.stack.get(0).unwrap() {
        // Pass
    } else {
        return Ok(());
    }

    let (code, environment) = if let Value::Promise { code, environment } = vm.stack.pop().unwrap() {
        (code, environment)
    } else {
        unreachable!();
    };

    vm.non_native_call(call_info.1, environment, code, branch);

    Ok(())
}

fn are_eqv(vm: &mut VmState, _call_info: (usize, bool), _: &Option<Branch>) -> Result<(), ExecutionError> {
    let x = vm.stack.pop().unwrap();
    let y = vm.stack.pop().unwrap();

    vm.stack.push(Value::Boolean(x == y));

    Ok(())
}

fn is_null(vm: &mut VmState, _call_info: (usize, bool), _: &Option<Branch>) -> Result<(), ExecutionError> {
    if let Value::EmptyList = vm.stack.pop().unwrap() {
        vm.stack.push(Value::Boolean(true));
    } else {
        vm.stack.push(Value::Boolean(false));
    }

    Ok(())
}
