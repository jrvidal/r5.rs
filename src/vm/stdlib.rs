
use super::{shared, ExecutionError, Value, VmState, Branch};

type NatFn = fn(&mut VmState, CallInfo, &Option<Branch>) -> Result<(), ExecutionError>;
type CallInfo = (usize, bool);

pub(super) const STDLIB: [(&'static str, NatFn, CallInfo); 5] = [
    ("list", list, (0, true)),
    ("cons", cons, (2, false)),
    ("apply", apply, (2, false)),
    ("car", car, (1, false)),
    ("cdr", cdr, (1, false)),
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
    let list = vm.pop_as_list(call_info.0).expect("Bad argc");
    vm.stack.push(list);
    Ok(())
}

fn cons(vm: &mut VmState, _: (usize, bool), _: &Option<Branch>) -> Result<(), ExecutionError> {
    let cdr = shared(vm.stack.pop().expect("bad cdr"));
    let car = shared(vm.stack.pop().expect("bad car"));
    vm.stack.push(Value::Pair { car, cdr });
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
    if let Value::Pair { ref car, .. } = vm.stack.pop().unwrap() {
        vm.stack.push(car.borrow().clone());
    } else {
        return Err(ExecutionError::BadArgType);
    }

    Ok(())
}

fn cdr(vm: &mut VmState, _call_info: (usize, bool), _: &Option<Branch>) -> Result<(), ExecutionError> {
    if let Value::Pair { ref cdr, .. } = vm.stack.pop().unwrap() {
        vm.stack.push(cdr.borrow().clone());
    } else {
        return Err(ExecutionError::BadArgType);
    }

    Ok(())
}