use std::collections::HashMap;
use std::rc::Rc;
use self::stack::Stack;
use self::gc::shared;
pub use self::value::{Scalar, Value};
use compiler::Instruction;

pub use self::gc::GcShared;
pub use self::value::Environment;

mod environment;
mod gc;
mod stack;
mod value;
mod stdlib;

const MAX_CALL_STACK_DEPTH: usize = 128;

impl Newable for GcShared<Environment> {
    fn new(&self) -> GcShared<Environment> {
        shared(Environment {
            parent: Some(self.clone()),
            bindings: HashMap::new(),
        })
    }
}

pub fn null_env() -> GcShared<Environment> {
    shared(Environment {
        parent: None,
        bindings: HashMap::new(),
    })
}

trait Newable {
    fn new(&self) -> Self;
}

#[derive(Debug)]
pub enum ExecutionError {
    StackOverflow,
    NonCallable,
    BadArgc,
    UnboundVar,
    Internal(&'static str),
}


pub fn exec(
    bytecode: &[Instruction],
    environment: GcShared<Environment>,
) -> Result<Value, ExecutionError> {
    use self::ExecutionError::*;

    let mut pc = 0;
    let mut stack = Stack::default();
    let mut current_env = environment;
    let mut compiled_code: Option<Rc<Vec<Instruction>>> = None;
    let mut next_compiled_code = None;
    let mut call_stack_depth = 0;

    loop {
        compiled_code = next_compiled_code.unwrap_or(compiled_code);
        next_compiled_code = None;
        let instruction: &Instruction = if let Some(ref compiled_code) = compiled_code {
            &compiled_code[pc]
        } else {
            if pc >= bytecode.len() {
                break;
            } else {
                &bytecode[pc]
            }
        };
        debug!(
            "pc {:?}\tcode: {:?}\tdepth: {:?}\n\t stack: {:?}",
            pc,
            instruction,
            call_stack_depth,
            stack
        );
        let mut next_pc = None;

        match *instruction {
            Instruction::Character(c) => {
                stack.push(Value::Scalar(Scalar::Character(c)));
            }
            Instruction::Boolean(b) => {
                stack.push(Value::Scalar(Scalar::Boolean(b)));
            }
            Instruction::Symbol(ref s) => {
                stack.push(Value::Scalar(Scalar::Symbol(s.clone())));
            }
            Instruction::Integer(n) => {
                stack.push(Value::Scalar(Scalar::Integer(n)));
            }
            Instruction::Nil => {
                stack.push(Value::Scalar(Scalar::Nil));
            }
            Instruction::String(ref s) => {
                stack.push(Value::String(s.clone()));
            }
            Instruction::Number => {
                stack.push(Value::Number);
            }
            Instruction::EmptyList => {
                stack.push(Value::Scalar(Scalar::EmptyList));
            }
            // Instruction::Pop => { stack.pop(); },
            Instruction::Pair => {
                let car = stack.pop().expect("No car");
                let cdr = stack.pop().expect("No cdr");
                let pair = Value::Pair {
                    car: shared(car),
                    cdr: shared(cdr),
                };
                stack.push(pair);
            }
            Instruction::Vector(n) => {
                let mut vector = vec![];
                for _ in 0..n {
                    vector.push(stack.pop().expect("vector entry"));
                }
                stack.push(Value::Vector(vector));
            }
            // Stack in reverse order:
            // a_n, ..., a_1, ...
            Instruction::List(0, false) => {
                stack.push(Value::Scalar(Scalar::EmptyList));
            }
            Instruction::List(n, improper) => {
                let cdr = if improper {
                    stack.pop().unwrap()
                } else {
                    Value::Scalar(Scalar::EmptyList)
                };

                let mut pair = Value::Pair {
                    car: shared(stack.pop().unwrap()),
                    cdr: shared(cdr),
                };

                let rest = if improper { n } else { n - 1 };

                for _ in 0..rest {
                    let val = stack.pop().unwrap();
                    pair = Value::Pair {
                        car: shared(val),
                        cdr: shared(pair),
                    };
                }
                stack.push(pair);
            }
            Instruction::Lambda(ref code) => {
                let environment = current_env.clone();
                let procedure = Value::Procedure {
                    code: code.clone(),
                    environment,
                };
                stack.push(procedure);
            }
            // Stack:                End stack:
            // * n_of_args           * n_of_args
            // * proc                * arg1
            // * arg1                * ...
            // * ...                 * return_record
            Instruction::Call(tail) => {
                if call_stack_depth > MAX_CALL_STACK_DEPTH {
                    return Err(StackOverflow);
                }

                let val = stack.swap_remove(1);
                let (code, environment) = match val {
                    Value::Procedure { code, environment } => (code, environment),
                    Value::NativeProcedure(fun) => {
                        call_native_procedure(&mut stack, fun)?;
                        pc += 1;
                        continue;
                    }
                    _ => return Err(NonCallable),
                };

                let environment = environment.new();

                if !tail {
                    call_stack_depth += 1;

                    let n_of_args = match stack.get(0) {
                        Some(&Value::Scalar(Scalar::Integer(ref n))) if *n >= 0 => *n as usize,
                        _ => return Err(Internal("no argc on stack")),
                    };

                    let return_record = Value::ReturnRecord {
                        environment: current_env,
                        address: pc + 1,
                        code: compiled_code.clone(),
                    };
                    stack.insert(n_of_args + 1, return_record);
                }

                current_env = environment;
                next_compiled_code = Some(Some(code));
                next_pc = Some(0);
            }
            Instruction::Arity(n_of_args, rest) => {
                let args_passed = match stack.pop() {
                    Some(Value::Scalar(Scalar::Integer(n))) if n >= 0 => n as usize,
                    _ => return Err(Internal("no argc on stack")),
                };

                if !rest {
                    if n_of_args == args_passed {
                        pc += 1;
                        continue;
                    } else {
                        return Err(BadArgc);
                    }
                }

                if n_of_args > 0 && args_passed < n_of_args {
                    return Err(BadArgc);
                }

                let diff = args_passed - n_of_args;
                let mut rest_args = vec![];
                for _ in 0..diff {
                    rest_args.push(stack.remove(n_of_args));
                }

                let mut pair = Value::Scalar(Scalar::EmptyList);
                while rest_args.len() > 0 {
                    let car = rest_args.pop().unwrap();
                    pair = Value::Pair {
                        car: shared(car),
                        cdr: shared(pair),
                    };
                }
                stack.insert(n_of_args, pair);
            }
            Instruction::SetVar(ref var_name) => {
                let value = stack.pop().unwrap();
                current_env
                    .borrow_mut()
                    .set(var_name.clone(), value.clone());
                stack.push(Value::Scalar(Scalar::Nil));
            }
            Instruction::LoadVar(ref var_name) => {
                let value = current_env.borrow().get(var_name.clone()).ok_or(UnboundVar)?;
                stack.push(value);
            }
            Instruction::DefineVar(ref var_name) => {
                let value = stack.pop().expect("set var");
                current_env.borrow_mut().define(var_name.clone(), value);
            }
            Instruction::BranchUnless(n) => {
                let cond: bool = (&stack.pop().expect("test")).into();
                if !cond {
                    next_pc = Some(pc + n);
                }
            }
            Instruction::BranchIf(n) => {
                let cond: bool = (&stack.pop().expect("test")).into();
                if cond {
                    next_pc = Some(pc + n);
                }
            }
            Instruction::ROBranchUnless(n) => {
                let cond: bool = (&stack[0]).into();
                if !cond {
                    next_pc = Some(pc + n);
                }
            }
            Instruction::ROBranchIf(n) => {
                let cond: bool = (&stack[0]).into();
                if cond {
                    next_pc = Some(pc + n);
                }
            }
            Instruction::Branch(n) => {
                next_pc = Some(pc + n);
            }
            // Stack:
            // * ret_value
            // * return_record
            Instruction::Ret => {
                call_stack_depth -= 1;

                let return_record = stack.swap_remove(1);

                let (ra, environment, code) = if let Value::ReturnRecord {
                    environment,
                    address,
                    code,
                } = return_record
                {
                    (address, environment, code)
                } else {
                    return Err(Internal("no return record on stack"));
                };

                current_env = environment;
                next_compiled_code = Some(code);
                next_pc = Some(ra);
            }
            Instruction::Pop => {
                stack.pop();
            }
            Instruction::NewEnv => {
                current_env = current_env.new();
            }
            Instruction::PopEnv => {
                let parent = current_env.borrow().parent.as_ref().unwrap().clone();
                current_env = parent;
            }
            Instruction::And => {
                let value1 = stack.pop().unwrap();
                let cond1: bool = (&value1).into();
                let value2 = stack.pop().unwrap();
                let result = cond1 && (&value2).into();
                stack.push(Value::Scalar(Scalar::Boolean(result)));
            }
            Instruction::Or => {
                let value1 = stack.pop().unwrap();
                let cond1: bool = (&value1).into();
                let value2 = stack.pop().unwrap();
                let result = cond1 || (&value2).into();
                stack.push(Value::Scalar(Scalar::Boolean(result)));
            }
        }

        pc = next_pc.unwrap_or(pc + 1);

        if compiled_code.is_none() && bytecode.len() == pc {
            break;
        }
    }

    stack.pop().ok_or(Internal("Empty stack"))
}


fn call_native_procedure(
    stack: &mut Stack<Value>,
    fun: fn(Vec<Value>) -> Result<Value, ExecutionError>,
) -> Result<(), ExecutionError> {
    let n = match stack.pop() {
        Some(Value::Scalar(Scalar::Integer(n))) if n >= 0 => n as usize,
        _ => return Err(ExecutionError::Internal("no argc on stack")),
    };

    let mut values = vec![];
    for _ in 0..n {
        values.push(stack
            .pop()
            .ok_or(ExecutionError::Internal("insufficient args"))?);
    }
    fun(values).map(|ret| {
        stack.push(ret);
        ()
    })
}


pub fn default_env() -> GcShared<Environment> {
    let mut env = Environment::default();

    env.define("list".into(), Value::NativeProcedure(stdlib::list));
    env.define("cons".into(), Value::NativeProcedure(stdlib::cons));
    shared(env)
}
