use std::collections::HashMap;
use std::rc::Rc;
use self::gc::{shared, GcShared};
use self::stack::Stack;
use self::value::{Environment, Scalar, Value};
use compiler::Instruction;

mod environment;
mod gc;
mod stack;
mod value;

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


pub fn exec(bytecode: Vec<Instruction>, environment: GcShared<Environment>) -> Result<Value, ()> {
    let mut pc = 0;
    let mut stack = Stack::new();
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
                stack.push_front(Value::Scalar(Scalar::Character(c)));
            }
            Instruction::Boolean(b) => {
                stack.push_front(Value::Scalar(Scalar::Boolean(b)));
            }
            Instruction::Symbol(ref s) => {
                stack.push_front(Value::Scalar(Scalar::Symbol(s.clone())));
            }
            Instruction::Integer(n) => {
                stack.push_front(Value::Scalar(Scalar::Integer(n)));
            }
            Instruction::Nil => {
                stack.push_front(Value::Scalar(Scalar::Nil));
            }
            Instruction::String(ref s) => {
                stack.push_front(Value::String(s.clone()));
            }
            Instruction::Number => {
                stack.push_front(Value::Number);
            }
            Instruction::EmptyList => {
                stack.push_front(Value::Scalar(Scalar::EmptyList));
            }
            // Instruction::Pop => { stack.pop_front(); },
            Instruction::Pair => {
                let car = stack.pop_front().expect("No car");
                let cdr = stack.pop_front().expect("No cdr");
                let pair = Value::Pair {
                    car: shared(car),
                    cdr: shared(cdr),
                };
                stack.push_front(pair);
            }
            Instruction::Vector(n) => {
                let mut vector = vec![];
                for _ in 0..n {
                    vector.push(stack.pop_front().expect("vector entry"));
                }
                stack.push_front(Value::Vector(vector));
            }
            // Stack in reverse order:
            // a_n, ..., a_1, ...
            Instruction::List(0, false) => {
                stack.push_front(Value::Scalar(Scalar::EmptyList));
            }
            Instruction::List(n, improper) => {
                let cdr = if improper {
                    stack.pop_front().unwrap()
                } else {
                    Value::Scalar(Scalar::EmptyList)
                };

                let mut pair = Value::Pair {
                    car: shared(stack.pop_front().unwrap()),
                    cdr: shared(cdr),
                };

                let rest = if improper { n } else { n - 1 };

                for _ in 0..rest {
                    let val = stack.pop_front().unwrap();
                    pair = Value::Pair {
                        car: shared(val),
                        cdr: shared(pair),
                    };
                }
                stack.push_front(pair);
            }
            Instruction::Lambda(ref code) => {
                let environment = current_env.clone();
                let procedure = Value::Procedure {
                    code: code.clone(),
                    environment,
                };
                stack.push_front(procedure);
            }
            // Stack:                End stack:
            // * n_of_args           * n_of_args
            // * proc                * arg1
            // * arg1                * ...
            // * ...                 * return_record
            Instruction::Call(tail) => {
                if call_stack_depth > MAX_CALL_STACK_DEPTH {
                    return Err(());
                }

                let val = stack.swap_remove_front(1).ok_or_else(|| ())?;
                let (code, environment) = match val {
                    Value::Procedure { code, environment } => (code, environment),
                    Value::NativeProcedure(fun) => {
                        call_native_procedure(&mut stack, fun)?;
                        pc += 1;
                        continue;
                    }
                    _ => return Err(()),
                };

                let environment = environment.new();

                if !tail {
                    call_stack_depth += 1;

                    let n_of_args = if let &Value::Scalar(Scalar::Integer(ref n)) =
                        stack.get(0).ok_or_else(|| ())?
                    {
                        *n as usize
                    } else {
                        return Err(());
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
                let args_passed = match stack.pop_front().ok_or_else(|| ())? {
                    Value::Scalar(Scalar::Integer(n)) => n as usize,
                    _ => return Err(()),
                };

                if !rest {
                    if n_of_args == args_passed {
                        pc += 1;
                        continue;
                    } else {
                        return Err(());
                    }
                }

                if n_of_args > 0 && args_passed < n_of_args {
                    return Err(());
                }

                let diff = args_passed - n_of_args;
                let mut rest_args = vec![];
                for _ in 0..diff {
                    rest_args.push(stack.remove(n_of_args).unwrap());
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
                let value = stack.pop_front().unwrap();
                current_env
                    .borrow_mut()
                    .set(var_name.clone(), value.clone());
                stack.push_front(Value::Scalar(Scalar::Nil));
            }
            Instruction::LoadVar(ref var_name) => {
                let value = current_env.borrow().get(var_name.clone()).ok_or_else(|| ())?;
                stack.push_front(value);
            }
            Instruction::DefineVar(ref var_name) => {
                let value = stack.pop_front().expect("set var");
                current_env.borrow_mut().define(var_name.clone(), value);
            }
            Instruction::BranchUnless(n) => {
                let cond: bool = (&stack.pop_front().expect("test")).into();
                if !cond {
                    next_pc = Some(pc + n);
                }
            }
            Instruction::BranchIf(n) => {
                let cond: bool = (&stack.pop_front().expect("test")).into();
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

                let return_record = stack.swap_remove_front(1).expect("return_record");

                let (ra, environment, code) = if let Value::ReturnRecord {
                    environment,
                    address,
                    code,
                } = return_record
                {
                    (address, environment, code)
                } else {
                    return Err(());
                };

                current_env = environment;
                next_compiled_code = Some(code);
                next_pc = Some(ra);
            }
            Instruction::Pop => {
                stack.pop_front();
            }
            Instruction::NewEnv => {
                current_env = current_env.new();
            }
            Instruction::PopEnv => {
                let parent = current_env.borrow().parent.as_ref().unwrap().clone();
                current_env = parent;
            }
            Instruction::And => {
                let value1 = stack.pop_front().unwrap();
                let cond1: bool = (&value1).into();
                let value2 = stack.pop_front().unwrap();
                let result = cond1 && (&value2).into();
                stack.push_front(Value::Scalar(Scalar::Boolean(result)));
            }
            Instruction::Or => {
                let value1 = stack.pop_front().unwrap();
                let cond1: bool = (&value1).into();
                let value2 = stack.pop_front().unwrap();
                let result = cond1 || (&value2).into();
                stack.push_front(Value::Scalar(Scalar::Boolean(result)));
            }
        }

        pc = next_pc.unwrap_or(pc + 1);

        if compiled_code.is_none() && bytecode.len() == pc {
            break;
        }
    }

    Ok(stack.pop_front().expect("empty stack"))
}


fn call_native_procedure(stack: &mut Stack<Value>, fun: fn(Vec<Value>) -> Value) -> Result<(), ()> {
    let n = if let Value::Scalar(Scalar::Integer(n)) = stack.pop_front().ok_or_else(|| ())? {
        n as usize
    } else {
        return Err(());
    };
    let mut values = vec![];
    for _ in 0..n {
        values.push(stack.pop_front().ok_or_else(|| ())?);
    }
    let ret = fun(values);
    stack.push_front(ret);
    Ok(())
}

fn list(mut values: Vec<Value>) -> Value {
    if values.len() == 0 {
        return Value::Scalar(Scalar::EmptyList);
    }

    let mut pair = Value::Pair {
        car: shared(values.pop().unwrap()),
        cdr: shared(Value::Scalar(Scalar::EmptyList)),
    };

    values.reverse();

    for val in values.into_iter() {
        pair = Value::Pair {
            car: shared(val),
            cdr: shared(pair),
        }
    }

    pair
}

pub fn default_env() -> GcShared<Environment> {
    let mut env = Environment::default();

    env.define("list".into(), Value::NativeProcedure(list));
    shared(env)
}
