use std::collections::HashMap;
use std::rc::Rc;
use self::stack::Stack;
use self::gc::shared;
pub use self::value::{NativeProcedure, Value};
use compiler::Instruction;

pub use self::gc::GcShared;
pub use self::value::Environment;

mod environment;
mod gc;
mod stack;
mod value;
mod stdlib;

const MAX_CALL_STACK_DEPTH: usize = 255;

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

#[derive(Debug, PartialEq)]
pub enum ExecutionError {
    StackOverflow,
    NonCallable,
    BadArgc,
    BadArgType,
    UnboundVar,
    Internal(&'static str),
}

type Branch = Rc<Vec<Instruction>>;

#[derive(Debug)]
struct ReturnRecord {
    environment: GcShared<Environment>,
    address: usize,
    code: Option<Branch>,
}

#[derive(Debug)]
struct VmState {
    pc: usize,
    environment: GcShared<Environment>,
    stack: Stack<Value>,
    call_stack: Stack<ReturnRecord>,
    // None: no changes ; Some(x): set current branch to x
    next_branch: Option<Option<Branch>>,
    next_pc: Option<usize>,
}

impl VmState {
    fn call(
        &mut self,
        tail_call: bool,
        args_passed: usize,
        branch: &Option<Branch>,
    ) -> Result<(), ExecutionError> {
        use self::ExecutionError::*;

        if self.call_stack.len() >= MAX_CALL_STACK_DEPTH {
            return Err(StackOverflow);
        }

        let (fn_details, arity) = match self.stack.pop().ok_or(Internal("No proc"))? {
            Value::Procedure {
                code,
                environment,
                arity,
            } => (Ok((code, environment)), arity),
            Value::NativeProcedure(NativeProcedure { fun, arity }) => (Err(fun), arity),
            _ => return Err(NonCallable),
        };

        let native_call = fn_details.is_err();

        let (n_of_args, rest) = arity;

        if (!rest && args_passed != n_of_args) || (rest && args_passed < n_of_args) {
            return Err(BadArgc);
        }

        if rest && args_passed == n_of_args && !native_call {
            self.stack.push(Value::EmptyList);
        } else if rest && args_passed > n_of_args && !native_call {
            let arg_list = self.pop_as_list(args_passed - n_of_args)
                .ok_or(Internal("No arg"))?;
            self.stack.push(arg_list);
        }

        match fn_details {
            Ok((code, environment)) => {
                use std::mem;
                let old_environment = mem::replace(&mut self.environment, environment.new());

                if !tail_call {
                    let return_record = ReturnRecord {
                        environment: old_environment,
                        address: self.pc + 1,
                        code: branch.clone(),
                    };
                    self.call_stack.push(return_record);
                }

                self.next_branch = Some(Some(code));
                self.next_pc = Some(0);
            }
            Err(fun) => {
                fun(self, (args_passed, tail_call), branch)?;
            }
        }
        Ok(())
    }

    // TODO
    #[allow(dead_code)]
    fn pop_as_vec(&mut self, count: usize) -> Option<Vec<Value>> {
        let mut values = vec![];
        for _ in 0..count {
            if let Some(val) = self.stack.pop() {
                values.push(val);
            } else {
                return None;
            }
        }
        Some(values)
    }

    fn pop_as_list(&mut self, count: usize) -> Option<Value> {
        let mut pair = Value::EmptyList;


        for _ in 0..count {
            pair = Value::Pair {
                cdr: shared(pair),
                car: match self.stack.pop() {
                    Some(val) => shared(val),
                    None => return None,
                },
            }
        }

        Some(pair)
    }

    fn push_list(&mut self, list: Value) {
        if let Value::Pair { car, cdr } = list {
            let mut list = cdr;
            self.stack.push(car.borrow().clone());

            loop {
                let clone = list.borrow().clone();
                if let Value::Pair { car, cdr } = clone {
                    list = cdr;
                    self.stack.push(car.borrow().clone());
                } else {
                    break;
                }
            }
        }
    }
}


pub fn exec(
    bytecode: &[Instruction],
    environment: GcShared<Environment>,
) -> Result<Value, ExecutionError> {
    use self::ExecutionError::*;

    let mut vm = VmState {
        pc: 0,
        environment: environment,
        stack: Stack::default(),
        call_stack: Stack::default(),
        next_pc: None,
        next_branch: None,
    };
    let mut branch: Option<Branch> = None;

    loop {
        // Bleugh, moved here from the bottom to avoid an extra pair of braces
        branch = vm.next_branch.unwrap_or(branch);

        debug!("{:?}", (&branch, vm.pc));

        match branch {
            None if bytecode.len() == vm.pc => break,
            None if vm.pc > bytecode.len() => return Err(Internal("PC overflow")),
            Some(ref b) if vm.pc >= b.len() => return Err(Internal("Ret not encountered")),
            _ => {}
        }

        vm.next_pc = None;
        vm.next_branch = None;

        let instruction = branch
            .as_ref()
            .map(|branch| &(**branch)[vm.pc])
            .or(bytecode.get(vm.pc))
            .ok_or(Internal("PC overflow"))?;

        debug!(
            "pc {:?}\tcode: {:?}\tdepth: {:?}\n\t stack: {:?}",
            vm.pc,
            instruction,
            vm.call_stack.len(),
            vm.stack
        );

        match *instruction {
            Instruction::Character(c) => {
                vm.stack.push(Value::Character(c));
            }
            Instruction::Boolean(b) => {
                vm.stack.push(Value::Boolean(b));
            }
            Instruction::Symbol(ref s) => {
                vm.stack.push(Value::Symbol(s.clone()));
            }
            Instruction::Integer(n) => {
                vm.stack.push(Value::Integer(n));
            }
            Instruction::Nil => {
                vm.stack.push(Value::Nil);
            }
            Instruction::String(ref s) => {
                vm.stack.push(Value::String(shared(s.into())));
            }
            Instruction::Number => {
                vm.stack.push(Value::Number);
            }
            Instruction::EmptyList => {
                vm.stack.push(Value::EmptyList);
            }
            Instruction::Pair => {
                let car = vm.stack.pop().expect("No car");
                let cdr = vm.stack.pop().expect("No cdr");
                let pair = Value::Pair {
                    car: shared(car),
                    cdr: shared(cdr),
                };
                vm.stack.push(pair);
            }
            Instruction::Vector(n) => {
                let mut vector = vec![];
                for _ in 0..n {
                    vector.push(vm.stack.pop().expect("vector entry"));
                }
                vm.stack.push(Value::Vector(shared(vector)));
            }
            // Stack in reverse order:
            // a_n, ..., a_1, ...
            Instruction::List(0, false) => {
                vm.stack.push(Value::EmptyList);
            }
            Instruction::List(n, improper) => {
                let cdr = if improper {
                    vm.stack.pop().unwrap()
                } else {
                    Value::EmptyList
                };

                let mut pair = Value::Pair {
                    car: shared(vm.stack.pop().unwrap()),
                    cdr: shared(cdr),
                };

                let rest = if improper { n } else { n - 1 };

                for _ in 0..rest {
                    let val = vm.stack.pop().unwrap();
                    pair = Value::Pair {
                        car: shared(val),
                        cdr: shared(pair),
                    };
                }
                vm.stack.push(pair);
            }
            Instruction::Lambda { ref code, arity } => {
                let environment = vm.environment.clone();
                let procedure = Value::Procedure {
                    code: code.clone(),
                    environment,
                    arity,
                };
                vm.stack.push(procedure);
            }

            // Stack:                End stack:
            // * proc                * arg1
            // * arg1                * ...
            // * ...                 * return_record
            Instruction::Call(tail_call, args_passed) => vm.call(tail_call, args_passed, &branch)?,
            Instruction::SetVar(ref var_name) => {
                let value = vm.stack.pop().unwrap();
                vm.environment
                    .borrow_mut()
                    .set(var_name.clone(), value.clone());
                vm.stack.push(Value::Nil);
            }
            Instruction::LoadVar(ref var_name) => {
                let value = vm.environment
                    .borrow()
                    .get(var_name.clone())
                    .ok_or(UnboundVar)?;
                vm.stack.push(value);
            }
            Instruction::DefineVar(ref var_name) => {
                let value = vm.stack.pop().expect("set var");
                vm.environment.borrow_mut().define(var_name.clone(), value);
            }
            Instruction::BranchUnless(n) => {
                let cond: bool = (&vm.stack.pop().expect("test")).into();
                if !cond {
                    vm.next_pc = Some(vm.pc + n);
                }
            }
            Instruction::BranchIf(n) => {
                let cond: bool = (&vm.stack.pop().expect("test")).into();
                if cond {
                    vm.next_pc = Some(vm.pc + n);
                }
            }
            Instruction::ROBranchUnless(n) => {
                let cond: bool = (&vm.stack[0]).into();
                if !cond {
                    vm.next_pc = Some(vm.pc + n);
                }
            }
            Instruction::ROBranchIf(n) => {
                let cond: bool = (&vm.stack[0]).into();
                if cond {
                    vm.next_pc = Some(vm.pc + n);
                }
            }
            Instruction::Branch(n) => {
                vm.next_pc = Some(vm.pc + n);
            }
            // Stack:
            // * ret_value
            // * return_record
            Instruction::Ret => {
                let return_record = vm.call_stack
                    .pop()
                    .ok_or(Internal("No return record on stack"))?;

                let ReturnRecord {
                    address,
                    environment,
                    code,
                } = return_record;

                // let (ra, environment, code) = if let Value::ReturnRecord {
                //     environment,
                //     address,
                //     code,
                // } = return_record
                // {
                //     (address, environment, code)
                // } else {
                //     return Err(Internal("no return record on stack"));
                // };

                vm.environment = environment;
                vm.next_branch = Some(code);
                vm.next_pc = Some(address);
            }
            Instruction::Pop => {
                vm.stack.pop();
            }
            Instruction::NewEnv => {
                vm.environment = vm.environment.new();
            }
            Instruction::PopEnv => {
                let parent = vm.environment.borrow().parent.as_ref().unwrap().clone();
                vm.environment = parent;
            }
            Instruction::And => {
                let value1 = vm.stack.pop().unwrap();
                let cond1: bool = (&value1).into();
                let value2 = vm.stack.pop().unwrap();
                let result = cond1 && (&value2).into();
                vm.stack.push(Value::Boolean(result));
            }
            Instruction::Or => {
                let value1 = vm.stack.pop().unwrap();
                let cond1: bool = (&value1).into();
                let value2 = vm.stack.pop().unwrap();
                let result = cond1 || (&value2).into();
                vm.stack.push(Value::Boolean(result));
            }
        }

        vm.pc = vm.next_pc.unwrap_or(vm.pc + 1);
    }

    vm.stack.pop().ok_or(Internal("Empty stack"))
}

pub fn default_env() -> GcShared<Environment> {
    let mut env = Environment::default();

    for &(name, fun, arity) in stdlib::STDLIB.iter() {
        env.define(
            name.into(),
            Value::NativeProcedure(NativeProcedure { fun, arity }),
        );
    }

    shared(env)
}
