//! A virtual machine to interpret the ISA from `compiler`.

use std::collections::{HashMap, VecDeque};
use std::rc::Rc;

use self::gc::shared;
use self::stack::Stack;
pub use self::value::{DeepEqual, NativeProcedure, Pair, Value};
use compiler::Instruction;

pub use self::gc::GcShared;
pub use self::profiler::*;
pub use self::value::Environment;

mod environment;
mod gc;
mod profiler;
mod stack;
mod stdlib;
mod value;

const MAX_CALL_STACK_DEPTH: usize = 1024;

impl Newable for GcShared<Environment> {
    fn new_child(&self) -> GcShared<Environment> {
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
    fn new_child(&self) -> Self;
}

#[derive(Debug, PartialEq)]
pub enum ExecutionError {
    StackOverflow,
    NonCallable,
    BadArgc,
    BadArgType,
    UnboundVar,
    InvalidNumber,
    InvalidPair,
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
            let arg_list = self.pop_as_list(args_passed - n_of_args, false)
                .ok_or(Internal("No arg"))?;
            self.stack.push(arg_list);
        }

        match fn_details {
            Ok((code, environment)) => self.non_native_call(tail_call, &environment, code, branch),
            Err(fun) => {
                fun(self, (args_passed, tail_call), branch)?;
            }
        }
        Ok(())
    }

    fn non_native_call(
        &mut self,
        tail_call: bool,
        environment: &GcShared<Environment>,
        code: Branch,
        branch: &Option<Branch>,
    ) {
        use std::mem;
        let old_environment = mem::replace(&mut self.environment, environment.new_child());

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

    /// Pops `count` values out of the stack into a list in reverse order
    fn pop_as_list(&mut self, count: usize, improper: bool) -> Option<Value> {
        let mut pair = if improper {
            let cdr = self.stack.pop();
            cdr?
        } else {
            Value::EmptyList
        };

        let remaining = count - if improper { 1 } else { 0 };

        for _ in 0..remaining {
            let car = match self.stack.pop() {
                Some(val) => val,
                None => return None,
            };
            pair = Value::Pair(shared(Pair(car, pair)));
        }

        Some(pair)
    }

    fn pop_as_list_with(&mut self, count: usize, start: Option<Value>) -> Option<Value> {
        let mut pair = start.unwrap_or(Value::EmptyList);

        for _ in 0..count {
            let car = match self.stack.pop() {
                Some(val) => val,
                None => return None,
            };
            pair = Value::Pair(shared(Pair(car, pair)));
        }

        Some(pair)
    }

    fn push_list(&mut self, list: Value) -> usize {
        let mut len = 0;
        let mut pair = list;
        loop {
            let inner = match pair.pair() {
                Some(pair) => pair,
                None => return len,
            };

            let borrowed = inner.borrow();
            len += 1;
            self.stack.push(borrowed.0.clone());

            pair = borrowed.1.clone();
        }
    }
}

pub fn exec<P: Profiler>(
    bytecode: &[Instruction],
    environment: GcShared<Environment>,
    profiler: &mut P,
) -> Result<Value, ExecutionError> {
    use self::ExecutionError::*;

    let mut vm = VmState {
        environment,
        pc: 0,
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
            .or_else(|| bytecode.get(vm.pc))
            .ok_or(Internal("PC overflow"))?;

        debug!(
            "pc {:?}\tcode: {:?}\tdepth: {:?}\n\t stack: {:?}",
            vm.pc,
            instruction,
            vm.call_stack.len(),
            vm.stack
        );

        profiler.on_instruction_start(instruction.into());

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
            Instruction::Nil => {
                vm.stack.push(Value::Nil);
            }
            Instruction::String(ref s) => {
                vm.stack.push(Value::String(shared(s.into())));
            }
            Instruction::EmptyList => {
                vm.stack.push(Value::EmptyList);
            }
            Instruction::Integer(n) => {
                vm.stack.push(Value::Integer(n));
            }
            Instruction::Float(f) => {
                vm.stack.push(Value::Float(f));
            }
            Instruction::InvalidNumber => {
                return Err(ExecutionError::InvalidNumber);
            }
            Instruction::Pair => {
                let car = vm.stack.pop().expect("No car");
                let cdr = vm.stack.pop().expect("No cdr");
                let pair = Value::Pair(shared(Pair(car, cdr)));
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
                let pair = vm.pop_as_list(n + if improper { 2 } else { 0 }, improper)
                    .ok_or(Internal("Bad argc"))?;
                vm.stack.push(pair);
            }
            Instruction::Flatten => {
                let list = match vm.stack.pop() {
                    Some(pair @ Value::Pair { .. }) => pair,
                    Some(Value::EmptyList) => Value::EmptyList,
                    _ => Err(Internal("Bad arg type"))?,
                };

                let length = vm.push_list(list);
                vm.stack.push(Value::Integer(length as i32));
            }
            Instruction::DynList(n, improper) => {
                let mut value = if improper {
                    Some(vm.stack.pop().expect("cdr"))
                } else {
                    None
                };
                let mut with_car = false;
                for _ in 0..n {
                    let n_of_elements = match vm.stack.pop() {
                        Some(Value::Integer(k)) if k >= 0 => k,
                        _ => Err(Internal("Bad DynList header"))?,
                    };

                    if n_of_elements > 0 {
                        with_car = true;
                    }

                    let new_value = vm.pop_as_list_with(n_of_elements as usize, value)
                        .ok_or(Internal("Bad argc"))?;
                    value = Some(new_value);
                }

                if improper && !with_car {
                    Err(InvalidPair)?
                }

                vm.stack.push(value.unwrap_or(Value::EmptyList));
            }
            Instruction::DynVector(n) => {
                let mut value = VecDeque::new();
                for _ in 0..n {
                    let n_of_elements = match vm.stack.pop() {
                        Some(Value::Integer(k)) if k >= 0 => k,
                        _ => Err(Internal("Bad DynList header"))?,
                    };

                    for _ in 0..n_of_elements {
                        match vm.stack.pop() {
                            Some(v) => value.push_front(v),
                            None => Err(Internal("Bad argc"))?,
                        }
                    }
                }
                vm.stack.push(Value::Vector(shared(value.into())));
            }
            Instruction::Lambda { ref code, arity } => {
                let environment = vm.environment.clone();
                let procedure = Value::Procedure {
                    code: Rc::clone(code),
                    environment,
                    arity,
                };
                vm.stack.push(procedure);
            }

            Instruction::Promise(ref code) => {
                let promise = Value::Promise {
                    code: Rc::clone(code),
                    environment: vm.environment.clone(),
                };
                vm.stack.push(promise);
            }

            Instruction::Arity(n, rest) => match vm.stack.get(0) {
                Some(&Value::Procedure { arity: (m, r), .. }) if m == n && rest == r => {}
                Some(&Value::NativeProcedure(NativeProcedure { arity: (m, r), .. }))
                    if m == n && rest == r => {}
                _ => return Err(ExecutionError::BadArgType),
            },

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
                let value = vm.environment.borrow().get(var_name).ok_or(UnboundVar)?;
                vm.stack.push(value);
            }
            Instruction::DefineVar(ref var_name) => {
                let value = vm.stack.pop().expect("define var");
                vm.environment.borrow_mut().define(var_name.clone(), value);
            }
            Instruction::BranchUnless(n) => {
                let cond: bool = (&vm.stack.pop().expect("test")).into();
                if !cond {
                    vm.next_pc = Some(((vm.pc as isize) + n) as usize);
                }
            }
            Instruction::BranchIf(n) => {
                let cond: bool = (&vm.stack.pop().expect("test")).into();
                if cond {
                    vm.next_pc = Some(((vm.pc as isize) + n) as usize);
                }
            }
            Instruction::ROBranchUnless(n) => {
                let cond: bool = (&vm.stack[0]).into();
                if !cond {
                    vm.next_pc = Some(((vm.pc as isize) + n) as usize);
                }
            }
            Instruction::ROBranchIf(n) => {
                let cond: bool = (&vm.stack[0]).into();
                if cond {
                    vm.next_pc = Some(((vm.pc as isize) + n) as usize);
                }
            }
            Instruction::Branch(n) => {
                vm.next_pc = Some(((vm.pc as isize) + n) as usize);
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
                vm.environment = vm.environment.new_child();
            }
            Instruction::PopEnv => {
                let parent = vm.environment.borrow().parent.as_ref().unwrap().clone();
                vm.environment = parent;
            }
            Instruction::And => {
                let cond: bool = (&vm.stack[0]).into();

                if cond {
                    vm.stack.pop();
                } else {
                    vm.stack.remove(1);
                }
            }
            Instruction::Or => {
                let cond: bool = (&vm.stack[0]).into();
                if cond {
                    vm.stack.remove(1);
                } else {
                    vm.stack.pop();
                }
            }
            Instruction::Eq => {
                let value1 = vm.stack.pop().unwrap();
                let result = Value::Boolean(&value1 == vm.stack.get(0).unwrap());
                vm.stack.push(result);
            }
        }

        vm.pc = vm.next_pc.unwrap_or(vm.pc + 1);
        profiler.on_instruction_end(instruction.into());
    }

    vm.stack.pop().ok_or(Internal("Empty stack"))
}

pub fn default_env() -> GcShared<Environment> {
    let mut env = Environment::default();

    for &(name, fun, arity) in stdlib::STDLIB {
        env.define(
            name.into(),
            Value::NativeProcedure(NativeProcedure { fun, arity }),
        );
    }

    shared(env)
}
