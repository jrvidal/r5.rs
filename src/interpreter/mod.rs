use std::collections::{HashMap, VecDeque};
use std::rc::Rc;
use gc::{Finalize, Gc, GcCell, Trace};
use std::fmt::{Debug, Error as FmtError, Formatter};

use helpers::{CowString, ImmutableString};
use compiler::Instruction;

pub type GcShared<T> = Gc<GcCell<T>>;

#[derive(Debug, Clone)]
pub enum Value {
    Scalar(Scalar),
    String(CowString),
    Number,
    Pair {
        car: GcShared<Value>,
        cdr: GcShared<Value>,
    },
    Procedure {
        code: Rc<Vec<Instruction>>,
        environment: GcShared<Environment>,
    },
    Environment(GcShared<Environment>),
    ReturnRecord {
        environment: GcShared<Environment>,
        address: usize,
        code: Option<Rc<Vec<Instruction>>>,
    },
}

impl Finalize for Value {}
unsafe impl Trace for Value {
    custom_trace!(this, {
        match *this {
            Value::Scalar(_) => {}
            Value::Pair { ref car, ref cdr } => {
                mark(car);
                mark(cdr);
            }
            Value::Procedure {
                ref environment, ..
            } => {
                mark(environment);
            }
            Value::Environment(ref env) => mark(env),
            Value::ReturnRecord {
                ref environment, ..
            } => mark(environment),
            Value::String(_) => {}
            Value::Number => {}
        }
    });
}

impl<'a> From<&'a Value> for bool {
    fn from(v: &Value) -> bool {
        match *v {
            Value::Scalar(Scalar::Boolean(false)) => false,
            _ => true,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Scalar {
    Nil,
    EmptyList,
    Symbol(ImmutableString),
    Boolean(bool),
    Character(char),
    Integer(isize),
}

impl Value {
    pub fn to_repl(&self) -> String {
        println!("to_repl: {:?}", self);
        match *self {
            Value::Scalar(Scalar::Nil) => "".to_owned(),
            Value::Scalar(Scalar::EmptyList) => "()".to_owned(),
            Value::Scalar(Scalar::Boolean(b)) => if b { "#t".to_owned() } else { "#f".to_owned() },
            Value::Scalar(Scalar::Character(c)) => {
                let printed = match c {
                    '\n' => "newline".to_owned(),
                    ' ' => "space".to_owned(),
                    c => format!("{}", c)
                };

                "#\\".to_owned() + &printed
            },
            Value::Scalar(Scalar::Integer(n)) => format!("{}", n),
            Value::Scalar(Scalar::Symbol(ref s)) => format!("{}", *s),
            Value::String(ref s) => "\"".to_owned() + &escape(s.into()) + "\"",
            Value::Procedure { .. } => "<procedure>".to_owned(),
            ref pair @ Value::Pair { .. } => format!("({})", pair_to_repl(&pair).1),
            ref v => format!("{:?}", v),
        }
    }
}

fn escape(s: String) -> String {
    s.chars()
        .flat_map(|c| {
            match c {
                '"' => vec!['\\', '"'],
                '\\' => vec!['\\', '\\'],
                c => vec![c]
            }
        })
        .collect()

}

fn pair_to_repl(value: &Value) -> (bool, String) {
    match *value {
        Value::Scalar(Scalar::EmptyList) => return (true, "".to_string()),
        Value::Pair { ref car, ref cdr } => {
            let (is_list, s) = pair_to_repl(&*cdr.borrow());
            let result = (&*car.borrow()).to_repl() + if is_list {
                if s.len() > 0 {
                    " "
                } else {
                    ""
                }
            } else {
                " . "
            } + &s;

            return (true, result);
        }
        _ => {}
    };

    return (false, value.to_repl());
}


pub struct Environment {
    parent: Option<GcShared<Environment>>,
    bindings: HashMap<ImmutableString, Value>,
}

use std::collections::hash_map;
#[derive(Debug)]
struct FmtEnvironment<'a, T: Debug + 'a> {
    parent: Option<&'a GcShared<Environment>>,
    bindings: hash_map::Keys<'a, ImmutableString, T>,
}

impl Debug for Environment {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), FmtError> {
        FmtEnvironment {
            parent: self.parent.as_ref(),
            bindings: self.bindings.keys(),
        }.fmt(fmt)
    }
}

impl Finalize for Environment {}
unsafe impl Trace for Environment {
    custom_trace!(this, {
        if let Some(ref env) = this.parent {
            mark(env);
        }
        for (_, v) in this.bindings.iter() {
            mark(v);
        }
    });
}

fn shared<T: Trace>(x: T) -> GcShared<T> {
    Gc::new(GcCell::new(x))
}

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

impl Environment {
    fn set(&mut self, name: ImmutableString, value: Value) {
        use std::collections::hash_map::Entry;

        if let Entry::Occupied(mut occ) = self.bindings.entry(name.clone()) {
            *occ.get_mut() = value;
            return;
        }

        if self.parent.is_none() {
            self.bindings.insert(name, value);
            return;
        }

        let mut env = self.parent.clone().unwrap();

        loop {
            env = {
                let mut envref = env.borrow_mut();
                if let Entry::Occupied(mut occ) = envref.bindings.entry(name.clone()) {
                    *occ.get_mut() = value;
                    return;
                }

                if envref.parent.is_none() {
                    envref.bindings.insert(name, value);
                    return;
                }
                envref.parent.clone().unwrap()
            }
        }
    }

    fn define(&mut self, name: ImmutableString, value: Value) {
        self.bindings.insert(name, value);
    }

    fn get(&self, name: ImmutableString) -> Option<Value> {
        if self.bindings.contains_key(&name) {
            return self.bindings.get(&name).cloned();
        } else if self.parent.is_none() {
            return None;
        }
        let mut environment = self.parent.clone().unwrap();
        loop {
            environment = {
                let borrowed = environment.borrow();
                if borrowed.bindings.contains_key(&name) {
                    return borrowed.bindings.get(&name).cloned();
                } else if borrowed.parent.is_none() {
                    return None;
                }
                borrowed.parent.clone().unwrap()
            }
        }


    }
}

#[derive(Debug, Clone)]
struct Stack<T>(VecDeque<T>);

impl<T> Stack<T> {
    fn new() -> Stack<T> {
        Stack(VecDeque::new())
    }
    fn push_front(&mut self, it: T) {
        self.0.push_front(it);
    }
    fn pop_front(&mut self) -> Option<T> {
        self.0.pop_front()
    }
    fn insert(&mut self, index: usize, it: T) {
        self.0.insert(index, it)
    }
    fn get(&mut self, index: usize) -> Option<&T> {
        self.0.get(index)
    }
    fn remove(&mut self, index: usize) -> Option<T> {
        self.0.remove(index)
    }
    fn swap_remove_front(&mut self, index: usize) -> Option<T> {
        self.0.swap_remove_front(index)
    }
}

use std::ops::Index;
impl<T> Index<usize> for Stack<T> {
    type Output = T;
    fn index(&self, i: usize) -> &T {
        &self.0[i]
    }
}

impl<T: Finalize> Finalize for Stack<T> {}

unsafe impl<T: Trace> Trace for Stack<T> {
    custom_trace!(this, {
        for x in this.0.iter() {
            mark(x);
        }
    });
}


pub fn exec(bytecode: Vec<Instruction>, environment: GcShared<Environment>) -> Result<Value, ()> {
    let mut pc = 0;
    let mut stack = Stack::new();
    let mut current_env = environment;
    let mut compiled_code: Option<Rc<Vec<Instruction>>> = None;
    let mut next_compiled_code = None;

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
        println!("pc {:?}\tcode: {:?}\n\t stack: {:?}", pc, instruction, stack);
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
            Instruction::List(0) => {
                stack.push_front(Value::Scalar(Scalar::EmptyList));
            }
            Instruction::List(n) => {
                let mut pair = Value::Pair {
                    car: shared(stack.pop_front().unwrap()),
                    cdr: shared(Value::Scalar(Scalar::EmptyList)),
                };

                for _ in 0..n - 1 {
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
                let val = stack.swap_remove_front(1).ok_or_else(|| ())?;
                let (code, environment) = if let Value::Procedure { code, environment } = val {
                    (code, environment)
                } else {
                    return Err(());
                };


                let environment = environment.new();

                if !tail {
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

                if !rest && n_of_args > 0 && n_of_args == args_passed {
                    pc += 1;
                    continue;
                 } else if n_of_args > 0 && args_passed < n_of_args {
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
                let value = current_env
                    .borrow()
                    .get(var_name.clone())
                    .ok_or_else(|| ())?;
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

        if bytecode.len() == pc {
            break;
        }
    }

    Ok(stack.pop_front().expect("empty stack"))
}
