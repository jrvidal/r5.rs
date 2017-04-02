use gc::{Gc, GcCell, Trace, Finalize};
use ::helpers::{ImmutableString, CowString};
use std::collections::{HashMap, VecDeque};
use ::compiler::Instruction;
use std::mem;
// use gc::{GcObject as GenericGcObject, Heap as GcHeap};

// pub type Heap = GcHeap<Reference>;
// pub type GcObject = GenericGcObject<Reference>;

pub type GcShared<T> = Gc<GcCell<T>>;

#[derive(Debug, Clone)]
pub enum Value {
    Scalar(Scalar),
    String(CowString),
    Pair { car: GcShared<Value>, cdr: GcShared<Value> },
    Procedure {
        address: usize,
        environment: GcShared<Environment>
    },
    Environment(GcShared<Environment>),
    ReturnRecord(GcShared<Environment>, usize),
}

impl Finalize for Value {}
unsafe impl Trace for Value {
    custom_trace!(this, {
        match *this {
            Value::Scalar(_) => {},
            Value::Pair { ref car, ref cdr } => {
                mark(car);
                mark(cdr);
            },
            Value::Procedure  { ref environment, .. } => {
                mark(environment);
            },
            Value::Environment(ref env) => mark(env),
            Value::ReturnRecord(ref env, ..) => mark(env),
            Value::String(_) => {},
        }
    });
}

#[derive(Debug, Clone)]
pub enum Scalar {
    Nil,
    // Symbol(ImmutableString),
    Boolean(bool),
    Character(char),
}


#[derive(Debug)]
pub struct Environment {
    parent: Option<GcShared<Environment>>,
    bindings: HashMap<ImmutableString, Value>
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
            bindings: HashMap::new()
        })
    }
}

pub fn null_env() -> GcShared<Environment> {
    shared(Environment {
        parent: None,
        bindings: HashMap::new()
    })
}

trait Newable where Self: Sized {
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
    fn swap_remove_front(&mut self, index: usize) -> Option<T> {
        self.0.swap_remove_front(index)
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


pub fn exec(bytecode: Vec<Instruction>, environment: GcShared<Environment>, offset: usize) -> Result<Value, ()> {
    let mut pc = offset;
    // let mut ra = 0;
    let mut stack = Stack::new();
    let mut current_env = environment;

    loop {
        if pc >= bytecode.len() {
            break;
        }
        let ref code = bytecode[pc];
        println!("pc {:?}, code: {:?}, stack: {:?}", pc, code, stack);

        match *code {
            Instruction::Character(c) => {
                stack.push_front(Value::Scalar(Scalar::Character(c))); 
            },
            Instruction::Boolean(b) => {
                stack.push_front(Value::Scalar(Scalar::Boolean(b))); 
            },
            Instruction::Nil => {
                stack.push_front(Value::Scalar(Scalar::Nil)); 
            },
            Instruction::String(ref s) => {
                stack.push_front(Value::String(s.clone())); 
            },
            // Instruction::Pop => { stack.pop_front(); },
            Instruction::Pair => {
                let car = stack.pop_front().expect("No car");
                let cdr = stack.pop_front().expect("No cdr");
                let pair = Value::Pair { car: shared(car), cdr: shared(cdr) };
                stack.push_front(pair);
            },
            Instruction::Lambda(lambda_idx) => {
                let environment = current_env.clone();
                let procedure = Value::Procedure {
                    address: lambda_idx,
                    environment
                };
                stack.push_front(procedure);
            },
            Instruction::Call(n_of_args) => {
                // TODO: tail call
                let val = stack.pop_front().ok_or_else(|| ())?;
                let (address, environment) = if let Value::Procedure { address, environment } = val {
                    (address, environment)
                } else {
                    return Err(());
                };

                let environment = environment.new();

                // TODO: arity

                stack.insert(n_of_args, Value::ReturnRecord(current_env, pc + 1));

                current_env = environment;
                pc = address;

                continue;
            },
            Instruction::SetVar(ref var_name) => {
                let value = stack.pop_front().unwrap();
                current_env.borrow_mut().set(var_name.clone(), value.clone());
                stack.push_front(Value::Scalar(Scalar::Nil));
            },
            Instruction::LoadVar(ref var_name) => {
                let value = current_env.borrow().get(var_name.clone()).ok_or_else(|| ())?;
                stack.push_front(value);
            },
            Instruction::SetVar(ref var_name) => {
                let value = stack.pop_front().expect("set var");
                current_env.borrow_mut().set(var_name.clone(), value);
            },
            Instruction::BranchUnless(n) => {
                let value = stack.pop_front().expect("test");
                if let Value::Scalar(Scalar::Boolean(false)) = value {
                    pc += n;
                    continue;
                }
            },
            Instruction::Branch(n) => {
                pc += n;
                continue;
            },
            Instruction::Ret => {
                let return_record = stack.swap_remove_front(1).expect("return_record");

                let (ra, environment) = if let Value::ReturnRecord(environment, ra) = return_record {
                    (ra, environment)
                } else {
                    return Err(());
                };

                current_env = environment;
                pc = ra;
                continue;
            },
            _ => panic!("unhandled bytecode {:?}", code),
        }


        pc += 1;

        if bytecode.len() == pc {
            break;
        }
    }

    Ok(stack.pop_front().expect("empty stack"))
}

// type Heap = HashMap<GcId,>

// use parser::{Expression, LambdaFormals, Definition, Derived, Binding};
// use helpers::*;
// use values::*;

// const NO_VALUE: Object = Object::ValueObject(ValueObject::NoValue);
// const TRUE: Object = Object::ValueObject(ValueObject::Boolean(true));
// const FALSE: Object = Object::ValueObject(ValueObject::Boolean(false));

// #[derive(Debug)]
// pub enum EvalError {
//     NonProcedure,
//     UnboundVar(String),
//     ArgumentCount,
// }

// pub fn eval(expression: &Expression,
//             mut environment: &mut Environment,
//             heap: &mut Heap)
//             -> Result<Object, EvalError> {
//     match *expression {
//         Expression::Boolean(b) => Ok(Object::ValueObject(ValueObject::Boolean(b))),
//         Expression::Character(c) => Ok(Object::ValueObject(ValueObject::Character(c))),
//         // FIXME: remove clone
//         Expression::String(ref s) => Ok(heap.insert_ref(RefObject::String(s.clone()))),
//         Expression::Number(_) => Ok(Object::ValueObject(ValueObject::Number)),

//         Expression::Variable(ref s) => {
//             environment.get(s).ok_or_else(|| EvalError::UnboundVar(s.clone()))
//         }

//         Expression::Assignment { variable: ref s, expression: ref exp } => {
//             eval(exp, environment, heap).and_then(|val| {
//                 let assigned = environment.set_mut(s, val, false);
//                 if assigned {
//                     Ok(NO_VALUE)
//                 } else {
//                     Err(EvalError::UnboundVar(s.clone()))
//                 }
//             })
//         }

//         Expression::Quotation(ref datum) => Ok(from_datum(datum, heap)),

//         Expression::Conditional { ref test, ref consequent, ref alternate } => {
//             let test_value = try![ eval(test, environment, heap) ].to_bool();

//             if test_value {
//                 eval(consequent, environment, heap)
//             } else {
//                 alternate.as_ref()
//                     .map(|alt_exp| eval(alt_exp, environment, heap))
//                     .unwrap_or(Ok(NO_VALUE))
//             }
//         }
//         Expression::Derived(Derived::And(ref expressions)) => {
//             if expressions.len() == 0 {
//                 return Ok(TRUE);
//             }

//             for exp in expressions.iter() {
//                 if !try![ eval(exp, environment, heap) ].to_bool() {
//                     return Ok(FALSE);
//                 }
//             }

//             Ok(TRUE)
//         }
//         Expression::Derived(Derived::Or(ref expressions)) => {
//             if expressions.len() == 0 {
//                 return Ok(FALSE);
//             }

//             for exp in expressions.iter() {
//                 if try![ eval(exp, environment, heap) ].to_bool() {
//                     return Ok(TRUE);
//                 }
//             }

//             Ok(FALSE)
//         }
//         Expression::Derived(Derived::Let { ref bindings, ref body }) => {
//             let mut let_env = Environment::new(Some(environment.clone()));

//             let bounded: Vec<(&String, Object)> = try![ bindings.iter().map(|&Binding {
//                 variable: ref var_name,
//                 init: ref exp
//             }| {
//                 (Ok(var_name), eval(exp, environment, heap)).result()
//             }).collect() ];

//             for (var_name, obj) in bounded.into_iter() {
//                 let_env.set(var_name, obj);
//             }

//             eval(&body.expression, &mut let_env, heap)
//         }
//         Expression::Lambda { ref formals, ref body } => {
//             Ok(heap.insert_ref(RefObject::Procedure {
//                                    environment: environment.clone(),
//                                    body: Box::new(body.clone()),
//                                    formals: formals.clone(),
//                                }))
//         }
//         Expression::Call { operator: ref fun, operands: ref args } => {
//             let evald_proc = try![ eval(fun, environment, heap) ];

//             let proc_ref = match evald_proc {
//                 Object::RefObject(shared) => shared,
//                 _ => return Err(EvalError::NonProcedure),
//             };

//             let borrowed_proc = proc_ref.borrow();

//             let (body, formals, env) = match *borrowed_proc {
//                 RefObject::Procedure { ref body, ref formals, environment: ref env } => {
//                     (body, formals, env)
//                 }
//                 _ => return Err(EvalError::NonProcedure),
//             };


//             // TO DO: evaluation order...
//             let evald_args: Vec<Object> = try![ args.into_iter()
//                 .map(|arg| {
//                     eval(&arg, environment, heap)
//                 }).collect() ];

//             let mut call_env = try![ build_call_env(formals, evald_args, env, heap) ];

//             for def in body.definitions.iter() {
//                 try![ eval_definition(def, &mut call_env, heap) ];
//             }

//             for exp in body.commands.iter() {
//                 try![ eval(exp, &mut call_env, heap) ];
//             }

//             eval(&body.expression, &mut call_env, heap)
//         }
//         _ => panic!("unimplemented expression type for interpreter!"),
//     }
// }

// fn eval_definition(definition: &Definition,
//                    environment: &mut Environment,
//                    heap: &mut Heap)
//                    -> Result<(), EvalError> {
//     match *definition {
//         Definition::Define { variable: ref var_name, ref expression } => {
//             eval(expression, environment, heap).map(|object| { environment.set(var_name, object); })
//         }
//         Definition::DefineLambda { variable: ref var_name, ref formals, ref body } => {
//             let env = environment.clone();
//             environment.set(var_name,
//                             heap.insert_ref(RefObject::Procedure {
//                                                 body: Box::new(body.clone()),
//                                                 formals: formals.clone(),
//                                                 environment: env,
//                                             }));
//             Ok(())
//         }
//         Definition::Begin(ref definitions) => {
//             for def in definitions.iter() {
//                 try![eval_definition(def, environment, heap)];
//             }
//             Ok(())
//         }
//     }
// }

// fn build_call_env(formals: &LambdaFormals,
//                   mut arguments: Vec<Object>,
//                   local_env: &Environment,
//                   heap: &mut Heap)
//                   -> Result<Environment, EvalError> {
//     let mut call_env = Environment::new(Some(local_env.clone()));

//     match *formals {
//         LambdaFormals::List(ref formal_list) => {
//             if formal_list.len() != arguments.len() {
//                 return Err(EvalError::ArgumentCount);
//             }
//             for (name, arg_value) in formal_list.iter().zip(arguments.into_iter()) {
//                 call_env.set(name, arg_value);
//             }
//         }
//         LambdaFormals::Rest(ref formal_list, ref rest_param) => {
//             if formal_list.len() > arguments.len() {
//                 return Err(EvalError::ArgumentCount);
//             }

//             let rest_args = arguments.split_off(formal_list.len());

//             for (name, arg_value) in formal_list.iter().zip(arguments.into_iter()) {
//                 call_env.set(name, arg_value);
//             }

//             call_env.set(rest_param, make_list(rest_args, heap));
//         }
//         LambdaFormals::VarArgs(ref rest_param) => {
//             call_env.set(rest_param, make_list(arguments, heap));
//         }
//     }

//     Ok(call_env)
// }
