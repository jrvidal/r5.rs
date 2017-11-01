use gc::{Finalize, Trace};
use helpers::ImmutableString;
use super::gc::GcShared;
use super::environment::Environment as GenericEnvironment;
use super::{Branch, ExecutionError, VmState};

pub type Environment = GenericEnvironment<Value>;

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    EmptyList,
    Symbol(ImmutableString),
    Boolean(bool),
    Character(char),
    Integer(isize),
    Number,
    Vector(GcShared<Vec<Value>>),
    String(GcShared<String>),
    Procedure {
        code: Branch,
        environment: GcShared<Environment>,
        arity: (usize, bool),
    },
    Pair {
        car: GcShared<Value>,
        cdr: GcShared<Value>,
    },
    NativeProcedure(NativeProcedure),
}

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Value) -> bool {
        use self::Value::*;
        use self::NativeProcedure;
        match (self, other) {
            (&Nil, &Nil) | (&EmptyList, &EmptyList) => true,
            (&Number, &Number) => true,
            (&Boolean(x), &Boolean(y)) => x == y,
            (&Character(x), &Character(y)) => x == y,
            (&Symbol(ref x), &Symbol(ref y)) => *x == *y,
            (&Integer(x), &Integer(y)) => x == y,
            (&String(ref x), &String(ref y)) => x.borrow().as_ptr() == y.borrow().as_ptr(),
            (&Vector(ref x), &Vector(ref y)) => x.borrow().as_ptr() == y.borrow().as_ptr(),
            (
                &Pair { ref car, ref cdr },
                &Pair {
                    car: ref car2,
                    cdr: ref cdr2,
                },
            ) => {
                (&*car.borrow() as *const Value) == (&*car2.borrow() as *const Value)
                    && (&*cdr.borrow() as *const Value) == (&*cdr2.borrow() as *const Value)
            }
            (
                &Procedure {
                    ref environment,
                    ref code,
                    arity,
                },
                &Procedure {
                    environment: ref environment2,
                    code: ref code2,
                    arity: arity2,
                },
            ) => {
                arity == arity2
                    && (&*environment.borrow() as *const Environment)
                        == (&*environment2.borrow() as *const Environment)
                    && code.as_ptr() == code2.as_ptr()
            }
            (
                &NativeProcedure(NativeProcedure { fun, arity }),
                &NativeProcedure(NativeProcedure {
                    fun: fun2,
                    arity: arity2,
                }),
            ) => arity == arity2 && (fun as *const NatFn) == (fun2 as *const NatFn),
            _ => false,
        }
    }
}

impl Finalize for Value {}
unsafe impl Trace for Value {
    custom_trace!(this, {
        use self::Value::*;
        match *this {
            Vector(ref vec) => mark(vec),
            String(ref s) => mark(s),
            Procedure {
                ref environment, ..
            } => mark(environment),
            Pair { ref car, ref cdr } => {
                mark(car);
                mark(cdr);
            }
            Nil |
            EmptyList |
            Number |
            Symbol(_) |
            Boolean(_) |
            Character(_) |
            Integer(_) |
            NativeProcedure(_) => {}
        }
    });
}


type CallInfo = (usize, bool);
type NatFn = fn(&mut VmState, CallInfo, &Option<Branch>)
    -> Result<(), ExecutionError>;


#[derive(Clone)]
pub struct NativeProcedure {
    pub(super) fun: NatFn,
    pub(super) arity: (usize, bool),
}

use std::fmt::{Debug, Formatter, Result as FmtResult};
impl Debug for NativeProcedure {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "NativeProcedure{:?}", self.arity)
    }
}

impl<'a> From<&'a Value> for bool {
    fn from(v: &Value) -> bool {
        match *v {
            Value::Boolean(false) => false,
            _ => true,
        }
    }
}

impl Value {
    pub fn to_repl(&self) -> String {
        debug!("to_repl: {:?}", self);
        match *self {
            Value::Nil => "".to_owned(),
            Value::EmptyList => "()".to_owned(),
            Value::Boolean(b) => if b { "#t" } else { "#f" }.to_owned(),
            Value::Character(c) => {
                let printed = match c {
                    '\n' => "newline".to_owned(),
                    ' ' => "space".to_owned(),
                    c => format!("{}", c),
                };

                "#\\".to_owned() + &printed
            }
            Value::Integer(n) => format!("{}", n),
            Value::Symbol(ref s) => format!("{}", *s),
            Value::String(ref s) => "\"".to_owned() + &escape(&*s.borrow()) + "\"",
            Value::Procedure { .. } => "<procedure>".to_owned(),
            Value::NativeProcedure { .. } => "<procedure>".to_owned(),
            Value::Vector(ref vals) => {
                vals.borrow()
                    .iter()
                    .fold("#(".into(), |acc: String, val| acc + &val.to_repl())
                    + ")"
            }
            ref pair @ Value::Pair { .. } => format!("({})", pair_to_repl(&pair).1),
            ref v => format!("{:?}", v),
        }
    }

    pub fn is_list(&self) -> bool {
        match *self {
            Value::Pair { ref cdr, .. } => cdr.borrow().is_list(),
            Value::EmptyList => true,
            _ => false,
        }
    }

    pub fn list_len(&self) -> Option<usize> {
        match *self {
            Value::EmptyList => Some(0),
            Value::Pair { ref cdr, .. } => cdr.borrow().list_len().map(|l| l + 1),
            _ => None,
        }
    }

    pub fn pair(&self) -> Option<(&GcShared<Value>, &GcShared<Value>)> {
        match *self {
            Value::Pair { ref car, ref cdr } => Some((car, cdr)),
            _ => None,
        }
    }
}


fn escape(s: &str) -> String {
    s.chars()
        .flat_map(|c| match c {
            '"' => vec!['\\', '"'],
            '\\' => vec!['\\', '\\'],
            c => vec![c],
        })
        .collect()
}

fn pair_to_repl(value: &Value) -> (bool, String) {
    match *value {
        Value::EmptyList => return (true, "".to_string()),
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
