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
    Integer(i32),
    Float(f32),
    InvalidNumber,
    Boolean(bool),
    Character(char),
    Vector(GcShared<Vec<Value>>),
    String(GcShared<String>),
    Procedure {
        code: Branch,
        environment: GcShared<Environment>,
        arity: (usize, bool),
    },
    Pair(GcShared<Pair<Value>>),
    NativeProcedure(NativeProcedure),
    Promise {
        code: Branch,
        environment: GcShared<Environment>,
    },
}

#[derive(Debug, Clone)]
pub struct Pair<T>(pub T, pub T);

impl<T> Finalize for Pair<T> {}

unsafe impl<T: Trace> Trace for Pair<T> {
    custom_trace!(this, {
        mark(&this.0);
        mark(&this.1);
    });
}


// This PartialEq implementation corresponds to the native eqv? procedure
// We've chosen a "hard" implementation where we compare pointers when possible
// without looking the contents of reference types
impl PartialEq<Value> for Value {
    fn eq(&self, other: &Value) -> bool {
        use self::Value::*;
        use self::NativeProcedure;

        match (self, other) {
            (&Nil, &Nil) | (&EmptyList, &EmptyList) => true,
            (&Float(f), &Float(g)) => f == g,
            (&Integer(n), &Integer(m)) => m == n,
            (&Boolean(x), &Boolean(y)) => x == y,
            (&Character(x), &Character(y)) => x == y,
            (&Symbol(ref x), &Symbol(ref y)) => *x == *y,
            (&String(ref x), &String(ref y)) => x.borrow().as_ptr() == y.borrow().as_ptr(),
            (&Vector(ref x), &Vector(ref y)) => x.borrow().as_ptr() == y.borrow().as_ptr(),
            (&Value::Pair(ref pair), &Value::Pair(ref pair2)) => {
                use self::Pair;
                let borrow = pair.borrow();
                let borrow2 = pair2.borrow();
                &*borrow as *const Pair<Value> == &*borrow2 as *const Pair<Value>
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
            (
                &Promise {
                    ref code,
                    ref environment,
                },
                &Promise {
                    code: ref code2,
                    environment: ref environment2,
                },
            ) => {
                (&*environment.borrow() as *const Environment)
                    == (&*environment2.borrow() as *const Environment)
                    && code.as_ptr() == code2.as_ptr()
            }
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
            Pair(ref pair) => mark(pair),
            Promise {
                ref environment, ..
            } => mark(environment),
            Nil |
            EmptyList |
            Symbol(_) |
            Boolean(_) |
            Character(_) |
            NativeProcedure(_) |
            Integer(_) |
            Float(_) |
            InvalidNumber => {}
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

macro_rules! simple_type {
    ($name:ident, $var:pat) => (
        pub fn $name(&self) -> bool {
            match *self {
                $var => true,
                _ => false
            }
        }
    );
    ($name:ident, $var:pat, $var2:pat) => (
        pub fn $name(&self) -> bool {
            match *self {
                $var | $var2 => true,
                _ => false
            }
        }
    )
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
            Value::Symbol(ref s) => format!("{}", *s),
            Value::String(ref s) => "\"".to_owned() + &escape(&*s.borrow()) + "\"",
            Value::Procedure { .. } | Value::NativeProcedure { .. } => "<procedure>".to_owned(),
            Value::Vector(ref vals) => {
                vals.borrow()
                    .iter()
                    .fold("#(".into(), |acc: String, val| acc + &val.to_repl())
                    + ")"
            }
            ref pair @ Value::Pair { .. } => format!("({})", pair_to_repl(pair).1),
            Value::Promise { .. } => "<promise>".to_owned(),
            Value::Integer(n) => format!("{}", n),
            Value::Float(f) => format!("{}", f),
            ref v => format!("{:?}", v),
        }
    }

    pub fn is_list(&self) -> bool {
        match *self {
            Value::Pair(ref pair) => pair.borrow().1.is_list(),
            Value::EmptyList => true,
            _ => false,
        }
    }


    #[cfg_attr(rustfmt, rustfmt_skip)]
    simple_type![is_vector, Value::Vector(..)];
    #[cfg_attr(rustfmt, rustfmt_skip)]
    simple_type![is_symbol, Value::Symbol(..)];
    simple_type![is_procedure, Value::Procedure{..}, Value::NativeProcedure(..)];
    #[cfg_attr(rustfmt, rustfmt_skip)]
    simple_type![is_string, Value::String(..)];
    #[cfg_attr(rustfmt, rustfmt_skip)]
    simple_type![is_char, Value::Character(..)];
    #[cfg_attr(rustfmt, rustfmt_skip)]
    simple_type![is_boolean, Value::Boolean(..)];
    #[cfg_attr(rustfmt, rustfmt_skip)]
    simple_type![is_null, Value::EmptyList];
    #[cfg_attr(rustfmt, rustfmt_skip)]
    simple_type![is_pair, Value::Pair(..)];
    simple_type![is_number, Value::Integer(_), Value::Float(_)];

    pub fn list_len(&self) -> Option<usize> {
        match *self {
            Value::EmptyList => Some(0),
            Value::Pair(ref pair) => pair.borrow().1.list_len().map(|l| l + 1),
            _ => None,
        }
    }

    pub fn pair(&self) -> Option<GcShared<Pair<Value>>> {
        match *self {
            Value::Pair(ref pair) => Some(pair.clone()),
            _ => None,
        }
    }
}

pub trait DeepEqual {
    fn equal(&self, other: &Self) -> bool;
}

impl DeepEqual for Value {
    fn equal(&self, other: &Value) -> bool {
        self == other || match (self, other) {
            (&Value::String(ref s), &Value::String(ref s2)) => s == s2,
            (&Value::Vector(ref v), &Value::Vector(ref v2)) => {
                let vec = &*v.borrow();
                let vec2 = &*v2.borrow();
                vec.len() == vec2.len()
                    && vec.iter()
                        .zip(vec2.iter())
                        .all(|(val, val2)| val.equal(val2))
            }
            (&Value::Pair(ref pair), &Value::Pair(ref pair2)) => {
                let borrowed = pair.borrow();
                let borrowed2 = pair2.borrow();
                borrowed.0.equal(&borrowed2.0) && borrowed.1.equal(&borrowed2.1)
            }
            _ => false,
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
        Value::Pair(ref pair) => {
            let (is_list, s) = pair_to_repl(&pair.borrow().1);
            let result = (&pair.borrow().0).to_repl() + if is_list {
                if !s.is_empty() {
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

    (false, value.to_repl())
}
