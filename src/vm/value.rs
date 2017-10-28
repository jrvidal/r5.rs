use std::rc::Rc;
use gc::{Finalize, Trace};
use helpers::{CowString, ImmutableString};
use super::gc::GcShared;
use super::environment::Environment as GenericEnvironment;
use super::ExecutionError;
use compiler::Instruction;

pub type Environment = GenericEnvironment<Value>;


#[derive(Debug, Clone)]
pub enum Value {
    Scalar(Scalar),
    String(CowString),
    Number,
    Pair {
        car: GcShared<Value>,
        cdr: GcShared<Value>,
    },
    Vector(Vec<Value>),
    Procedure {
        code: Rc<Vec<Instruction>>,
        environment: GcShared<Environment>,
    },
    NativeProcedure(fn(Vec<Value>) -> Result<Value, ExecutionError>),
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
            Value::NativeProcedure(..) => {}
            Value::Environment(ref env) => mark(env),
            Value::ReturnRecord {
                ref environment, ..
            } => mark(environment),
            Value::Vector(ref vals) => for v in vals.iter() {
                mark(v);
            },
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
        debug!("to_repl: {:?}", self);
        match *self {
            Value::Scalar(Scalar::Nil) => "".to_owned(),
            Value::Scalar(Scalar::EmptyList) => "()".to_owned(),
            Value::Scalar(Scalar::Boolean(b)) => if b { "#t" } else { "#f" }.to_owned(),
            Value::Scalar(Scalar::Character(c)) => {
                let printed = match c {
                    '\n' => "newline".to_owned(),
                    ' ' => "space".to_owned(),
                    c => format!("{}", c),
                };

                "#\\".to_owned() + &printed
            }
            Value::Scalar(Scalar::Integer(n)) => format!("{}", n),
            Value::Scalar(Scalar::Symbol(ref s)) => format!("{}", *s),
            Value::String(ref s) => "\"".to_owned() + &escape(s.into()) + "\"",
            Value::Procedure { .. } => "<procedure>".to_owned(),
            Value::NativeProcedure(..) => "<procedure>".to_owned(),
            Value::Vector(ref vals) => {
                vals.iter()
                    .fold("#(".into(), |acc: String, val| acc + &val.to_repl())
                    + ")"
            }
            ref pair @ Value::Pair { .. } => format!("({})", pair_to_repl(&pair).1),
            ref v => format!("{:?}", v),
        }
    }
}


fn escape(s: String) -> String {
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
