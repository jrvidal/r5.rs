use super::parser::{Body, LambdaFormals};
use super::reader::{Datum, AbbreviationKind};
use gc::{GcObject as GenericGcObject, Heap as GcHeap, GcRef as GenericGcRef};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::borrow::Borrow;
use std::hash::Hash;

pub type Shared<T> = Rc<RefCell<T>>;
pub type Heap = GcHeap<Reference>;
pub type GcObject = GenericGcObject<Reference>;
// pub struct GcObject(GenericGcObject<Reference>);
pub type GcRef<'a> = GenericGcRef<'a, Reference>;
// pub struct GcRef<'a>(GenericGcRef<'a, Reference>);

#[derive(Debug, Clone)]
pub enum Object {
    Scalar(Scalar),
    Reference(GcObject),
}

#[derive(Debug)]
pub enum Reference {
    Pair { car: Object, cdr: Object },
    String(String),
    Vector(Vec<Object>),
    Procedure {
        // body: Box<Body>,
        // formals: LambdaFormals,
        environment: Environment,
    },
}

// TO DO: ports?
#[derive(Clone, Debug)]
pub enum Scalar {
    Boolean(bool),
    Number,
    Character(char),
    Symbol(String),
    EmptyList,
    NoValue,
}


#[derive(Clone, Debug)]
pub struct Environment(Shared<InnerEnv>);

impl Environment {
    pub fn new(parent: Option<Environment>) -> Environment {
        let inner = InnerEnv {
            bindings: HashMap::new(),
            parent: parent.map(|env| env.0),
        };
        Environment(Rc::new(RefCell::new(inner)))
    }

    pub fn has<T>(&self, var_name: T) -> bool
        where T: Borrow<String>,
              T: Hash + Eq
    {
        let inner_cell: &RefCell<InnerEnv> = &*self.0;
        inner_cell.borrow().has(var_name)
    }


    pub fn get<T>(&self, var_name: T) -> Option<Object>
        where T: Borrow<String>,
              T: Hash + Eq
    {
        let inner_cell: &RefCell<InnerEnv> = &*self.0;
        inner_cell.borrow().get(var_name)
    }

    pub fn set_mut<T>(&mut self, var_name: T, value: Object, strict: bool) -> bool
        where T: Borrow<String>,
              T: Hash + Eq
    {
        let inner_cell: &RefCell<InnerEnv> = &*self.0;
        inner_cell.borrow_mut().set_mut(var_name, value, strict)
    }

    pub fn set<T>(&mut self, var_name: T, value: Object)
        where T: Borrow<String>,
              T: Hash + Eq
    {
        let inner_cell: &RefCell<InnerEnv> = &*self.0;
        inner_cell.borrow_mut().set(var_name, value)
    }
}

#[derive(Debug)]
pub struct InnerEnv {
    parent: Option<Shared<InnerEnv>>,
    bindings: HashMap<String, Object>,
}

impl Heap {
    pub fn new() -> Heap {
        GcHeap::general_new()
    }

    pub fn insert_ref(&mut self, obj: Reference) -> Object {
        Object::Reference(self.insert(obj))
    }
}


impl Object {
    // pub fn new_ref(obj: Reference, h: &mut Heap) -> Object {
    //     Object::Reference(GcObject(h.insert(obj)))
    // }
    pub fn to_bool(&self) -> bool {
        match *self {
            Object::Scalar(Scalar::Boolean(false)) => false,
            _ => true,
        }
    }

    pub fn to_repl(&self) -> String {
        let is_pair = match *self {
            Object::Reference(ref shared) => {
                match *shared.borrow() {
                    Reference::Pair { .. } => true,
                    _ => false,
                }
            }
            _ => false,
        };

        if is_pair {
            return "(".to_string() + &pair_to_repl(self).1 + ")";
        }

        match *self {
            Object::Scalar(Scalar::Boolean(true)) => "#t".to_string(),
            Object::Scalar(Scalar::Boolean(false)) => "#f".to_string(),
            Object::Scalar(Scalar::Number) => "<number>".to_string(),
            Object::Scalar(Scalar::Character(' ')) => "#\\space".to_string(),
            Object::Scalar(Scalar::Character('\n')) => "#\\newline".to_string(),
            Object::Scalar(Scalar::Character(c)) => "#\\".to_string() + &c.to_string(),
            Object::Scalar(Scalar::Symbol(ref s)) => s.clone(),
            Object::Scalar(Scalar::EmptyList) => "()".to_string(),
            Object::Scalar(Scalar::NoValue) => "".to_string(),
            Object::Reference(ref shared) => {
                match *shared.borrow() {
                    Reference::Procedure { .. } => "<procedure>".to_string(),
                    Reference::String(ref s) => "\"".to_string() + s + "\"",
                    _ => "<unimplemented>".to_string(),
                }
            }
        }
    }
}

fn pair_to_repl(object: &Object) -> (bool, String) {
    match *object {
        Object::Scalar(Scalar::EmptyList) => return (true, "".to_string()),
        Object::Reference(ref shared) => {
            match *shared.borrow() {
                Reference::Pair { ref car, ref cdr } => {
                    let (is_list, s) = pair_to_repl(cdr);
                    let result = car.to_repl() +
                                 if is_list {
                                     if s.len() > 0 { " " } else { "" }
                                 } else {
                                     " . "
                                 } + &s;

                    return (true, result);
                }
                _ => {}
            }
        }
        _ => {}
    };

    return (false, object.to_repl());
}

impl InnerEnv {
    pub fn has<T>(&self, var_name: T) -> bool
        where T: Borrow<String>,
              T: Hash + Eq
    {
        if self.bindings.contains_key(var_name.borrow()) {
            return true;
        }

        let parent: &RefCell<InnerEnv> = match self.parent {
            Some(ref e) => e,
            None => return false,
        };

        parent.borrow().has(var_name)
    }

    pub fn get<T>(&self, var_name: T) -> Option<Object>
        where T: Borrow<String>,
              T: Hash + Eq
    {
        self.bindings
            .get(var_name.borrow())
            .map(|val| val.clone())
            .or_else(|| match self.parent {
                         None => None,
                         Some(ref parent) => {
                let cell: &RefCell<InnerEnv> = &*parent;
                cell.borrow().get(var_name)
            }
                     })
    }

    pub fn set<T>(&mut self, var_name: T, value: Object)
        where T: Borrow<String>,
              T: Hash + Eq
    {
        self.bindings.insert(var_name.borrow().clone(), value);
    }

    pub fn set_mut<T>(&mut self, var_name: T, value: Object, strict: bool) -> bool
        where T: Borrow<String>,
              T: Hash + Eq
    {
        if self.bindings.contains_key(var_name.borrow()) || (self.parent.is_none() && !strict) {
            self.bindings.insert(var_name.borrow().clone(), value);
            return true;
        }

        match self.parent {
            Some(ref parent) => {
                let borrowed: &RefCell<InnerEnv> = parent.borrow();
                borrowed.borrow_mut().set_mut(var_name, value, strict)
            }
            None => !strict,
        }
    }
}

pub fn make_list(mut objects: Vec<Object>, heap: &mut Heap) -> Object {
    if objects.len() == 0 {
        return Object::Scalar(Scalar::EmptyList);
    }

    let mut last_pair = heap.insert_ref(Reference::Pair {
                                            car: objects.pop().unwrap(),
                                            cdr: Object::Scalar(Scalar::EmptyList),
                                        });

    for obj in objects.into_iter().rev() {
        last_pair = heap.insert_ref(Reference::Pair {
                                        car: obj,
                                        cdr: last_pair,
                                    });
    }

    last_pair
}

pub fn from_datum(datum: &Datum, heap: &mut Heap) -> Object {
    match *datum {
        Datum::Boolean(b) => Object::Scalar(Scalar::Boolean(b)),
        Datum::Number(_) => Object::Scalar(Scalar::Number),
        Datum::Character(c) => Object::Scalar(Scalar::Character(c)),
        Datum::String(ref s) => heap.insert_ref(Reference::String(s.clone())),
        Datum::Symbol(ref s) => Object::Scalar(Scalar::Symbol(s.clone())),
        Datum::List(ref datums) => {
            if datums.len() == 0 {
                return Object::Scalar(Scalar::EmptyList);
            }

            make_list(datums.iter().map(|dat| from_datum(dat, heap)).collect(),
                      heap)
        }
        Datum::Vector(ref datums) => {
            let objects = datums.iter().map(|dat| from_datum(dat, heap)).collect();
            heap.insert_ref(Reference::Vector(objects))
        }
        Datum::Pair { ref car, ref cdr } => {
            let last_cdr = from_datum(cdr, heap);
            let last_car = from_datum(car.back().unwrap(), heap);
            let mut result = heap.insert_ref(Reference::Pair {
                                                 car: last_car,
                                                 cdr: last_cdr,
                                             });

            for dat in car.iter().rev().skip(1) {
                let local_car = from_datum(dat, heap);
                result = heap.insert_ref(Reference::Pair {
                                             car: local_car,
                                             cdr: result,
                                         });
            }

            result
        }
        Datum::Abbreviation { kind: AbbreviationKind::Quote, ref datum } => {
            let obj = from_datum(datum, heap);
            let cdr = make_list(vec![obj], heap);
            heap.insert_ref(Reference::Pair {
                                car: Object::Scalar(Scalar::Symbol("quote".to_string())),
                                cdr: cdr,
                            })
        }
        _ => panic!("unimplemented from_datum"),
    }
}
