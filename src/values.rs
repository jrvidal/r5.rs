use super::parser::{Body, LambdaFormals};
use gc::{GcObject as GenericGcObject, Heap as GcHeap, GcRef as GenericGcRef};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::borrow::Borrow;
use std::hash::Hash;

pub type Shared<T> = Rc<RefCell<T>>;
pub type Heap = GcHeap<RefObject>;
pub type GcObject = GenericGcObject<RefObject>;
// pub struct GcObject(GenericGcObject<RefObject>);
pub type GcRef<'a> = GenericGcRef<'a, RefObject>;
// pub struct GcRef<'a>(GenericGcRef<'a, RefObject>);

#[derive(Debug, Clone)]
pub enum Object {
    ValueObject(ValueObject),
    RefObject(GcObject),
}

#[derive(Debug)]
pub enum RefObject {
    Pair {
        car: Object,
        cdr: Object,
    },
    String(String),
    Vector(Vec<Object>),
    Procedure {
        body: Box<Body>,
        formals: LambdaFormals,
        environment: Environment
    }
}

// TO DO: ports?
#[derive(Clone, Debug)]
pub enum ValueObject {
    Boolean(bool),
    Number,
    Character(char),
    Symbol(String),
    EmptyList,
    NoValue
}


#[derive(Clone, Debug)]
pub struct Environment(Shared<InnerEnv>);

impl Environment {
    pub fn new(parent: Option<Environment>) -> Environment {
        let inner = InnerEnv {
            bindings: HashMap::new(),
            parent: parent.map(|env| env.0)
        };
        Environment(Rc::new(RefCell::new(inner)))
    }

    pub fn has<T>(&self, var_name: T) -> bool where T: Borrow<String>, T: Hash + Eq {
        let inner_cell : &RefCell<InnerEnv> = &*self.0;
        inner_cell.borrow().has(var_name)
    }


    pub fn get<T>(&self, var_name: T) -> Option<Object> where T: Borrow<String>, T: Hash + Eq {
        let inner_cell : &RefCell<InnerEnv> = &*self.0;
        inner_cell.borrow().get(var_name)
    }

    pub fn set_mut<T>(&mut self, var_name: T, value: Object, strict: bool) -> bool
    where T: Borrow<String>, T: Hash + Eq {
        let inner_cell : &RefCell<InnerEnv> = &*self.0;
        inner_cell.borrow_mut().set_mut(var_name, value, strict)
    }

    pub fn set<T>(&mut self, var_name: T, value: Object) where T: Borrow<String>, T: Hash + Eq {
        let inner_cell : &RefCell<InnerEnv> = &*self.0;
        inner_cell.borrow_mut().set(var_name, value)
    }
}

#[derive(Debug)]
pub struct InnerEnv {
    parent: Option<Shared<InnerEnv>>,
    bindings: HashMap<String, Object>
}

impl Heap {
    pub fn new() -> Heap {
        GcHeap::general_new()
    }

    pub fn insert_ref(&mut self, obj: RefObject) -> Object {
        Object::RefObject(self.insert(obj))
    }
}


impl Object {
    // pub fn new_ref(obj: RefObject, h: &mut Heap) -> Object {
    //     Object::RefObject(GcObject(h.insert(obj)))
    // }
    pub fn to_bool(&self) -> bool {
        match *self {
            Object::ValueObject(ValueObject::Boolean(false)) => false,
            _ => true
        }
    }

    pub fn to_repl(&self) -> String {
        match *self {
            Object::ValueObject(ValueObject::Boolean(true)) => "#t".to_string(),
            Object::ValueObject(ValueObject::Boolean(false)) => "#f".to_string(),
            Object::ValueObject(ValueObject::Number) => "<number>".to_string(),
            Object::ValueObject(ValueObject::Character(' ')) => "#\\space".to_string(),
            Object::ValueObject(ValueObject::Character('\n')) => "#\\newline".to_string(),
            Object::ValueObject(ValueObject::Character(c)) => "#\\".to_string() + &c.to_string(),
            Object::ValueObject(ValueObject::Symbol(ref s)) => s.clone(),
            Object::ValueObject(ValueObject::EmptyList) => "()".to_string(),
            Object::ValueObject(ValueObject::NoValue) => "".to_string(),
            // _ => "<unimplemented>".to_string()
            Object::RefObject(ref shared) => {
                match *shared.borrow() {
                    RefObject::Procedure{..} => "<procedure>".to_string(),
                    RefObject::Pair {
                        ref car,
                        ref cdr
                    } => {
                        "(".to_string() + &car.to_repl() + " . " + &cdr.to_repl() + ")"
                    },
                    _ => "<unimplemented>".to_string()
                }
            }
        }
    }
}

// impl Clone for Object {
//     fn clone(&self) -> Object {
//         match *self {
//             Object::ValueObject(ref v) => Object::ValueObject(v.clone()),
//             Object::RefObject(ref shared) => Object::RefObject({
//                 // let clone : GcObject = shared.clone();
//                 // clone
//                 shared
//             })
//         }
//     }
// }

impl InnerEnv {
    pub fn has<T>(&self, var_name: T) -> bool where T: Borrow<String>, T: Hash + Eq {
        if self.bindings.contains_key(var_name.borrow()) {
            return true;
        }

        let parent : &RefCell<InnerEnv> = match self.parent {
            Some(ref e) => e,
            None => return false
        };

        parent.borrow().has(var_name)
    }

    pub fn get<T>(&self, var_name: T) -> Option<Object> where T: Borrow<String>, T: Hash + Eq {
        self.bindings.get(var_name.borrow())
            .map(|val| { val.clone() })
            .or_else(|| {
                match self.parent {
                    None => None,
                    Some(ref parent) => {
                        let cell : &RefCell<InnerEnv> = &*parent;
                        cell.borrow().get(var_name)
                    }
                }
            })
    }

    pub fn set<T>(&mut self, var_name: T, value: Object) where T: Borrow<String>, T: Hash + Eq {
        self.bindings.insert(var_name.borrow().clone(), value);
    }

    pub fn set_mut<T>(&mut self, var_name: T, value: Object, strict: bool) -> bool
    where T: Borrow<String>, T: Hash + Eq {
        if self.bindings.contains_key(var_name.borrow()) || (self.parent.is_none() && !strict) {
            self.bindings.insert(var_name.borrow().clone(), value);
            return true;
        }

        match self.parent {
            Some(ref parent) => {
                let borrowed : &RefCell<InnerEnv> = parent.borrow();
                borrowed.borrow_mut().set_mut(var_name, value, strict)
            },
            None => !strict,
        }
    }
}

// use std::cell::{RefCell};
// use std::rc::Rc;
// use std::collections::HashMap;
// use std::borrow::Borrow;
// use gc::GcObject;


// // }

pub fn make_list(mut objects: Vec<Object>, heap: &mut Heap) -> Object {
    if objects.len() == 0 {
        return Object::ValueObject(ValueObject::EmptyList);
    }

    let mut last_pair = heap.insert_ref(RefObject::Pair {
        car: objects.pop().unwrap(),
        cdr: Object::ValueObject(ValueObject::EmptyList)
    });

    for obj in objects.into_iter().rev() {
        last_pair = heap.insert_ref(RefObject::Pair {
            car: obj,
            cdr: last_pair
        });
    }

    last_pair
}
