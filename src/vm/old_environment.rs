use std::collections::hash_map::HashMap;
use std::fmt::{Debug, Error as FmtError, Formatter};
use gc::{Finalize, Gc, GcCell, Trace};

use helpers::*;

pub type GcShared<T> = Gc<GcCell<T>>;

pub struct Environment<T: 'static> {
    parent: Option<GcShared<Environment<T>>>,
    bindings: HashMap<ImmutableString, T>,
}

use std::collections::hash_map;
#[derive(Debug)]
struct FmtEnvironment<'a, E, T: Debug + 'a> {
    parent: Option<&'a GcShared<E>>,
    bindings: hash_map::Keys<'a, ImmutableString, T>,
}

impl<T> Debug for Environment<T> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), FmtError> {
        FmtEnvironment {
            parent: self.parent.as_ref(),
            bindings: self.bindings.keys(),
        }.fmt(fmt)
    }
}

impl<T> Finalize for Environment<T> {}
unsafe impl<T> Trace for Environment<T> {
    custom_trace!(this, {
        if let Some(ref env) = this.parent {
            mark(env);
        }
        for (_, v) in this.bindings.iter() {
            mark(v);
        }
    });
}

impl<T> Newable for GcShared<Environment<T>> {
    fn new(&self) -> GcShared<Environment<T>> {
        shared(Environment {
            parent: Some(self.clone()),
            bindings: HashMap::new(),
        })
    }
}

pub fn null_env<T>() -> GcShared<Environment<T>> {
    shared(Environment {
        parent: None,
        bindings: HashMap::new(),
    })
}

trait Newable {
    fn new(&self) -> Self;
}

impl<T> Environment<T> {
    fn set(&mut self, name: ImmutableString, value: T) {
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

    fn define(&mut self, name: ImmutableString, value: T) {
        self.bindings.insert(name, value);
    }

    fn get(&self, name: ImmutableString) -> Option<T> {
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


fn shared<T: Trace>(x: T) -> GcShared<T> {
    Gc::new(GcCell::new(x))
}
