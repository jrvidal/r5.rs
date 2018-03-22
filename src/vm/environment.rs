use std::fmt::{Debug, Error as FmtError, Formatter};
use std::collections::hash_map;
use gc::{Finalize, Trace};

use helpers::ImmutableString;
use std::collections::HashMap;
use super::GcShared;

pub struct Environment<V: Trace + 'static> {
    pub(super) parent: Option<GcShared<Environment<V>>>,
    pub(super) bindings: HashMap<ImmutableString, V>,
}

impl<V: Trace> Default for Environment<V> {
    fn default() -> Environment<V> {
        Environment {
            parent: None,
            bindings: HashMap::new(),
        }
    }
}

#[derive(Debug)]
struct FmtEnvironment<'a, V: Trace + Debug + 'static> {
    parent: Option<&'a GcShared<Environment<V>>>,
    bindings: hash_map::Keys<'a, ImmutableString, V>,
}

impl<V: Trace + Debug> Debug for Environment<V> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), FmtError> {
        FmtEnvironment {
            parent: self.parent.as_ref(),
            bindings: self.bindings.keys(),
        }.fmt(fmt)
    }
}

impl<V: Trace> Finalize for Environment<V> {}
unsafe impl<V: Trace> Trace for Environment<V> {
    custom_trace!(this, {
        if let Some(ref env) = this.parent {
            mark(env);
        }
        for v in this.bindings.values() {
            mark(v);
        }
    });
}

impl<V: Trace + Clone> Environment<V> {
    pub fn set(&mut self, name: ImmutableString, value: V) {
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

    pub fn define(&mut self, name: ImmutableString, value: V) {
        self.bindings.insert(name, value);
    }

    pub fn get(&self, name: &ImmutableString) -> Option<V> {
        if self.bindings.contains_key(name) {
            return self.bindings.get(name).cloned();
        } else if self.parent.is_none() {
            return None;
        }
        let mut environment = self.parent.clone().unwrap();
        loop {
            environment = {
                let borrowed = environment.borrow();
                if borrowed.bindings.contains_key(name) {
                    return borrowed.bindings.get(name).cloned();
                } else if borrowed.parent.is_none() {
                    return None;
                }
                borrowed.parent.clone().unwrap()
            }
        }
    }
}
