use std::collections::{HashMap};
use std::cell::{RefCell, Ref, RefMut};
use std::ops::{Deref, DerefMut, Not};
use std::rc::Rc;
use ::helpers::{Shared, SharedRef, SharedRefMut};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
pub struct GcId(usize);

impl GcId {
    fn next(&self) -> GcId {
        GcId(self.0 + 1)
    }
}

#[derive(Debug)]
pub struct GcObject<T> {
    id: GcId,
    _ref: Shared<T>
}

impl<T> Clone for GcObject<T> {
    fn clone(&self) -> GcObject<T> {
        GcObject {
            id: self.id,
            _ref: self._ref.clone()
        }
    }
}

impl<T> GcObject<T> {
    fn new(value: T, id: GcId) -> GcObject<T> {
        GcObject {
            _ref: Shared::new(value),
            id: id
        }
    }
}

pub struct Heap<T> {
    map: HashMap<GcId, GcObject<T>>,
    counter: GcId
}


impl<T> Heap<T> {
    // http://is.gd/7uMWU7
    pub fn new() -> Heap<T> {
        Heap {
            map: HashMap::new(),
            counter: GcId(0)
        }
    }

    pub fn insert(&mut self, value: T) -> GcObject<T> {
        let id = self.counter;
        let gc_object = GcObject::new(value, id);

        self.map.insert(id, gc_object.clone());
        self.counter = self.counter.next();

        gc_object
    }

    pub fn get(&mut self, id: GcId) -> Result<GcObject<T>, ()> {
        self.map.get(&id).ok_or_else(|| ()).map(|r| r.clone())
    }

    // pub fn collect_garbage<'a>(&mut self, alive: T) where T: Iterator<Item=&'a GcObject<T>>, T: 'a {
    //     for obj in alive {
    //         let id = obj.id;
    //         match self.map.get_mut(&id) {
    //             None => panic!("alive object not found on heap!"),
    //             Some(ref cell) => {
    //                 cell.borrow_mut().flip()
    //             }
    //         };
    //     }

    //     let deads : Vec<GcId> = self.map.iter()
    //         .filter(|&(_, val)| {
    //             (*val.borrow()).reachable == self.state
    //         })
    //         .map(|(&k, _)| k)
    //         .collect();

    //     use std::collections::hash_map::Entry;

    //     for k in deads.into_iter() {
    //         if let Entry::Occupied(entry) = self.map.entry(k) {
    //             entry.remove();
    //         }
    //     }

    //     self.state = !self.state
    // }
}


pub struct BorrowError;

impl<T> GcObject<T> {
    // pub fn borrow(&self) -> SharedRef<T> {
    //     self._ref.borrow()
    // }

    // pub fn borrow_mut(&self) -> SharedRefMut<T> {
    //     self._ref.borrow_mut()
    // }

    pub fn borrow(&self) -> Result<SharedRef<T>, ()> {
        self._ref.try_borrow().map_err(|_| ())
    }

    pub fn borrow_mut(&self) -> Result<SharedRefMut<T>, ()> {
        self._ref.try_borrow_mut().map_err(|_| ())
    }
}

// pub struct GcRef<'a, T: 'a>(Ref<'a, T>);
// pub struct GcRefMut<'a, T: 'a>(RefMut<'a, T>);

// impl<'a, T> Deref for GcRef<'a, T> {
//     type Target = T;

//     fn deref(&self) -> &T {
//         self.0.deref()
//     }
// }

// impl<'a, T> Deref for GcRefMut<'a, T> {
//     type Target = T;

//     fn deref(&self) -> &T {
//         self.0.deref()
//     }
// }

// impl<'a, T> DerefMut for GcRefMut<'a, T> {
//     fn deref_mut(&mut self) -> &mut T {
//         self.0.deref_mut()
//     }
// }

// #[derive(Clone, Copy, Debug, Eq, PartialEq)]
// enum GarbageState {
//     LEFT,
//     RIGHT
// }

// impl Not for GarbageState {
//     type Output = GarbageState;

//     fn not(self) -> GarbageState {
//         match self {
//             GarbageState::LEFT => GarbageState::RIGHT,
//             GarbageState::RIGHT => GarbageState::LEFT
//         }
//     }
// }

// #[derive(Clone, Debug)]
// struct Payload<T> {
//     reachable: GarbageState,
//     payload: T
// }

// impl<T> Payload<T> {
//     fn flip(&mut self) {
//         self.reachable = !self.reachable;
//     }
// }

// #[derive(Debug)]
// pub struct GcObject<T> {
//     id: GcId,
//     ptr: *const RefCell<Payload<T>>
// }
