use std::collections::HashMap;
use std::cell::{RefCell, Ref, RefMut};
use std::ops::{Deref, DerefMut, Not};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
pub struct GcId(u64);

impl GcId {
    fn next(&self) -> GcId {
        GcId(self.0 + 1)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum GarbageState {
    LEFT,
    RIGHT,
}

impl Not for GarbageState {
    type Output = GarbageState;

    fn not(self) -> GarbageState {
        match self {
            GarbageState::LEFT => GarbageState::RIGHT,
            GarbageState::RIGHT => GarbageState::LEFT,
        }
    }
}


pub struct Heap<T> {
    // The Box<> is necessary so we have a stable address to look for the RefCell
    // TO DO: or maybe not? we could deref the RefCell (gasp!)
    map: HashMap<GcId, Box<RefCell<Payload<T>>>>,
    counter: GcId,
    state: GarbageState,
}

#[derive(Clone, Debug)]
struct Payload<T> {
    reachable: GarbageState,
    payload: T,
}

impl<T> Payload<T> {
    fn flip(&mut self) {
        self.reachable = !self.reachable;
    }
}

#[derive(Debug)]
pub struct GcObject<T> {
    id: GcId,
    ptr: *const RefCell<Payload<T>>,
}

pub struct GcRef<'a, T: 'a>(Ref<'a, Payload<T>>);
pub struct GcRefMut<'a, T: 'a>(RefMut<'a, Payload<T>>);

impl<T> GcObject<T> {
    pub fn borrow(&self) -> GcRef<T> {
        unsafe { GcRef((*self.ptr).borrow()) }
    }

    // Unsafe so we know when we're using it
    pub unsafe fn borrow_mut(&self) -> GcRefMut<T> {
        GcRefMut((*self.ptr).borrow_mut())
    }
}

impl<T> Clone for GcObject<T> {
    fn clone(&self) -> GcObject<T> {
        GcObject {
            id: self.id,
            ptr: self.ptr,
        }
    }
}

impl<'a, T> Deref for GcRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0.deref().payload
    }
}

impl<'a, T> Deref for GcRefMut<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0.deref().payload
    }
}

impl<'a, T> DerefMut for GcRefMut<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.0.deref_mut().payload
    }
}

impl<T> Heap<T> {
    // http://is.gd/7uMWU7
    pub fn general_new() -> Heap<T> {
        Heap {
            map: HashMap::new(),
            counter: GcId(0),
            state: GarbageState::LEFT,
        }
    }

    pub fn insert(&mut self, value: T) -> GcObject<T> {
        let id = self.counter;
        let cell = RefCell::new(Payload {
                                    payload: value,
                                    reachable: self.state,
                                });
        let cell_box = Box::new(cell);
        let ptr = {
            let cell: &RefCell<Payload<T>> = &*cell_box;
            cell as *const RefCell<Payload<T>>
        };

        self.map.insert(id, cell_box);
        self.counter = self.counter.next();

        GcObject { ptr: ptr, id: id }
    }

    pub fn collect_garbage<'a>(&mut self, alive: T)
        where T: Iterator<Item = &'a GcObject<T>>,
              T: 'a
    {
        for obj in alive {
            let id = obj.id;
            match self.map.get_mut(&id) {
                None => panic!("alive object not found on heap!"),
                Some(ref cell) => cell.borrow_mut().flip(),
            };
        }

        let deads: Vec<GcId> = self.map
            .iter()
            .filter(|&(_, val)| (*val.borrow()).reachable == self.state)
            .map(|(&k, _)| k)
            .collect();

        use std::collections::hash_map::Entry;

        for k in deads.into_iter() {
            if let Entry::Occupied(entry) = self.map.entry(k) {
                entry.remove();
            }
        }

        self.state = !self.state
    }
}
