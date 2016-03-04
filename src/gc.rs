use std::collections::{HashMap};
use std::cell::{RefCell, Ref, RefMut};
use std::ops::{Deref, DerefMut};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
pub struct GcId(u64);

impl GcId {
    fn next(&self) -> GcId {
        GcId(self.0 + 1)
    }
}


pub struct Heap<T> {
    // The Box<> is necessary so we have a stable address to look for the RefCell
    map: HashMap<GcId, Box<RefCell<Payload<T>>>>,
    // map: HashMap<GcId, Rc<RefCell<(GcId, Payload<T>)>>>,
    counter: GcId
}

#[derive(Clone, Debug)]
struct Payload<T> {
    reachable: bool,
    payload: T
}

#[derive(Debug)]
pub struct GcObject<T> {
    id: GcId,
    ptr: *const RefCell<Payload<T>>
}

pub struct GcRef<'a, T: 'a>(Ref<'a, Payload<T>>);
pub struct GcRefMut<'a, T: 'a>(RefMut<'a, Payload<T>>);

impl<T> GcObject<T> {
    pub fn borrow(&self) -> GcRef<T> {
        unsafe {
            GcRef((*self.ptr).borrow())
        }
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
            ptr: self.ptr
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
            counter: GcId(0)
        }
    }

    pub fn insert(&mut self, value: T) -> GcObject<T> {
        let id = self.counter;
        let cell = RefCell::new(Payload {
            payload: value,
            reachable: false
        });
        let cell_box = Box::new(cell);
        let ptr = {
            let cell : &RefCell<Payload<T>> = &*cell_box;
            cell as *const RefCell<Payload<T>>
        };

        self.map.insert(id, cell_box);
        self.counter = self.counter.next();

        GcObject {ptr: ptr, id: id}
    }

    pub fn collect_garbage(&mut self /*, some kind of iterator over gcobjects? */) {
        unimplemented!()
    }
}



// use std::rc::{Rc, Weak};
// use std::convert::AsRef;





// pub struct GcRef<T>(
//     // Ref<'a, (GcId, Payload<T>)>,
//     Rc<RefCell<(GcId, Payload<T>)>>, *const (GcId, Payload<T>)
// );

// use std::mem::forget;
// impl<T> GcObject<T> {
//     fn borrow(&self) -> GcRef<T> {
//         let rc = self.1.upgrade().unwrap();

//         let _ref = rc.as_ref().borrow();
//         forget(_ref);

//         // let _ref = rc.as_ref() as *const RefCell<(GcId, Payload<T>)>;

//         // forget(_ref);

//         GcRef(rc, &*_ref)
//     }
// }

// // fn foo<T>(GcRef(rc, ptr): GcRef<T>) {
// //     let ptr = ptr as Ref<(GcId, Payload<T>)>;
// // }

// use std::ops::Deref;

// // impl<'a, T: 'a> Deref for GcRef<T> where T: 'a {
// //     type Target = GcHiddenRef<'a, T>;

// //     fn deref(&self) -> GcHiddenRef<'a, T> {
// //         GcHiddenRef(self.0.borrow())
// //     }
// // }

// // impl<'a> Deref for GcRef<u8> {
// //     type Target = GcHiddenRef<'a, u8>;

// //     fn deref<'b>(&'b self) -> GcHiddenRef<'a, u8> {
// //         GcHiddenRef(self.0.borrow())
// //     }
// // }

// // struct GcHiddenRef<'a, T: 'a>(Ref<'a, (GcId, Payload<T>)>);



// // pub struct AliveGcObject<T>(Rc<RefCell<(GcId, Payload<T>)>>);

// // impl AsRef<T> for GcRef<T>;

// // pub struct GcRef<'a, T: 'a>(Ref<'a, Payload<T>>);

// // use owning_ref::RcRef;

// // impl AsRef<T> for GcRef<T> {
// //     fn as_ref(&self) -> &T {
// //         self.0
// //     }
// // }

// // impl<T> GcObject<T> {
// //     pub fn borrow<'a>(&self) -> GcRef<'a, T> {
// //         GcRef(self.1.upgrade().unwrap().as_ref())
// //     }
// // }



// // impl<T> Heap<T> {
// //     pub fn reset_reachability(&mut self) {
// //         for (_, val) in self.map.iter_mut() {
// //             val.as_ref().borrow_mut().1.reachable = false;
// //         }
// //     }

// //     pub fn collect_garbage(&mut self) {
// //         let unreachables : Vec<_> = self.map.iter()
// //             .filter_map(|(k, v)| {
// //                 if !v.as_ref().borrow().1.reachable {
// //                     Some(k.clone())
// //                 } else {
// //                     None
// //                 }
// //             }).collect();

// //         for unreachable in unreachables {
// //             self.map.remove(&unreachable);
// //         }
// //     }

// //     pub fn allocate(&mut self, value: T) -> GcObject<T> {
// //         self.counter = self.counter.next();
// //         let gc_obj = Rc::new(RefCell::new((self.counter.clone(), Payload{ payload: value, reachable: true })));
// //         let weak = Rc::downgrade(&gc_obj);
// //         self.map.insert(self.counter, gc_obj);

// //         GcObject(self.counter.clone(), weak)
// //     }

// //     // pub fn borrow(&self, GcObject { id } : GcObject) -> Option<&T> {
// //     //     self.map.get(&id).map(|val| &val.1.payload)
// //     // }

// //     // pub fn borrow_mut(&mut self, GcObject { id } : GcObject) -> Option<&mut T> {
// //     //     self.map.get_mut(&id).map(|val| &mut val.1.payload)
// //     // }

// // //     pub fn mark(&mut self, id: &GcId) -> bool {
// // //         if let Some(ref obj) = self.map.get_mut(id) {
// // //             obj.borrow_mut().reachable = false
// // //         } else {
// // //             false
// // //         }
// // //     }

// // }


// // // use std::cell::{RefCell, Ref, RefMut};
// // // use std::rc::{Rc, Weak};
// // // use std::ops::{Deref, DerefMut};


// // // #[derive(Debug)]
// // // struct GcAllocation<T> {
// // //     reference: Rc<RefCell<Payload<T>>>,
// // //     id: GcId
// // // }

// // // // #[derive(Debug)]
// // // // pub struct GcObject<T> {
// // // //     reference: Weak<RefCell<Payload<T>>>,
// // // //     id: GcId
// // // // }

// // // // impl<T> GcAllocation<T> {
// // // // }


// // // // impl<T> Clone for GcAllocation<T> {
// // // //     fn clone(&self) -> GcAllocation<T> {
// // // //         GcAllocation {
// // // //             id: self.id.clone(),
// // // //             reference: self.reference.clone()
// // // //         }
// // // //     }
// // // // }


// // // // impl<T> GcObject<T> {
// // // //     // pub fn borrow(&self) -> GcRef<T> {
// // // //     //     let x = self.reference;
// // // //     //     GcRef(x.upgrade().unwrap().as_ref().borrow())
// // // //     // }
// // // // }

// // // // #[derive(Debug)]
// // // // pub struct GcRef<'a, T: 'a>(Ref<'a, Payload<T>>);

// // // // impl <'a, T> Deref for GcRef<'a, T> {
// // // //     type Target = T;
// // // //     fn deref(&self) -> &T {
// // // //         &self.0.payload
// // // //     }
// // // // }

// // // // struct GcExtraRef<'a, T: 'a>(Ref<'a, Payload<)


// // // // impl<T> GcObject<T> {
// // // //     pub fn borrow(&self) -> Option<GcRef<T>> {
// // // //         self.reference.upgrade()
// // // //             .map(|full_rc| {
// // // //                 let refref = RcRef::new(full_rc);
// // // //                 let myrefref = refref.map(|x| &x.borrow().payload);
// // // //                 GcRef(myrefref)
// // // //             })
// // // //             // .map(GcRef)
// // // //     }
// // // //     // pub fn borrow_mut(&self) -> Option<GcMutRef<T>> {
// // // //     //     self.reference.upgrade()
// // // //     //         .map(|full_rc| {
// // // //     //             full_rc.borrow_mut()
// // // //     //         })
// // // //     //         .map(GcMutRef)
// // // //     // }
// // // // }

// // // // use owning_ref::{RefRef, OwningRef, RcRef};

// // // // #[derive(Debug)]
// // // // pub struct GcRef<T>(OwningRef<Rc<RefCell<Payload<T>>>, T>);

// // // // // impl<'a, T> Deref for GcRef<'a, T> {
// // // // //     type Target = T;
// // // // //     fn deref(&self) -> &T {
// // // // //         &self.0
// // // // //         // &self.0.as_ref().borrow().payload
// // // // //     }
// // // // // }

// // // // #[derive(Debug)]
// // // // pub struct GcMutRef<'a, T: 'a>(RefMut<'a, Payload<T>>);
// // // // impl<'a, T> Deref for GcMutRef<'a, T> {
// // // //     type Target = T;
// // // //     fn deref(&self) -> &T {
// // // //         &self.0.payload
// // // //     }
// // // // }
// // // // impl<'a, T> DerefMut for GcMutRef<'a, T> {
// // // //     fn deref_mut(&mut self) -> &mut T {
// // // //         &mut self.0.payload
// // // //     }
// // // // }

// // // pub struct Heap<T> {
// // //     map: HashMap<GcId, GcAllocation<T>>,
// // //     counter: GcId
// // // }