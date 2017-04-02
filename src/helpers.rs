use std::rc::Rc;
use std::cell::{Ref, RefCell, RefMut, BorrowError, BorrowMutError};
use std::ops::{Deref, DerefMut};
use std::borrow::{Borrow, BorrowMut};

pub trait Tuple2Helper<A, B, E> {
    fn result(self) -> Result<(A, B), E>;
}

impl<A, B, E> Tuple2Helper<A, B, E> for (Result<A, E>, Result<B, E>) {
    fn result(self) -> Result<(A, B), E> {
        match (self.0, self.1) {
            (Ok(a), Ok(b)) => Ok((a, b)),
            (Err(e), _) | (_, Err(e)) => Err(e),
        }
    }
}

pub trait Tuple3Helper<A, B, C, E> {
    fn result(self) -> Result<(A, B, C), E>;
}

impl<A, B, C, E> Tuple3Helper<A, B, C, E> for (Result<A, E>, Result<B, E>, Result<C, E>) {
    fn result(self) -> Result<(A, B, C), E> {
        match (self.0, self.1, self.2) {
            (Ok(a), Ok(b), Ok(c)) => Ok((a, b, c)),
            (Err(e), _, _) | (_, Err(e), _) | (_, _, Err(e)) => Err(e),
        }
    }
}

pub trait ResultHelper {
    fn result(self) -> Result<(), ()>;
}

impl ResultHelper for bool {
    fn result(self) -> Result<(), ()> {
        if self { Ok(()) } else { Err(()) }
    }
}

//
// Macros
//
macro_rules! ret_val {
    ($x:expr) => (return ok_some!($x));
    ($x:expr, $s:ident, $n:expr) => ({
        $s.advance($n);
        return Ok(Some($x))
    });
}

macro_rules! ok_some {
    ($x:expr) => (Ok(Some($x)))
}

macro_rules! vec_deque {
    ($( $x:expr ),*) => ({
        let v = vec![$( $x ),*];
        VecDeque::from_iter(v.into_iter())
    });
    // TO DO: WTF??
    ($( $x:expr, )*) => (vec_deque![ $( $x ),* ]);
}


#[derive(Debug)]
pub struct Shared<T>(Rc<RefCell<T>>);

impl<T> Clone for Shared<T> {
    fn clone(&self) -> Shared<T> {
        Shared(self.0.clone())
    }
}

impl<T> Shared<T> {
    pub fn new(value: T) -> Shared<T> {
        Shared(Rc::new(RefCell::new(value)))
    }
}

pub struct SharedRef<'a, T: 'a>(Ref<'a, T>);
pub struct SharedRefMut<'a, T: 'a>(RefMut<'a, T>);

impl<T> Shared<T> {
    pub fn borrow(&self) -> SharedRef<T> {
        SharedRef((*self.0).borrow())
    }

    pub fn borrow_mut(&self) -> SharedRefMut<T> {
        SharedRefMut((*self.0).borrow_mut())
    }

    pub fn try_borrow(&self) -> Result<SharedRef<T>, BorrowError> {
        self.0.try_borrow().map(SharedRef)
    }

    pub fn try_borrow_mut(&self) -> Result<SharedRefMut<T>, BorrowMutError> {
        self.0.try_borrow_mut().map(SharedRefMut)
    }
}

impl<'a, T> Deref for SharedRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.0.deref()
    }
}

impl<'a, T> Deref for SharedRefMut<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.0.deref()
    }
}

impl<'a, T> DerefMut for SharedRefMut<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.0.deref_mut()
    }
}


//
// Strings
//

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct ImmutableString(Rc<String>);

impl Deref for ImmutableString {
    type Target = String;
    fn deref(&self) -> &String {
        &*self.0
    }
}

impl From<String> for ImmutableString {
    fn from(s: String) -> ImmutableString {
        ImmutableString(Rc::new(s))
    }
}

impl<'a> From<&'a str> for ImmutableString {
    fn from(s: &'a str) -> ImmutableString {
        ImmutableString(Rc::new(String::from(s)))
    }
}

use std::borrow;
impl borrow::Borrow<String> for ImmutableString {
    fn borrow(&self) -> &String {
        &self.0
    }
}

impl<'a> borrow::Borrow<String> for &'a ImmutableString {
    fn borrow(&self) -> &String {
        &*self.0
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CowString(Rc<String>);

impl CowString {
    fn mutate<F: FnMut(&mut String)>(&self, mut mutator: F) -> CowString {
        let mut copy = (*self.0).clone();
        mutator(&mut copy);
        CowString(Rc::new(copy))
    }
}

impl From<String> for CowString {
    fn from(s: String) -> CowString {
        CowString(Rc::new(s))
    }
}

impl<'a> From<&'a str> for CowString {
    fn from(s: &'a str) -> CowString {
        CowString(Rc::new(String::from(s)))
    }
}


pub struct NonEmptyVec<T>(Vec<T>);

impl<T> NonEmptyVec<T> {
    pub fn new(first: T) -> NonEmptyVec<T> {
        let v = vec![first];
        NonEmptyVec(v)
    }

    pub fn pop(self) -> (T, Vec<T>) {
        let mut vec = self.0;
        let tail = match vec.pop() {
            Some(x) => x,
            None => unreachable!()
        };
        (tail, vec)
    }
}
