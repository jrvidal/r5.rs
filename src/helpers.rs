use std::fmt::{Display, Error as FmtError, Formatter};
use std::ops::Deref;
use std::rc::Rc;

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
        if self {
            Ok(())
        } else {
            Err(())
        }
    }
}

//
// Macros
//
macro_rules! ret_val {
    ($x:expr) => {
        return ok_some!($x);
    };
    ($x:expr, $s:ident, $n:expr) => {{
        $s.advance($n);
        return Ok(Some($x));
    }};
}

macro_rules! ok_some {
    ($x:expr) => {
        Ok(Some($x))
    };
}

//
// Strings
//

#[derive(Clone, Debug, PartialEq, Hash, Eq, Default)]
pub struct ImmutableString(Rc<String>);

impl Deref for ImmutableString {
    type Target = String;
    fn deref(&self) -> &String {
        &*self.0
    }
}

impl Display for ImmutableString {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), FmtError> {
        self.0.fmt(fmt)
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

#[derive(Clone, Debug, PartialEq, Default)]
pub struct CowString(Rc<String>);

impl CowString {
    #[allow(dead_code)]
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

impl<'a> From<&'a CowString> for String {
    fn from(s: &CowString) -> String {
        s.0.deref().clone()
    }
}
