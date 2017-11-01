use std::fmt::Debug;
use gc::{Finalize, Trace};

#[derive(Clone)]
pub(super) struct Stack<T>(Vec<T>);

impl<T> Stack<T> {
    pub fn push(&mut self, it: T) {
        self.0.push(it);
    }
    pub fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }
    // pub fn insert(&mut self, index: usize, it: T) {
    //     // This one is different
    //     let i = self.len() - index;
    //     self.0.insert(i, it)
    // }
    // pub fn get(&mut self, index: usize) -> Option<&T> {
    //     let i = self.index(index);
    //     self.0.get(i)
    // }
    // pub fn remove(&mut self, index: usize) -> T {
    //     let i = self.index(index);
    //     self.0.remove(i)
    // }
    // pub fn swap_remove(&mut self, index: usize) -> T {
    //     let i = self.index(index);
    //     self.0.swap_remove(i)
    // }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    fn index(&self, i: usize) -> usize {
        self.0.len() - i - 1
    }
}

impl<T: Debug> Debug for Stack<T> {
    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(fmt, "[")?;
        let l = self.0.len();
        for i in 0..l {
            write!(fmt, "{:?}", self.0[l - i - 1])?;
            if i != l - 1 {
                write!(fmt, ", ")?;
            }
        }

        write!(fmt, "]")
    }
}

impl<T> Default for Stack<T> {
    fn default() -> Stack<T> {
        Stack(Vec::new())
    }
}

use std::ops::Index;
impl<T> Index<usize> for Stack<T> {
    type Output = T;
    fn index(&self, i: usize) -> &T {
        &self.0[self.index(i)]
    }
}

impl<T: Finalize> Finalize for Stack<T> {}

unsafe impl<T: Trace> Trace for Stack<T> {
    custom_trace!(this, {
        for x in this.0.iter() {
            mark(x);
        }
    });
}
