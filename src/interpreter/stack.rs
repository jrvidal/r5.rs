use std::collections::VecDeque;
use gc::{Finalize, Trace};

#[derive(Debug, Clone)]
pub(super) struct Stack<T>(VecDeque<T>);

impl<T> Stack<T> {
    pub fn new() -> Stack<T> {
        Stack(VecDeque::new())
    }
    pub fn push_front(&mut self, it: T) {
        self.0.push_front(it);
    }
    pub fn pop_front(&mut self) -> Option<T> {
        self.0.pop_front()
    }
    pub fn insert(&mut self, index: usize, it: T) {
        self.0.insert(index, it)
    }
    pub fn get(&mut self, index: usize) -> Option<&T> {
        self.0.get(index)
    }
    pub fn remove(&mut self, index: usize) -> Option<T> {
        self.0.remove(index)
    }
    pub fn swap_remove_front(&mut self, index: usize) -> Option<T> {
        self.0.swap_remove_front(index)
    }
}

use std::ops::Index;
impl<T> Index<usize> for Stack<T> {
    type Output = T;
    fn index(&self, i: usize) -> &T {
        &self.0[i]
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
