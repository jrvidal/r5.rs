
use gc::{Gc, GcCell, Trace};
pub type GcShared<T> = Gc<GcCell<T>>;


pub(super) fn shared<T: Trace>(x: T) -> GcShared<T> {
    Gc::new(GcCell::new(x))
}
