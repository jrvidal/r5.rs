
macro_rules! ok_some {
    ($x:expr) => (Ok(Some($x)))
}

macro_rules! ret_val {
    ($x:expr) => (return ok_some!($x));
    ($x:expr, $s:ident, $n:expr) => ({
        $s.advance($n);
        return Ok(Some($x))
    });
}

macro_rules! vec_deque {
    ($( $x:expr ),*) => ({
        let v = vec![$( $x ),*];
        VecDeque::from_iter(v.into_iter())
    });
    // TO DO: WTF??
    ($( $x:expr, )*) => (vec_deque![ $( $x ),* ]);
}