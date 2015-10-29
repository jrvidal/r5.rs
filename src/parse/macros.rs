
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
