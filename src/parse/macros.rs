
macro_rules! ret_val {
    ($x:expr) => (return Ok(Some($x)));
    ($x:expr, $s:ident, $n:expr) => ({
        $s.advance($n);
        return Ok(Some($x))
    });
}