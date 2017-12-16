pub const IF: &str = "if";
pub const OR: &str = "or";
pub const ARROW: &str = "=>";
pub const DO: &str = "do";
pub const AND: &str = "and";
pub const LET: &str = "let";
pub const ELSE: &str = "else";
pub const SET_BANG: &str = "set!";
pub const COND: &str = "cond";
pub const CASE: &str = "case";
pub const LET_STAR: &str = "let*";
pub const QUOTE: &str = "quote";
pub const BEGIN: &str = "begin";
pub const DELAY: &str = "delay";
pub const DEFINE: &str = "define";
pub const LAMBDA: &str = "lambda";
pub const LETREC: &str = "letrec";
pub const UNQUOTE: &str = "unquote";
pub const QUASIQUOTE: &str = "quasiquote";
pub const UNQUOTE_SPLICING: &str = "unquote-splicing";

macro_rules! one_of {
    ($x:expr, [$c:expr]) => ($x == $c);
    ($x:expr, [ $c:expr, $( $d:expr ),* ]) => (
        $x == $c || one_of!($x, [$( $d ),* ])
    )
}

pub fn is_syntactic_keyword(name: &str) -> bool {
    match name.len() {
        2 => one_of!(name, [IF, OR, ARROW, DO]),
        3 => one_of!(name, [AND, LET]),
        4 => one_of!(name, [ELSE, SET_BANG, COND, CASE, LET_STAR]),
        5 => one_of!(name, [QUOTE, BEGIN, DELAY]),
        6 => one_of!(name, [DEFINE, LAMBDA, LETREC]),
        7 => one_of!(name, [UNQUOTE]),
        10 => one_of!(name, [QUASIQUOTE]),
        16 => one_of!(name, [UNQUOTE_SPLICING]),
        _ => false,
    }
}
