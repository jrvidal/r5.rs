pub const IF: &'static str = "if";
pub const OR: &'static str = "or";
pub const ARROW: &'static str = "=>";
pub const DO: &'static str = "do";
pub const AND: &'static str = "and";
pub const LET: &'static str = "let";
pub const ELSE: &'static str = "else";
pub const SET_BANG: &'static str = "set!";
pub const COND: &'static str = "cond";
pub const CASE: &'static str = "case";
pub const LET_STAR: &'static str = "let*";
pub const QUOTE: &'static str = "quote";
pub const BEGIN: &'static str = "begin";
pub const DELAY: &'static str = "delay";
pub const DEFINE: &'static str = "define";
pub const LAMBDA: &'static str = "lambda";
pub const LETREC: &'static str = "letrec";
pub const UNQUOTE: &'static str = "unquote";
pub const QUASIQUOTE: &'static str = "quasiquote";
pub const UNQUOTE_SPLICING: &'static str = "unquote-splicing";

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
