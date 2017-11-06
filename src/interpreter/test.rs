
use vm::{default_env, null_env, ExecutionError, Value};
use super::{interpret, InterpreterError};

use self::ExecutionError::*;

macro_rules! with_null {
    ($code:expr) => (interpret($code, null_env()))
}

macro_rules! with_std {
    ($code:expr) => (interpret($code, default_env()))
}

macro_rules! rt_err {
    ($err:expr) => (Err(InterpreterError::Exec($err)))
}

#[test]
fn symbol() {
    assert_eq![with_null!["'a"], Ok(Value::Symbol("a".into()))];
}

#[test]
fn two_expressions() {
    assert_eq![with_null!["'a\n'b"], Ok(Value::Symbol("b".into()))];
}

#[test]
fn stdlib_list_empty() {
    assert_eq![with_std!["(list)"], Ok(Value::EmptyList)];
}

#[test]
fn stdlib_cons_argc() {
    assert_eq![with_std!["(cons)"], rt_err![BadArgc]];
    assert_eq![with_std!["(cons 'a)"], rt_err![BadArgc]];
    assert_eq![with_std!["(cons 'a 'b 'c)"], rt_err![BadArgc]];
}

#[test]
fn stdlib_cons() {
    let ret = with_std!["(cons 'a 'b)"].expect("valid cons");
    let pair = ret.pair().expect("pair");
    let borrow = pair.borrow();
    assert_eq![&borrow.0, &Value::Symbol("a".into())];
    assert_eq![&borrow.1, &Value::Symbol("b".into())];
}

#[test]
fn basic_call() {
    assert_eq![
        with_null!["((lambda () 'a))"],
        Ok(Value::Symbol("a".into()))
    ];
}

#[test]
fn basic_call_varargs() {
    let ret = with_null!["((lambda x x) 'a 'b)"].expect("valid call");
    let list = ret.pair().expect("pair");
    let borrow = list.borrow();

    assert_eq![&borrow.0, &Value::Symbol("a".into())];

    let list2 = borrow.1.pair().expect("list");
    let borrow2 = list2.borrow();

    assert_eq![&borrow2.0, &Value::Symbol("b".into())];
    assert_eq![&borrow2.1, &Value::EmptyList];
}

#[test]
fn stdlib_apply_argc() {
    assert_eq![with_std!["(apply)"], rt_err![BadArgc]];
    assert_eq![with_std!["(apply 'a)"], rt_err![BadArgc]];
    assert_eq![with_std!["(apply 'a 'b 'c)"], rt_err![BadArgc]];
}

#[test]
fn stdlib_apply_argtype() {
    assert_eq![with_std!["(apply 'a '())"], rt_err![NonCallable]];
    assert_eq![with_std!["(apply apply 'a)"], rt_err![BadArgType]];
}

#[test]
fn stdlib_apply_basic() {
    let ret = with_std!["(apply (lambda (x . y) y) '(a b))"].expect("valid call");
    let list = ret.pair().expect("valid pair");
    let borrow = list.borrow();
    assert_eq![&borrow.0, &Value::Symbol("b".into())];
    assert_eq![&borrow.1, &Value::EmptyList];
}

#[test]
fn stdlib_apply_basic_native() {
    let ret = with_std!["(apply list '(a))"].expect("valid call");
    let list = ret.pair().expect("valid pair");
    let borrow = list.borrow();
    assert_eq![&borrow.0, &Value::Symbol("a".into())];
    assert_eq![&borrow.1, &Value::EmptyList];
}

#[test]
fn cond_basic() {
    assert_eq![with_null!["(cond (#t 'a))"], Ok(Value::Symbol("a".into()))];
}

#[test]
fn cond_empty() {
    assert_eq![with_null!["(cond (#f 'a))"], Ok(Value::Nil)];
}

#[test]
fn cond_cascade() {
    assert_eq![
        with_null!["(cond (((lambda () #f)) 'a) (1 'b))"],
        Ok(Value::Symbol("b".into()))
    ];
}

#[test]
fn cond_test_only() {
    assert_eq![with_null!["(cond ('a))"], Ok(Value::Symbol("a".into()))];
}

#[test]
fn cond_arrow() {
    assert_eq![
        with_std!["(cond ('(a b) => car))"],
        Ok(Value::Symbol("a".into()))
    ];
}

#[test]
fn cond_arrow_false() {
    assert_eq![
        with_null!["(cond (#f => car) (#t 'z))"],
        Ok(Value::Symbol("z".into()))
    ];
}

#[test]
fn delay_basic() {
    assert_eq![
        with_std!["(force (delay 'a))"],
        Ok(Value::Symbol("a".into()))
    ];
}

#[test]
fn delay_no_clobber() {
    assert_eq![
        with_std!["(force (let ((a 'one)) (delay a)))"],
        Ok(Value::Symbol("one".into()))
    ];
}

#[test]
fn delay_is_delayed() {
    assert![with_null!["(delay variable-yet-to-be-defined)"].is_ok()];
}

#[test]
fn case_basic() {
    assert_eq![
        with_null!["(case 'a ((a) 'b))"],
        Ok(Value::Symbol("b".into()))
    ];
}

#[test]
fn case_multiple() {
    assert_eq![
        with_null!["(case 'a ((b c) 'd) ((e a) 'x))"],
        Ok(Value::Symbol("x".into()))
    ];
}

#[test]
fn case_with_else() {
    assert_eq![
        with_null!["(case 'a ((b c) 'd) ((e z) 'x) (else 'w 'n))"],
        Ok(Value::Symbol("n".into()))
    ];
}

#[test]
fn case_nil() {
    assert_eq![
        with_null!["(case 'a ((b c) 'd) ((e z) 'x))"],
        Ok(Value::Nil)
    ];
}

#[test]
fn let_basic() {
    assert_eq![
        with_null!["(let ((x 'a)) x)"],
        Ok(Value::Symbol("a".into()))
    ];
}

#[test]
fn let_order() {
    assert_eq![with_null!["(let ((x 'a) (y x)) x)"], rt_err![UnboundVar]];
}


#[test]
fn let_star_order() {
    assert_eq![
        with_null!["(let* ((x 'a) (y x)) y)"],
        Ok(Value::Symbol("a".into()))
    ];
}

#[test]
fn named_let_basic() {
    assert_eq![
        with_std![
            "(let fn ((x '(a b))) (if (null? (cdr x)) (car x) (fn (cdr x))))"
        ],
        Ok(Value::Symbol("b".into()))
    ];
}

#[test]
fn unquoted_vectors() {
    assert![with_null!["#('a)"].is_err()];
}

#[test]
fn stdlib_eqv_lists() {
    assert_eq![with_std!["(eqv? '(a) '(a))"], Ok(Value::Boolean(false))];
}

#[test]
fn stdlib_eqv_empty_lists() {
    assert_eq![with_std!["(eqv? '() '())"], Ok(Value::Boolean(true))];
}

#[test]
fn stdlib_eqv_lambdas() {
    assert_eq![
        with_std!["(eqv? (lambda () 'a) (lambda () 'a))"],
        Ok(Value::Boolean(false))
    ];
}

#[test]
fn stdlib_eqv_native_procs() {
    assert_eq![with_std!["(eqv? eqv? eqv?)"], Ok(Value::Boolean(true))];
}

#[test]
fn quotation_pair() {
    let pair = with_std!["'(a . b)"]
        .expect("valid quote")
        .pair()
        .expect("valid pair");

    let borrow = pair.borrow();
    assert_eq![&borrow.0, &Value::Symbol("a".into())];
    assert_eq![&borrow.1, &Value::Symbol("b".into())];
}

#[test]
fn stdlib_substraction_simple() {
    assert_eq![with_std!["(-)"], Ok(Value::Integer(0))];
    assert_eq![with_std!["(- 1)"], Ok(Value::Integer(-1))];
    assert_eq![with_std!["(- 1 1 1)"], Ok(Value::Integer(-1))];
    assert_eq![with_std!["(- 2 1 1)"], Ok(Value::Integer(0))];
}

#[test]
fn stdlib_leq_than_simple() {
    assert_eq![with_std!["(<=)"], Ok(Value::Boolean(true))];
    assert_eq![with_std!["(<= 1)"], Ok(Value::Boolean(true))];
    assert_eq![with_std!["(<= -1)"], Ok(Value::Boolean(true))];
    assert_eq![with_std!["(<= 0)"], Ok(Value::Boolean(true))];
    assert_eq![with_std!["(<= 0 1)"], Ok(Value::Boolean(true))];
    assert_eq![with_std!["(<= -1 1)"], Ok(Value::Boolean(true))];
    assert_eq![with_std!["(<= 2 1)"], Ok(Value::Boolean(false))];
    assert_eq![with_std!["(<= 1 2 0)"], Ok(Value::Boolean(false))];
}


// http://rosettacode.org/wiki/Man_or_boy_test#Scheme
#[test]
fn man_or_boy() {
    let code = "
        (set! A (lambda (k x1 x2 x3 x4 x5)
          (define (B)
            (set! k (- k 1))
            (A k B x1 x2 x3 x4))
          (if (<= k 0)
              (+ (x4) (x5))
              (B))))

        (A 10 (lambda () 1) (lambda () -1) (lambda () -1) (lambda () 1) (lambda () 0))
    ";
    assert_eq![with_std![code], Ok(Value::Integer(-67))];
}
