
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

macro_rules! list_from_vec {
    ($v:expr) => ({
      if $v.len() == 0
      let list = Value::P
    })
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
    assert_eq![&*pair.0.borrow(), &Value::Symbol("a".into())];
    assert_eq![&*pair.1.borrow(), &Value::Symbol("b".into())];
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

    assert_eq![&*list.0.borrow(), &Value::Symbol("a".into())];

    let cdr = &(*list.1).borrow();
    let list2 = cdr.pair().expect("list");

    assert_eq![&*list2.0.borrow(), &Value::Symbol("b".into())];
    assert_eq![&*list2.1.borrow(), &Value::EmptyList];
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
    assert_eq![&*list.0.borrow(), &Value::Symbol("b".into())];
    assert_eq![&*list.1.borrow(), &Value::EmptyList];
}

#[test]
fn stdlib_apply_basic_native() {
    let ret = with_std!["(apply list '(a))"].expect("valid call");
    let list = ret.pair().expect("valid pair");
    assert_eq![&*list.0.borrow(), &Value::Symbol("a".into())];
    assert_eq![&*list.1.borrow(), &Value::EmptyList];
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
