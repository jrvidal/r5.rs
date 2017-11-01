
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
