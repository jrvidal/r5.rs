use super::*;
use super::{parse_lambda_formals_exp, parse_definition, parse_body};
use std::collections::VecDeque;
use std::iter::FromIterator;
use ::parse::ast::datum::{Datum, AbbreviationKind};

macro_rules! list {
    ( [ $( $x:expr ),* ] ) => (
        Datum::List(vec_deque![
            $( $x ),*
        ])
    );
    ( $k:expr, [ $( $x:expr ),* ] ) => (
        Datum::List(vec_deque![
            Datum::Symbol($k.to_string()),
            $( $x ),*
        ])
    );
    // TO DO: WTF??
    ( $k:expr, [ $( $x:expr, )* ] ) => (list!($k, [ $( $x ),* ]));
    ( [ $( $x:expr, )* ] ) => (list!([ $( $x ),* ]));
}

macro_rules! assert_parsed {
    ($d:expr, $e:expr) => (assert_eq!(parse_expression($d), Ok($e));)
}

macro_rules! assert_parse_err {
    ($d:expr) => (assert!(parse_expression($d).is_err());)
}

macro_rules! symbol {
    ($s:expr) => (Datum::Symbol($s.to_string()))
}

#[test]
fn self_evaluating() {
    let datum = symbol!("foobar");
    assert_eq!(parse_expression(datum), Ok(Expression::Variable("foobar".to_string())));

    let datum = Datum::Boolean(true);
    assert_eq!(parse_expression(datum), Ok(Expression::Boolean(true)));

    let datum = Datum::Character('a');
    assert_eq!(parse_expression(datum), Ok(Expression::Character('a')));

    let datum = Datum::String("foobar".to_string());
    assert_eq!(parse_expression(datum), Ok(Expression::String("foobar".to_string())));
}

#[test]
fn reserved_keywords_error() {
    let datum = symbol!("letrec");
    assert_parse_err!(datum);
}

#[test]
fn simple_quotation() {
    let datum = Datum::Abbreviation {
        datum: Box::new(Datum::Boolean(true)),
        kind: AbbreviationKind::Quote
    };
    let expected = Expression::Quotation(Datum::Boolean(true));
    assert_parsed!(datum, expected);
}

#[test]
fn list_quotation() {
    let pair = Datum::Pair {
        car: vec_deque![symbol!("foobar")],
        cdr: Box::new(Datum::Character('a'))
    };
    let datum = Datum::Abbreviation {
        datum: Box::new(pair.clone()),
        kind: AbbreviationKind::Quote
    };
    let expected = Expression::Quotation(pair);
    assert_parsed!(datum, expected);
}

#[test]
fn verbose_quotation() {
    let datum = list!("quote", [
        Datum::Vector(vec_deque![
            Datum::Boolean(false)
        ])
    ]);
    let expected = Expression::Quotation(Datum::Vector(
        vec_deque![Datum::Boolean(false)],
    ));
    assert_parsed!(datum, expected);
}

#[test]
fn verbose_quotation_multiple_args_error() {
    let datum = list!("quote", [
        Datum::Vector(vec_deque![
            Datum::Boolean(false)
        ]),
        Datum::Boolean(true)
    ]);
    assert_parse_err!(datum);
}

#[test]
fn verbose_quotation_no_args_error() {
    let datum = list!("quote", []);
    assert_parse_err!(datum);
}

#[test]
fn empty_list_error() {
    let datum = list!([]);
    assert!(parse_expression(datum).is_err())
}

#[test]
fn call_expression() {
    let datum = list!("foobar", [
        Datum::Character('a'),
        Datum::Abbreviation {
            kind: AbbreviationKind::Quote,
            datum: Box::new(list!([]))
        }
    ]);
    let expected = Expression::Call {
        operator: Box::new(Expression::Variable("foobar".to_string())),
        operands: vec![
            Expression::Character('a'),
            Expression::Quotation(list!([]))
        ]
    };
    assert_parsed!(datum, expected);
}

#[test]
fn short_if_expression() {
    let datum = list!("if", [
        symbol!("foobar"),
        list!("some_fn", [Datum::Boolean(true)])
    ]);
    let expected = Expression::Conditional {
        test: Box::new(Expression::Variable("foobar".to_string())),
        consequent: Box::new(Expression::Call {
            operator: Box::new(Expression::Variable("some_fn".to_string())),
            operands: vec![Expression::Boolean(true)]
        }),
        alternate: None
    };
    assert_parsed!(datum, expected);
}

#[test]
fn long_if_expression() {
    let datum = list!("if", [
        Datum::Boolean(false),
        Datum::String("foobar".to_string()),
        Datum::Abbreviation {
            kind: AbbreviationKind::Quote,
            datum: Box::new(list!([]))
        }
    ]);
    let expected = Expression::Conditional {
        test: Box::new(Expression::Boolean(false)),
        consequent: Box::new(Expression::String("foobar".to_string())),
        alternate: Some(Box::new(Expression::Quotation(list!([]))))
    };
    assert_parsed!(datum, expected);
}

#[test]
fn too_short_if_error() {
    let datum = list!("if", [
        Datum::Character('a')
    ]);
    assert_parse_err!(datum);
}

#[test]
fn lonely_if_error() {
    let datum = list!("if", []);
    assert_parse_err!(datum);
}

#[test]
fn too_long_if_error() {
    let datum = list!("if", [
        Datum::Character('a'),
        Datum::Boolean(true),
        list!([]),
    ]);
    assert_parse_err!(datum);
}

#[test]
fn assignment() {
    let datum = list!("set!", [
        symbol!("x"),
        Datum::Boolean(true)
    ]);
    let expected = Expression::Assignment {
        variable: "x".to_string(),
        expression: Box::new(Expression::Boolean(true))
    };
    assert_parsed!(datum, expected);
}

#[test]
fn wrong_length_assignment_error() {
    let datum = list!("set!", [
    ]);
    assert_parse_err!(datum);

    let datum = list!("set!", [
        symbol!("x"),
    ]);
    assert_parse_err!(datum);

    let datum = list!("set!", [
        symbol!("x"),
        Datum::String("sakldfj".to_string()),
        Datum::Boolean(true),
    ]);
    assert_parse_err!(datum);
}

#[test]
fn bad_identifier_assignment_error() {
    let datum = list!("set!", [
        symbol!("if"),
        Datum::String("sakldfj".to_string()),
        Datum::Boolean(true),
    ]);
    assert_parse_err!(datum);
}

#[test]
fn lambda_formals_varargs() {
    let datum = symbol!("x");
    let expected = LambdaFormals::VarArgs("x".to_string());
    assert_eq!(parse_lambda_formals_exp(datum), Ok(expected));
}

#[test]
fn lambda_formals_list() {
    let datum = list!([symbol!("x"), symbol!("y")]);
    let expected = LambdaFormals::List(vec![
        "x".to_string(),
        "y".to_string(),
    ]);
    assert_eq!(parse_lambda_formals_exp(datum), Ok(expected));
}

#[test]
fn lambda_formals_rest() {
    let datum = Datum::Pair {
        car: vec_deque![
            symbol!("x"),
            symbol!("y"),
        ],
        cdr: Box::new(symbol!("z"))
    };
    let expected = LambdaFormals::Rest(vec![
        "x".to_string(),
        "y".to_string(),
    ], "z".to_string());
    assert_eq!(parse_lambda_formals_exp(datum), Ok(expected));
}

#[test]
fn lambda_formals_wrong_identifier_error() {
    let datum = symbol!("if");
    assert!(parse_lambda_formals_exp(datum).is_err());
}

#[test]
fn lambda_formals_no_vars_error() {
    let datum = list!([
        symbol!("x"),
        list!([symbol!("y")])
    ]);
    assert!(parse_lambda_formals_exp(datum).is_err());
}

#[test]
fn lambda_formals_wrong_datum_error() {
    let datum = Datum::Vector(vec_deque![
        symbol!("x"),
        symbol!("y")
    ]);
    assert!(parse_lambda_formals_exp(datum).is_err());
}

#[test]
fn simple_definition() {
    let datum = list!("define", [
        symbol!("x"),
        list!([
            symbol!("f")
        ])
    ]);
    let expected = Definition::Define {
        variable: "x".to_string(),
        expression: Box::new(Expression::Call {
            operator: Box::new(Expression::Variable("f".to_string())),
            operands: vec![]
        })
    };
    assert_eq!(parse_definition(datum), Ok(expected));
}

#[test]
fn lambda_definition() {
    let datum = list!("define", [
        list!([
            symbol!("x"),
            symbol!("y"),
        ]),
        symbol!("y")
    ]);
    let expected = Definition::DefineLambda {
        variable: "x".to_string(),
        formals: LambdaFormals::List(vec![
            "y".to_string()
        ]),
        body: Body {
            definitions: vec![],
            commands: vec![],
            expression: Box::new(Expression::Variable("y".to_string()))
        }
    };
    assert_eq!(parse_definition(datum), Ok(expected));
}

#[test]
fn begin_definition() {
    let datum = list!("begin", [
        list!("define", [
            symbol!("x"),
            Datum::Character('a')
        ])
    ]);
    let expected = Definition::Begin(vec![
        Definition::Define {
            variable: "x".to_string(),
            expression: Box::new(Expression::Character('a'))
        }
    ]);
    assert_eq!(parse_definition(datum), Ok(expected));
}

#[test]
fn definition_no_list_error() {
    let datum = Datum::String("foo".to_string());
    assert!(parse_definition(datum).is_err());
}

#[test]
fn definition_wrong_list_errors() {
    let datum = list!([]);
    assert!(parse_definition(datum).is_err());

    let datum = list!([symbol!("x")]);
    assert!(parse_definition(datum).is_err());

    let datum = list!([Datum::Character('a')]);
    assert!(parse_definition(datum).is_err());
}

#[test]
fn simple_definition_wrong_length_error() {
    let datum = list!([
        symbol!("define"), symbol!("x")
    ]);
    assert!(parse_definition(datum).is_err());

    let datum = list!([
        symbol!("define"), symbol!("x"), symbol!("y"), symbol!("z"),
    ]);
    assert!(parse_definition(datum).is_err());
}

#[test]
fn lambda_definition_no_var_error() {
    let datum = list!("define", [
        list!([]),
        symbol!("x")
    ]);
    assert!(parse_definition(datum).is_err());
}

#[test]
fn body() {
    let datums = vec_deque![
        symbol!("x")
    ];
    let expected = Body {
        definitions: vec![],
        commands: vec![],
        expression: Box::new(Expression::Variable("x".to_string()))
    };
    assert_eq!(parse_body(datums), Ok(expected));
}

#[test]
fn body_no_datums_error() {
    let datums = vec_deque![];
    assert!(parse_body(datums).is_err());
}

#[test]
fn only_definitions_body_error() {
    let datums = vec_deque![
        list!("define", [
            symbol!("x"),
            symbol!("y")
        ])
    ];
    assert!(parse_body(datums).is_err());
}

#[test]
fn lambda_exp() {
    let datum = list!("lambda", [
        symbol!("x"),
        symbol!("x")
    ]);
    let expected = Expression::Lambda {
        formals: LambdaFormals::VarArgs("x".to_string()),
        body: Body {
            definitions: vec![],
            commands: vec![],
            expression: Box::new(Expression::Variable("x".to_string()))
        }
    };

    assert_parsed!(datum, expected);
}

#[test]
fn boolean_expressions() {
    let datum = list!("and", [
        symbol!("x"),
        Datum::String("x".to_string())
    ]);
    let expected = Expression::Derived(Derived::And(vec![
        Expression::Variable("x".to_string()),
        Expression::String("x".to_string())
    ]));
    assert_parsed!(datum, expected);

    let datum = list!("or", [
        Datum::Boolean(false),
        symbol!("x"),
    ]);
    let expected = Expression::Derived(Derived::Or(vec![
        Expression::Boolean(false),
        Expression::Variable("x".to_string()),
    ]));
    assert_parsed!(datum, expected);
}