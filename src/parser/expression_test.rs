use super::*;
use super::{parse_body, parse_case_clause_exp, parse_cond_clause_exp, parse_definition,
            parse_else_clause, parse_iteration_spec, parse_lambda_formals_exp, parse_let_exp};
use std::collections::VecDeque;
use std::iter::FromIterator;
use reader::{AbbreviationKind, Datum};

macro_rules! vec_deque {
    ($( $x:expr ),*) => ({
        let v = vec![$( $x ),*];
        VecDeque::from_iter(v.into_iter())
    });
    // TO DO: WTF??
    ($( $x:expr, )*) => (vec_deque![ $( $x ),* ]);
}


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
    assert_eq!(
        parse_expression(datum),
        Ok(Expression::Variable("foobar".to_string()))
    );

    let datum = Datum::Boolean(true);
    assert_eq!(parse_expression(datum), Ok(Expression::Boolean(true)));

    let datum = Datum::Character('a');
    assert_eq!(parse_expression(datum), Ok(Expression::Character('a')));

    let datum = Datum::String("foobar".to_string());
    assert_eq!(
        parse_expression(datum),
        Ok(Expression::String("foobar".to_string()))
    );
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
        kind: AbbreviationKind::Quote,
    };
    let expected = Expression::Quotation(Datum::Boolean(true));
    assert_parsed!(datum, expected);
}

#[test]
fn list_quotation() {
    let pair = Datum::Pair {
        car: vec_deque![symbol!("foobar")],
        cdr: Box::new(Datum::Character('a')),
    };
    let datum = Datum::Abbreviation {
        datum: Box::new(pair.clone()),
        kind: AbbreviationKind::Quote,
    };
    let expected = Expression::Quotation(pair);
    assert_parsed!(datum, expected);
}

#[test]
fn verbose_quotation() {
    let datum = list!("quote", [Datum::Vector(vec_deque![Datum::Boolean(false)])]);
    let expected = Expression::Quotation(Datum::Vector(vec_deque![Datum::Boolean(false)]));
    assert_parsed!(datum, expected);
}

#[test]
fn verbose_quotation_multiple_args_error() {
    let datum = list!(
        "quote",
        [
            Datum::Vector(vec_deque![Datum::Boolean(false)]),
            Datum::Boolean(true)
        ]
    );
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
    let datum = list!(
        "foobar",
        [
            Datum::Character('a'),
            Datum::Abbreviation {
                kind: AbbreviationKind::Quote,
                datum: Box::new(list!([])),
            }
        ]
    );
    let expected = Expression::Call {
        operator: Box::new(Expression::Variable("foobar".to_string())),
        operands: vec![Expression::Character('a'), Expression::Quotation(list!([]))],
    };
    assert_parsed!(datum, expected);
}

#[test]
fn short_if_expression() {
    let datum = list!(
        "if",
        [symbol!("foobar"), list!("some_fn", [Datum::Boolean(true)])]
    );
    let expected = Expression::Conditional {
        test: Box::new(Expression::Variable("foobar".to_string())),
        consequent: Box::new(Expression::Call {
            operator: Box::new(Expression::Variable("some_fn".to_string())),
            operands: vec![Expression::Boolean(true)],
        }),
        alternate: None,
    };
    assert_parsed!(datum, expected);
}

#[test]
fn long_if_expression() {
    let datum = list!(
        "if",
        [
            Datum::Boolean(false),
            Datum::String("foobar".to_string()),
            Datum::Abbreviation {
                kind: AbbreviationKind::Quote,
                datum: Box::new(list!([])),
            }
        ]
    );
    let expected = Expression::Conditional {
        test: Box::new(Expression::Boolean(false)),
        consequent: Box::new(Expression::String("foobar".to_string())),
        alternate: Some(Box::new(Expression::Quotation(list!([])))),
    };
    assert_parsed!(datum, expected);
}

#[test]
fn too_short_if_error() {
    let datum = list!("if", [Datum::Character('a')]);
    assert_parse_err!(datum);
}

#[test]
fn lonely_if_error() {
    let datum = list!("if", []);
    assert_parse_err!(datum);
}

#[test]
fn too_long_if_error() {
    let datum = list!(
        "if",
        [Datum::Character('a'), Datum::Boolean(true), list!([])]
    );
    assert_parse_err!(datum);
}

#[test]
fn assignment() {
    let datum = list!("set!", [symbol!("x"), Datum::Boolean(true)]);
    let expected = Expression::Assignment {
        variable: "x".to_string(),
        expression: Box::new(Expression::Boolean(true)),
    };
    assert_parsed!(datum, expected);
}

#[test]
fn wrong_length_assignment_error() {
    let datum = list!("set!", []);
    assert_parse_err!(datum);

    let datum = list!("set!", [symbol!("x")]);
    assert_parse_err!(datum);

    let datum = list!(
        "set!",
        [
            symbol!("x"),
            Datum::String("sakldfj".to_string()),
            Datum::Boolean(true)
        ]
    );
    assert_parse_err!(datum);
}

#[test]
fn bad_identifier_assignment_error() {
    let datum = list!(
        "set!",
        [
            symbol!("if"),
            Datum::String("sakldfj".to_string()),
            Datum::Boolean(true)
        ]
    );
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
    let expected = LambdaFormals::List(vec!["x".to_string(), "y".to_string()]);
    assert_eq!(parse_lambda_formals_exp(datum), Ok(expected));
}

#[test]
fn lambda_formals_rest() {
    let datum = Datum::Pair {
        car: vec_deque![symbol!("x"), symbol!("y"),],
        cdr: Box::new(symbol!("z")),
    };
    let expected = LambdaFormals::Rest(vec!["x".to_string(), "y".to_string()], "z".to_string());
    assert_eq!(parse_lambda_formals_exp(datum), Ok(expected));
}

#[test]
fn lambda_formals_wrong_identifier_error() {
    let datum = symbol!("if");
    assert!(parse_lambda_formals_exp(datum).is_err());
}

#[test]
fn lambda_formals_no_vars_error() {
    let datum = list!([symbol!("x"), list!([symbol!("y")])]);
    assert!(parse_lambda_formals_exp(datum).is_err());
}

#[test]
fn lambda_formals_wrong_datum_error() {
    let datum = Datum::Vector(vec_deque![symbol!("x"), symbol!("y")]);
    assert!(parse_lambda_formals_exp(datum).is_err());
}

#[test]
fn simple_definition() {
    let datum = list!("define", [symbol!("x"), list!([symbol!("f")])]);
    let expected = Definition::Define {
        variable: "x".to_string(),
        expression: Box::new(Expression::Call {
            operator: Box::new(Expression::Variable("f".to_string())),
            operands: vec![],
        }),
    };
    assert_eq!(parse_definition(datum), Ok(expected));
}

#[test]
fn lambda_definition() {
    let datum = list!(
        "define",
        [list!([symbol!("x"), symbol!("y")]), symbol!("y")]
    );
    let expected = Definition::DefineLambda {
        variable: "x".to_string(),
        formals: LambdaFormals::List(vec!["y".to_string()]),
        body: Body {
            definitions: vec![],
            commands: vec![],
            expression: Box::new(Expression::Variable("y".to_string())),
        },
    };
    assert_eq!(parse_definition(datum), Ok(expected));
}

#[test]
fn begin_definition() {
    let datum = list!(
        "begin",
        [list!("define", [symbol!("x"), Datum::Character('a')])]
    );
    let expected = Definition::Begin(vec![
        Definition::Define {
            variable: "x".to_string(),
            expression: Box::new(Expression::Character('a')),
        },
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
    let datum = list!([symbol!("define"), symbol!("x")]);
    assert!(parse_definition(datum).is_err());

    let datum = list!([
        symbol!("define"),
        symbol!("x"),
        symbol!("y"),
        symbol!("z")
    ]);
    assert!(parse_definition(datum).is_err());
}

#[test]
fn lambda_definition_no_var_error() {
    let datum = list!("define", [list!([]), symbol!("x")]);
    assert!(parse_definition(datum).is_err());
}

#[test]
fn body() {
    let datums = vec_deque![symbol!("x")];
    let expected = Body {
        definitions: vec![],
        commands: vec![],
        expression: Box::new(Expression::Variable("x".to_string())),
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
    let datums = vec_deque![list!("define", [symbol!("x"), symbol!("y")])];
    assert!(parse_body(datums).is_err());
}

#[test]
fn lambda_exp() {
    let datum = list!("lambda", [symbol!("x"), symbol!("x")]);
    let expected = Expression::Lambda {
        formals: LambdaFormals::VarArgs("x".to_string()),
        body: Body {
            definitions: vec![],
            commands: vec![],
            expression: Box::new(Expression::Variable("x".to_string())),
        },
    };

    assert_parsed!(datum, expected);
}

#[test]
fn boolean_expressions() {
    let datum = list!("and", [symbol!("x"), Datum::String("x".to_string())]);
    let expected = Expression::Derived(Derived::And(vec![
        Expression::Variable("x".to_string()),
        Expression::String("x".to_string()),
    ]));
    assert_parsed!(datum, expected);

    let datum = list!("or", [Datum::Boolean(false), symbol!("x")]);
    let expected = Expression::Derived(Derived::Or(vec![
        Expression::Boolean(false),
        Expression::Variable("x".to_string()),
    ]));
    assert_parsed!(datum, expected);
}

#[test]
fn let_exp() {
    let datums = vec_deque![
        list!([list!([symbol!("x"), Datum::Character('a')])]),
        symbol!("y")
    ];
    let expected = (
        vec![
            Binding {
                variable: "x".to_string(),
                init: Box::new(Expression::Character('a')),
            },
        ],
        Body {
            commands: vec![],
            definitions: vec![],
            expression: Box::new(Expression::Variable("y".to_string())),
        },
    );
    assert_eq!(parse_let_exp(datums), Ok(expected));
}

#[test]
fn let_exp_no_bindings_list_error() {
    let datums = vec_deque![symbol!("x"), symbol!("y")];
    assert!(parse_let_exp(datums).is_err());
}

#[test]
fn let_exp_no_body_error() {
    let datums = vec_deque![list!([list!([symbol!("x"), symbol!("y")])]),];
    assert!(parse_let_exp(datums).is_err());
}

#[test]
#[should_panic]
fn let_exp_no_details_error() {
    let datums = vec_deque![];
    assert!(parse_let_exp(datums).is_err());
}

#[test]
fn let_exp_invalid_binding_list_error() {
    let datums = vec_deque![
        list!([
            symbol!("y"),
            list!([Datum::Boolean(true), Datum::Character('a')])
        ]),
        symbol!("x")
    ];
    assert!(parse_let_exp(datums).is_err());
}

#[test]
fn let_exp_invalid_binding_length_error() {
    let datums = vec_deque![list!([list!([symbol!("x")])]), symbol!("x")];
    assert!(parse_let_exp(datums).is_err());

    let datums = vec_deque![
        list!([
            list!([symbol!("x"), Datum::Boolean(true), Datum::Character('a')])
        ]),
        symbol!("x")
    ];
    assert!(parse_let_exp(datums).is_err());
}

#[test]
fn let_exp_invalid_variable_binding_error() {
    let datums = vec_deque![
        list!([list!([Datum::Boolean(true), Datum::Character('a')])]),
        symbol!("x")
    ];
    assert!(parse_let_exp(datums).is_err());
}

#[test]
fn let_exp_invalid_expression_binding_error() {
    let datums = vec_deque![
        list!([list!([Datum::Boolean(true), symbol!("if")])]),
        symbol!("x")
    ];
    assert!(parse_let_exp(datums).is_err());
}

#[test]
fn let_star() {
    let datum = list!(
        "let*",
        [
            list!([list!([symbol!("x"), Datum::Boolean(true)])]),
            symbol!("x")
        ]
    );
    let expected = Expression::Derived(Derived::LetStar {
        bindings: vec![
            Binding {
                variable: "x".to_string(),
                init: Box::new(Expression::Boolean(true)),
            },
        ],
        body: Body {
            commands: vec![],
            definitions: vec![],
            expression: Box::new(Expression::Variable("x".to_string())),
        },
    });
    assert_parsed!(datum, expected);
}

#[test]
fn named_let() {
    let datum = list!(
        "let",
        [
            symbol!("f"),
            list!([list!([symbol!("x"), Datum::Boolean(true)])]),
            symbol!("x")
        ]
    );
    let expected = Expression::Derived(Derived::NamedLet {
        variable: "f".to_string(),
        bindings: vec![
            Binding {
                variable: "x".to_string(),
                init: Box::new(Expression::Boolean(true)),
            },
        ],
        body: Body {
            commands: vec![],
            definitions: vec![],
            expression: Box::new(Expression::Variable("x".to_string())),
        },
    });
    assert_parsed!(datum, expected);
}

#[test]
fn iteration_spec() {
    let datum = list!([symbol!("x"), Datum::Boolean(true)]);
    let expected = IterationSpec {
        variable: "x".to_string(),
        init: Box::new(Expression::Boolean(true)),
        step: None,
    };
    assert_eq!(parse_iteration_spec(datum), Ok(expected));
}

#[test]
fn iteration_spec_with_step() {
    let datum = list!([
        symbol!("x"),
        Datum::Boolean(true),
        Datum::String("dsf".to_string())
    ]);
    let expected = IterationSpec {
        variable: "x".to_string(),
        init: Box::new(Expression::Boolean(true)),
        step: Some(Box::new(Expression::String("dsf".to_string()))),
    };
    assert_eq!(parse_iteration_spec(datum), Ok(expected));
}

#[test]
fn wrong_length_iteration_spec_error() {
    let datum = list!([]);
    assert!(parse_iteration_spec(datum).is_err());

    let datum = list!([symbol!("x")]);
    assert!(parse_iteration_spec(datum).is_err());

    let datum = list!([
        symbol!("x"),
        Datum::Boolean(true),
        Datum::String("dsf".to_string()),
        symbol!("y")
    ]);
    assert!(parse_iteration_spec(datum).is_err());
}

#[test]
fn no_variable_iteration_spec_error() {
    let datum = list!([Datum::Boolean(true), Datum::String("dsf".to_string())]);
    assert!(parse_iteration_spec(datum).is_err());
}

#[test]
fn no_list_iteration_spec_error() {
    let datum = symbol!("x");
    assert!(parse_iteration_spec(datum).is_err());
}

#[test]
fn else_clause() {
    let mut datums = vec_deque![symbol!("x"), list!("else", [symbol!("y")])];
    let expected = (vec![], Box::new(Expression::Variable("y".to_string())));
    assert_eq!(parse_else_clause(&mut datums), Ok(Some(expected)));
}

#[test]
fn long_else_clause() {
    let mut datums = vec_deque![list!("else", [symbol!("y"), Datum::Boolean(true)])];
    let expected = (
        vec![Expression::Variable("y".to_string())],
        Box::new(Expression::Boolean(true)),
    );
    assert_eq!(parse_else_clause(&mut datums), Ok(Some(expected)));
}

#[test]
fn no_else_clause() {
    let mut datums = vec_deque![symbol!("x"),];
    let clone = datums.clone();
    assert_eq!(parse_else_clause(&mut datums), Ok(None));
    assert_eq!(datums, clone);

    let mut datums = vec_deque![list!("foobar", [symbol!["x"]])];
    let clone = datums.clone();
    assert_eq!(parse_else_clause(&mut datums), Ok(None));
    assert_eq!(datums, clone);
}

#[test]
fn empty_else_clause_error() {
    let mut datums = vec_deque![list!("else", [])];
    assert!(parse_else_clause(&mut datums).is_err());
}

#[test]
fn bad_expression_else_clause_error() {
    let mut datums = vec_deque![symbol!("x"), list!("else", [list!("if", [])])];
    assert!(parse_else_clause(&mut datums).is_err());
}

#[test]
fn cond_clause() {
    let datum = list!([symbol!("x"), symbol!("y"), Datum::Boolean(true)]);
    let expected = CondClause::Normal {
        test: Box::new(Expression::Variable("x".to_string())),
        expressions: vec![
            Expression::Variable("y".to_string()),
            Expression::Boolean(true),
        ],
    };
    assert_eq!(parse_cond_clause_exp(datum), Ok(expected));
}

#[test]
fn expressionless_cond_clause() {
    let datum = list!([symbol!("x"), symbol!("=>"), symbol!("y")]);
    let expected = CondClause::Arrow {
        test: Box::new(Expression::Variable("x".to_string())),
        recipient: Box::new(Expression::Variable("y".to_string())),
    };
    assert_eq!(parse_cond_clause_exp(datum), Ok(expected));
}

#[test]
fn arrow_cond_clause() {
    let datum = list!([symbol!("x")]);
    let expected = CondClause::Normal {
        test: Box::new(Expression::Variable("x".to_string())),
        expressions: vec![],
    };
    assert_eq!(parse_cond_clause_exp(datum), Ok(expected));
}

#[test]
fn empty_cond_clause_error() {
    let datum = list!([]);
    assert!(parse_cond_clause_exp(datum).is_err());
}

#[test]
fn no_list_clause_error() {
    let datum = Datum::Boolean(true);
    assert!(parse_cond_clause_exp(datum).is_err());
}

#[test]
fn too_many_args_arrow_clause_error() {
    let datum = list!([
        symbol!("x"),
        symbol!("=>"),
        symbol!("y"),
        Datum::String("foobar".to_string())
    ]);
    assert!(parse_cond_clause_exp(datum).is_err());
}

#[test]
fn case_clause() {
    let datum = list!([
        list!([symbol!("a"), Datum::Vector(vec_deque![])]),
        symbol!("y")
    ]);
    let expected = CaseClause {
        datums: vec_deque![symbol!("a"), Datum::Vector(vec_deque![])],
        commands: vec![],
        expression: Box::new(Expression::Variable("y".to_string())),
    };
    assert_eq!(parse_case_clause_exp(datum), Ok(expected));
}

#[test]
fn case_clause_no_list_error() {
    let datum = symbol!("a");
    assert!(parse_case_clause_exp(datum).is_err());
}

#[test]
fn case_clause_short_list_error() {
    let datum = list!([list!([])]);
    assert!(parse_case_clause_exp(datum).is_err());
}

#[test]
fn case_clause_no_datum_list_error() {
    let datum = list!([Datum::Boolean(true), symbol!("y")]);
    assert!(parse_case_clause_exp(datum).is_err());
}

#[test]
fn cond() {
    let datum = list!(
        "cond",
        [
            list!([
                symbol!("a"),
                symbol!("=>"),
                Datum::String("foo".to_string())
            ])
        ]
    );
    let expected = Expression::Derived(Derived::Cond {
        head_clause: CondClause::Arrow {
            test: Box::new(Expression::Variable("a".to_string())),
            recipient: Box::new(Expression::String("foo".to_string())),
        },
        tail_clauses: vec![],
    });
    assert_parsed!(datum, expected);
}

#[test]
fn empty_cond_error() {
    let datum = list!("cond", []);
    assert_parse_err!(datum);
}

#[test]
fn invalid_case_error() {
    let datum = list!("case", []);
    assert_parse_err!(datum);

    let datum = list!("case", [symbol!("x")]);
    assert_parse_err!(datum);
}

#[test]
fn delay() {
    let datum = list!("delay", [symbol!("x")]);
    let expected = Expression::Derived(Derived::Delay(
        Box::new(Expression::Variable("x".to_string())),
    ));
    assert_parsed!(datum, expected);
}

#[test]
fn empty_delay_error() {
    let datum = list!("delay", []);
    assert_parse_err!(datum);
}

#[test]
fn too_many_args_delay_error() {
    let datum = list!("delay", [symbol!("x"), symbol!("y")]);
    assert_parse_err!(datum);
}
