use ::parse::token::{NumberToken};
use super::datum::{Datum, AbbreviationKind};
use ::parse::keywords;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Variable(String),

    Boolean(bool),
    Character(char),
    String(String),
    // TO DO: are we going to store the real number or this?
    Number(NumberToken),
    Quotation(Datum),

    Call {
        operator: Box<Expression>,
        operands: Vec<Expression>
    },
    Lambda {
        formals: LambdaFormals,
        // TO DO: Definitions
        commands: Vec<Expression>,
        expression: Box<Expression>,
    },
    Conditional {
        test: Box<Expression>,
        consequent: Box<Expression>,
        alternate: Option<Box<Expression>>,
    },
    Assignment {
        variable: String,
        expression: Box<Expression>,
    },
    // TO DO: derived expression
    Derived,
    MacroUse {
        keyword: String,
        datum: Vec<Datum>
    },
    // TO DO: macro block
    MacroBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LambdaFormals {
    VarArgs(String),
    List(Vec<String>),
    // For rest params, vec.len() *must* be >= 1
    Rest(Vec<String>, String)
}

macro_rules! one_of {
    ($x:expr, [$c:expr]) => ($x == $c);
    ($x:expr, [ $c:expr, $( $d:expr ),* ]) => (
        $x == $c || one_of!($x, [$( $d ),* ])
    )
}

fn is_syntactic_keyword(name: &str) -> bool {
    match name.len() {
        2 => one_of!(name, [keywords::IF, keywords::OR, keywords::ARROW, keywords::DO]),
        3 => one_of!(name, [keywords::AND, keywords::LET]),
        4 => one_of!(name, [keywords::ELSE, keywords::SET_BANG, keywords::COND, keywords::CASE, keywords::LET_STAR]),
        5 => one_of!(name, [keywords::QUOTE, keywords::BEGIN, keywords::DELAY]),
        6 => one_of!(name, [keywords::DEFINE, keywords::LAMBDA, keywords::LETREC]),
        7 => one_of!(name, [keywords::UNQUOTE]),
        10 => one_of!(name, [keywords::QUASIQUOTE]),
        16 => one_of!(name, [keywords::UNQUOTE_SPLICING]),
        _ => false
    }
}

pub fn parse_expression(d: Datum) -> Result<Expression, ()> {
    // Simple cases
    let (head, mut datums) = match d {
        Datum::Symbol(s) => {
            if is_syntactic_keyword(&s) {
                return Err(());
            } else {
                return Ok(Expression::Variable(s))
            }
        },
        // TO DO: why doesn't this work??
        // Datum::Symbol(s) if is_syntactic_keyword(&s) => {
        //         ret_val!(Expression::Variable(s))
        // },
        Datum::Boolean(b) => return Ok(Expression::Boolean(b)),
        Datum::Character(c) => return Ok(Expression::Character(c)),
        Datum::String(s) => return Ok(Expression::String(s)),
        Datum::Number(nt) => return Ok(Expression::Number(nt)),

        Datum::Abbreviation {
            kind: AbbreviationKind::Quote,
            datum: dat
        } => return Ok(Expression::Quotation(*dat)),

        Datum::Abbreviation {
            kind: AbbreviationKind::Quasiquote,
            datum,
        } => return parse_quasiquotation(*datum),

        // Delegate
        Datum::List(mut datums) => {
            if datums.len() == 0 {
                return Err(());
            }
            let head = datums.remove(0);
            (head, datums)
        },

        _ => return Err(())
    };

    // Discard function calls, extract symbol
    let symbol = match head {
        Datum::Symbol(s) =>  {
            if is_syntactic_keyword(&s) {
                s
            } else {
                return parse_call_exp(Datum::Symbol(s), datums);
            }
        },
        _ => return parse_call_exp(head, datums)
    };

    match (&symbol[..], datums.len()) {
        // Verbose quotations
        (keywords::QUOTE, 1) => return Ok(Expression::Quotation(datums.remove(0))),
        // Verbose quasiquotations
        (keywords::QUASIQUOTE, 1) => return parse_quasiquotation(datums.remove(0)),
        // If
        (keywords::IF, 2) | (keywords::IF, 3) => return Ok(match consume_expressions(datums, 2, Some(3)) {
            Ok(mut exprs) => {
                let test = exprs.remove(0);
                let consequent = exprs.remove(0);
                let alternate = if exprs.len() == 1 {
                    Some(exprs.remove(0))
                } else {
                    None
                };
                Expression::Conditional {
                    test: Box::new(test),
                    consequent: Box::new(consequent),
                    alternate: alternate.map(|alt| {Box::new(alt)})
                }
            },
            Err(()) => return Err(()),
        }),
        // Assignment
        (keywords::SET_BANG, 2) => return Ok({
            let var = datums.remove(0);
            match (var, consume_expressions(datums, 1, Some(1))) {
                (Datum::Symbol(s), Ok(mut exprs)) => Expression::Assignment {
                    variable: s,
                    expression: Box::new(exprs.remove(0))
                },
                _ => return Err(())
            }
        }),
        // Lambda
        (keywords::LAMBDA, l) if l >= 2 => return parse_lambda_exp(datums),
        _ => return Err(())
    }
}

fn parse_call_exp(operator: Datum, operands: Vec<Datum>) -> Result<Expression, ()> {
    let parsed_operator = parse_expression(operator);
    let parsed_operands = consume_expressions(operands, 0, None);

    Ok(match (parsed_operator, parsed_operands) {
        (Ok(exp), Ok(exps)) => {
            Expression::Call {
                operator: Box::new(exp),
                operands: exps
            }
        },
        _ => return Err(())
    })
}

macro_rules! ask_for_vars {
    ($x:expr) => ({
        let mut v = vec![];
        let l = $x.len();
        for d in $x.into_iter() {
            match d {
                Datum::Symbol(s) => v.push(s),
                _ => break
            }
        }
        if v.len() == l {
            Ok(v)
        } else {
            Err(())
        }
    })
}

fn parse_lambda_exp(mut datums: Vec<Datum>) -> Result<Expression, ()> {
    let formals = match datums.remove(0) {
        Datum::Symbol(s) => LambdaFormals::VarArgs(s),
        Datum::Pair {
            car, cdr
        } => match (ask_for_vars!(car), *cdr) {
            (Ok(vars), Datum::Symbol(s)) => LambdaFormals::Rest(vars, s),
            _ => return Err(())
        },
        Datum::List(dats) => match ask_for_vars!(dats) {
            Ok(vars) => LambdaFormals::List(vars),
            _ => return Err(())
        },
        _ => return Err(())
    };
    panic!("");
}

fn consume_expressions(datums: Vec<Datum>, min: usize, max: Option<usize>) -> Result<Vec<Expression>, ()> {
    if datums.len() < min {
        return Err(())
    }
    match max {
        Some(m) => if min > m {
            panic!("Wrong call to consume_expressions");
        } else if datums.len() > m {
            return Err(())
        },
        None => {}
    }

    let mut result = vec![];

    for datum in datums {
        match parse_expression(datum) {
            Ok(exp) => result.push(exp),
            Err(()) => return Err(())
        }
    }

    Ok(result)
}

// TO DO: does this method really take 1 datum? The grammar suggests so.
fn parse_quasiquotation(datum: Datum) -> Result<Expression, ()> {
    println!("{:?}", datum);
    unimplemented!();
}


#[cfg(test)]
mod test {
    use super::*;
    use ::parse::ast::datum::{Datum, AbbreviationKind};


    #[test]
    fn parse_self_evaluating_test() {
        let datum = Datum::Symbol("foobar".to_string());
        assert_eq!(parse_expression(datum), Ok(Expression::Variable("foobar".to_string())));

        let datum = Datum::Boolean(true);
        assert_eq!(parse_expression(datum), Ok(Expression::Boolean(true)));

        let datum = Datum::Character('a');
        assert_eq!(parse_expression(datum), Ok(Expression::Character('a')));

        let datum = Datum::String("foobar".to_string());
        assert_eq!(parse_expression(datum), Ok(Expression::String("foobar".to_string())));
    }

    #[test]
    fn parse_reserved_keywords_test() {
        let datum = Datum::Symbol("letrec".to_string());
        assert!(parse_expression(datum).is_err());
    }

    #[test]
    fn parse_simple_datum_test() {
        let datum = Datum::Abbreviation {
            datum: Box::new(Datum::Boolean(true)),
            kind: AbbreviationKind::Quote
        };
        let expected = Expression::Quotation(Datum::Boolean(true));
        assert_eq!(parse_expression(datum), Ok(expected));
    }

    #[test]
    fn parse_list_datum_test() {
        let pair = Datum::Pair {
            car: vec![Datum::Symbol("foobar".to_string())],
            cdr: Box::new(Datum::Character('a'))
        };
        let datum = Datum::Abbreviation {
            datum: Box::new(pair.clone()),
            kind: AbbreviationKind::Quote
        };
        let expected = Expression::Quotation(pair);
        assert_eq!(parse_expression(datum), Ok(expected));
    }

    #[test]
    fn parse_verbose_datum_test() {
        let datum = Datum::List(vec![
            Datum::Symbol("quote".to_string()),
            Datum::Vector(vec![
                Datum::Boolean(false)
            ])
        ]);
        let expected = Expression::Quotation(Datum::Vector(
            vec![Datum::Boolean(false)],
        ));
        assert_eq!(parse_expression(datum), Ok(expected));
    }

    #[test]
    fn parse_datum_multiple_error() {
        let datum = Datum::List(vec![
            Datum::Symbol("quote".to_string()),
            Datum::Vector(vec![
                Datum::Boolean(false)
            ]),
            Datum::Boolean(true)
        ]);
        assert!(parse_expression(datum).is_err());
    }

    #[test]
    fn parse_datum_none_error() {
        let datum = Datum::List(vec![
            Datum::Symbol("quote".to_string()),
        ]);
        assert!(parse_expression(datum).is_err());
    }

    #[test]
    fn parse_call_expression_test() {
        let datum = Datum::List(vec![
            Datum::Symbol("foobar".to_string()),
            Datum::Character('a'),
            Datum::Abbreviation {
                kind: AbbreviationKind::Quote,
                datum: Box::new(Datum::List(vec![]))
            }
        ]);
        let expected = Expression::Call {
            operator: Box::new(Expression::Variable("foobar".to_string())),
            operands: vec![
                Expression::Character('a'),
                Expression::Quotation(Datum::List(vec![]))
            ]
        };
        assert_eq!(parse_expression(datum), Ok(expected));
    }

    // // Temporary
    // #[test]
    // #[should_panic]
    // fn parse_call_expression_delegation_test() {
    //     let mut stream = tokens_list(&[
    //         Token::Identifier("cond".to_string()),
    //         Token::Character('a'),
    //         Token::SingleQuote,
    //         Token::Open,
    //         Token::Close
    //     ]);
    //     assert!(parse_expression(datum).is_err());
    // }
}