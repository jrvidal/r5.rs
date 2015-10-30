use ::parse::token::{NumberToken};
use super::datum::{Datum, AbbreviationKind};
use ::parse::keywords;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    // max(String, bool, char, NumberToken, Datum, Vec)
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
    Derived(Derived),
    MacroUse {
        keyword: String,
        datum: Vec<Datum>
    },
    // TO DO: macro block
    MacroBlock,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Derived {
    Cond {
        head_clause: CondClause,
        tail_clauses: Vec<CondClause>
    },
    CondElse {
        clauses: Vec<CondClause>,
        else_commands: Vec<Expression>,
        else_expression: Box<Expression>
    },
    Case {
        key: Box<Expression>,
        head_clause: CaseClause,
        tail_clauses: Vec<CaseClause>
    },
    CaseElse {
        key: Box<Expression>,
        clauses: Vec<CaseClause>,
        else_commands: Vec<Expression>,
        else_expression: Box<Expression>
    },
    And(Vec<Expression>),
    Or(Vec<Expression>),
    Let {
        bindings: Vec<Binding>,
        body: Body
    },
    NamedLet {
        variable: String,
        bindings: Vec<Binding>,
        body: Body
    },
    LetStar {
        bindings: Vec<Binding>,
        body: Body
    },
    LetRec {
        bindings: Vec<Binding>,
        body: Body,
    },
    Begin {
        commands: Vec<Expression>,
        expression: Box<Expression>
    },
    Do {
        iterations: Vec<IterationSpec>,
        test: Box<Expression>,
        result: Vec<Expression>,
        commands: Vec<Expression>
    },
    Delay(Box<Expression>),
    // Quasiquotation is a the top level
}

#[derive(Debug, PartialEq, Clone)]
pub struct IterationSpec {
    variable: String,
    init: Box<Expression>,
    step: Box<Option<Expression>>
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binding {
    variable: String,
    init: Box<Expression>
}

#[derive(Debug, PartialEq, Clone)]
pub enum Definition {
    Define {
        variable: String,
        expression: Box<Expression>
    },
    DefineLambda {
        variable: String,
        formals_car: Vec<String>,
        formals_cdr: Option<String>,
        body: Body
    },
    Begin(Vec<Definition>)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Body {
    definitions: Vec<Definition>,
    commands: Vec<Expression>,
    expression: Box<Expression>
}

#[derive(Debug, PartialEq, Clone)]
pub enum CondClause {
    Normal {
        test: Box<Expression>,
        commands: Vec<Expression>,
        expression: Box<Expression>
    },
    Arrow {
        test: Box<Expression>,
        expression: Box<Expression>
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CaseClause {
    datums: Vec<Datum>,
    commands: Vec<Expression>,
    expression: Box<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub enum LambdaFormals {
    VarArgs(String),
    List(Vec<String>),
    // For rest params, vec.len() *must* be >= 1
    Rest(Vec<String>, String)
}

trait IntoExpressions {
    fn into_expressions(self) -> Result<Vec<Expression>, ()>;
}

impl IntoExpressions for Vec<Datum> {
    fn into_expressions(self) -> Result<Vec<Expression>, ()> {
        self.into_iter()
            .map(parse_expression)
            .collect::<Result<Vec<Expression>, ()>>()
    }
}

pub fn parse_expression(d: Datum) -> Result<Expression, ()> {
    // Simple cases
    let (head, mut datums) = match d {
        Datum::Symbol(s, false) => return Ok(Expression::Variable(s)),
        // TO DO: why doesn't this work??
        // Datum::Symbol(s) if keywords::is_syntactic_keyword(&s) => {
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
        Datum::Symbol(s, true) => s,
        _ => return parse_call_exp(head, datums)
    };

    match (&symbol[..], datums.len()) {
        // Verbose quotations
        (keywords::QUOTE, 1) => return Ok(Expression::Quotation(datums.remove(0))),
        // Verbose quasiquotations
        (keywords::QUASIQUOTE, 1) => return parse_quasiquotation(datums.remove(0)),
        // If
        (keywords::IF, 2) | (keywords::IF, 3) => datums.into_expressions().map(|mut exprs| {
            let test = exprs.remove(0);
            let consequent = exprs.remove(0);
            let alternate = exprs.pop();
            Expression::Conditional {
                test: Box::new(test),
                consequent: Box::new(consequent),
                alternate: alternate.map(|alt| {Box::new(alt)})
            }
        }),
        // Assignment
        (keywords::SET_BANG, 2) => Ok({
            let parsed = parse_expression(datums.pop().unwrap());
            let var = datums.pop().unwrap();
            match (var, parsed) {
                (Datum::Symbol(s, false), Ok(expr)) => Expression::Assignment {
                    variable: s,
                    expression: Box::new(expr)
                },
                _ => return Err(())
            }
        }),

        // Lambda
        (keywords::LAMBDA, l) if l >= 2 => return parse_lambda_exp(datums),

        (keywords::COND, l) if l >= 1 => return parse_cond_exp(datums),
        (keywords::CASE, l) if l >= 2 => return parse_case_exp(datums),
        (keywords::LET, l) if l >= 2 => match datums.remove(0) {
            Datum::Symbol(s, false) => parse_let_exp(datums).map(|details| {
                Expression::Derived(Derived::NamedLet {
                    variable: s,
                    bindings: details.0,
                    body: details.1
                })
            }),
            d @ _ => {
                datums.insert(0, d);
                parse_let_exp(datums).map(|details| {
                    Expression::Derived(Derived::Let {
                        bindings: details.0,
                        body: details.1
                    })
                })
            }
        },
        (keywords::LETREC, l) if l >= 2 => parse_let_exp(datums).map(|details| {
            Expression::Derived(Derived::LetRec {
                bindings: details.0,
                body: details.1
            })
        }),
        (keywords::LET_STAR, l) if l >= 2 => parse_let_exp(datums).map(|details| {
            Expression::Derived(Derived::LetStar {
                bindings: details.0,
                body: details.1
            })
        }),
        // TO DO: Do
        (keywords::AND, _) =>  datums.into_expressions()
                                .map(|exprs| {
                                    Expression::Derived(Derived::And(exprs))
                                }),
        (keywords::OR, _) => datums.into_expressions()
                                .map(|exprs| {
                                    Expression::Derived(Derived::Or(exprs))
                                }),
        (keywords::BEGIN, l) if l >= 2 => datums.into_expressions()
                                .map(|mut exprs| {
                                    let expression = exprs.pop().unwrap();
                                    Expression::Derived(Derived::Begin {
                                        commands: exprs,
                                        expression: Box::new(expression),
                                    })
                                }),
        (keywords::DELAY, 1) => parse_expression(datums.remove(0))
                                    .map(|expr| {
                                        Expression::Derived(Derived::Delay(Box::new(expr)))
                                    }),
        _ => return Err(())
    }
}

fn parse_cond_exp(datums: Vec<Datum>) -> Result<Expression, ()> {
    unimplemented!();
}

fn parse_case_exp(datums: Vec<Datum>) -> Result<Expression, ()> {
    unimplemented!();
}

fn parse_let_exp(datums: Vec<Datum>) -> Result<(Vec<Binding>, Body), ()> {
    unimplemented!();
}

fn parse_call_exp(operator: Datum, operands: Vec<Datum>) -> Result<Expression, ()> {
    match (parse_expression(operator), operands.into_expressions()) {
        (Ok(exp), Ok(exps)) => Ok(Expression::Call {
            operator: Box::new(exp),
            operands: exps
        }),
        _ => Err(())
    }
}

macro_rules! ask_for_vars {
    ($x:expr) => ({
        let mut v = vec![];
        let l = $x.len();
        for d in $x.into_iter() {
            match d {
                Datum::Symbol(s, false) => v.push(s),
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
        Datum::Symbol(s, false) => LambdaFormals::VarArgs(s),
        Datum::Pair {
            car, cdr
        } => match (ask_for_vars!(car), *cdr) {
            (Ok(vars), Datum::Symbol(s, false)) => LambdaFormals::Rest(vars, s),
            _ => return Err(())
        },
        Datum::List(dats) => match ask_for_vars!(dats) {
            Ok(vars) => LambdaFormals::List(vars),
            _ => return Err(())
        },
        _ => return Err(())
    };
    panic!("{:?}", formals);
}

// TO DO: does this method really take 1 datum? The grammar suggests so.
fn parse_quasiquotation(datum: Datum) -> Result<Expression, ()> {
    println!("{:?}", datum);
    unimplemented!();
}


#[cfg(test)]
mod test {
    use super::*;
    use ::parse::ast::{datum as datum_mod};
    use ::parse::ast::datum::{Datum, AbbreviationKind};

    #[test]
    fn parse_self_evaluating_test() {
        let datum = datum_mod::symbol_for("foobar".to_string());
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
        let datum = datum_mod::symbol_for("letrec".to_string());
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
            car: vec![datum_mod::symbol_for("foobar".to_string())],
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
            datum_mod::symbol_for("quote".to_string()),
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
            datum_mod::symbol_for("quote".to_string()),
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
            datum_mod::symbol_for("quote".to_string()),
        ]);
        assert!(parse_expression(datum).is_err());
    }

    #[test]
    fn parse_call_expression_test() {
        let datum = Datum::List(vec![
            datum_mod::symbol_for("foobar".to_string()),
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