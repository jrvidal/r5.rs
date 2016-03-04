use std::collections::VecDeque;
// use std::iter::IntoIterator;

use ::lexer::{NumberToken};
use ::reader::{Datum, AbbreviationKind};
use ::parser::keywords;
use ::parser::keywords::is_syntactic_keyword;
use ::helpers::*;

#[cfg(test)]
#[path = "expression_test.rs"]
mod test;

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
    QuasiQuotation(Datum),

    Call {
        operator: Box<Expression>,
        operands: Vec<Expression>
    },
    Lambda {
        formals: LambdaFormals,
        body: Body,
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
        datum: VecDeque<Datum>
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
    step: Option<Box<Expression>>
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binding {
    pub variable: String,
    pub init: Box<Expression>
}

#[derive(Debug, PartialEq, Clone)]
pub enum Definition {
    Define {
        variable: String,
        expression: Box<Expression>
    },
    DefineLambda {
        variable: String,
        formals: LambdaFormals,
        body: Body
    },
    Begin(Vec<Definition>)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Body {
    pub definitions: Vec<Definition>,
    pub commands: Vec<Expression>,
    pub expression: Box<Expression>
}

#[derive(Debug, PartialEq, Clone)]
pub enum CondClause {
    Normal {
        test: Box<Expression>,
        expressions: Vec<Expression>
    },
    Arrow {
        test: Box<Expression>,
        recipient: Box<Expression>
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CaseClause {
    datums: VecDeque<Datum>,
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

pub fn parse_expression(d: Datum) -> Result<Expression, ()> {
    // Simple cases
    let (head, mut datums) = match symbol_type(d) {
        (Datum::Symbol(s), Symbol::Variable) => return Ok(Expression::Variable(s)),
        // TO DO: why doesn't this work??
        // Datum::Symbol(s) if keywords::is_syntactic_keyword(&s) => {
        //         ret_val!(Expression::Variable(s))
        // },
        (Datum::Boolean(b), _) => return Ok(Expression::Boolean(b)),
        (Datum::Character(c), _) => return Ok(Expression::Character(c)),
        (Datum::String(s), _) => return Ok(Expression::String(s)),
        (Datum::Number(nt), _) => return Ok(Expression::Number(nt)),

        (Datum::Abbreviation {
            kind: AbbreviationKind::Quote,
            datum: dat
        }, _) => return Ok(Expression::Quotation(*dat)),

        (Datum::Abbreviation {
            kind: AbbreviationKind::Quasiquote,
            datum,
        }, _) => return parse_quasiquotation(*datum),

        // Delegate
        (Datum::List(mut datums), _) => {
            try!((datums.len() > 0).result());
            let head = datums.pop_front().unwrap();
            (head, datums)
        },

        _ => return Err(())
    };

    let symbol = &match keyword_name(head.clone()) {
        Some(s) => s,
        _ => return parse_call_exp(head, datums)
    };

    match (&symbol[..], datums.len()) {
        // Verbose quotations
        (keywords::QUOTE, 1) => Ok(datums.pop_front().map(Expression::Quotation).unwrap()),
        // Verbose quasiquotations
        (keywords::QUASIQUOTE, 1) => Ok(datums.pop_front().map(Expression::QuasiQuotation).unwrap()),
        // If
        (keywords::IF, 2) | (keywords::IF, 3) => datums.into_expressions().map(|mut exprs| {
            let (test, consequent, alternate) = {
                let last = exprs.pop();
                let next_to_last = exprs.pop().unwrap();
                if exprs.len() == 0 {
                    (next_to_last, last.unwrap(), None)
                } else {
                    (exprs.pop().unwrap(), next_to_last, last)
                }
            };
            Expression::Conditional {
                test: Box::new(test),
                consequent: Box::new(consequent),
                alternate: alternate.map(Box::new)
            }
        }),
        // Assignment
        (keywords::SET_BANG, 2) => {
            let var = symbol_type(datums.pop_front().unwrap());
            let parsed = parse_expression(datums.pop_front().unwrap());
            match (var.0, var.1, parsed) {
                (Datum::Symbol(s), Symbol::Variable, Ok(expr)) => Ok(Expression::Assignment {
                    variable: s,
                    expression: Box::new(expr)
                }),
                _ => Err(())
            }
        },

        // Lambda
        (keywords::LAMBDA, l) if l >= 2 => return parse_lambda_exp(datums),

        (keywords::COND, l) if l >= 1 => {
            let else_expressions = parse_else_clause(&mut datums); 

            let cond_clauses = datums
                .into_iter()
                .map(parse_cond_clause_exp)
                .collect::<Result<Vec<CondClause>, ()>>();

            (cond_clauses, else_expressions).result().map(|(mut clauses, else_exprs)| {
                match else_exprs {
                    Some((commands, expr)) => Derived::CondElse {
                        clauses: clauses,
                        else_commands: commands,
                        else_expression: expr
                    },
                    None => {
                        let head = clauses.remove(0);
                        Derived::Cond {
                            head_clause: head,
                            tail_clauses: clauses
                        }
                    }
                }
            }).map(Expression::Derived)
        },
        (keywords::CASE, l) if l >= 2 => {
            let key = Box::new(try![ parse_expression(datums.pop_front().unwrap()) ]);
            let else_expressions = parse_else_clause(&mut datums); 

            let case_clauses = datums
                .into_iter()
                .map(parse_case_clause_exp)
                .collect();

            (case_clauses, else_expressions).result().map(|(mut clauses, else_exprs)| {
                match else_exprs {
                    Some((commands, expr)) => Derived::CaseElse {
                        key: key,
                        clauses: clauses,
                        else_commands: commands,
                        else_expression: expr
                    },
                    None => {
                        let head = clauses.remove(0);
                        Derived::Case {
                            key: key,
                            head_clause: head,
                            tail_clauses: clauses
                        }
                    }
                }
            }).map(Expression::Derived)
        },
        (keywords::LET, l) if l >= 2 => match symbol_type(datums.pop_front().unwrap()) {
            (Datum::Symbol(s), Symbol::Variable) => parse_let_exp(datums).map(|details| {
                Derived::NamedLet {
                    variable: s,
                    bindings: details.0,
                    body: details.1
                }
            }),
            (d, _) => {
                datums.insert(0, d);
                parse_let_exp(datums).map(|details| {
                    Derived::Let {
                        bindings: details.0,
                        body: details.1
                    }
                })
            }
        }.map(Expression::Derived),
        (keywords::LETREC, l) if l >= 2 => parse_let_exp(datums).map(|details| {
            Derived::LetRec {
                bindings: details.0,
                body: details.1
            }
        }).map(Expression::Derived),
        (keywords::LET_STAR, l) if l >= 2 => parse_let_exp(datums).map(|details| {
            Derived::LetStar {
                bindings: details.0,
                body: details.1
            }
        }).map(Expression::Derived),
        (keywords::DO, l) if l >= 2 => {
            let specs = try![ datums
                .pop_front().unwrap()
                .list().ok_or(())
                .and_then(|dats| dats.into_iter().map(parse_iteration_spec).collect())
            ];

            let mut list = try![datums.pop_front().unwrap().list().ok_or(())];
            try![ (list.len() > 0).result() ];
            let test = list.pop_front().map(parse_expression).unwrap().map(Box::new);
            let do_result = list.into_expressions();

            let commands = datums.into_expressions();

            (test, do_result, commands).result().map(|(test_exp, do_res_exp, comms)| {
                Expression::Derived(Derived::Do {
                    iterations: specs,
                    test: test_exp,
                    result: do_res_exp,
                    commands: comms
                })
            })

        },
        (keywords::AND, _) =>  datums.into_expressions()
                                .map(Derived::And)
                                .map(Expression::Derived),

        (keywords::OR, _) => datums.into_expressions()
                                .map(Derived::Or)
                                .map(Expression::Derived),

        (keywords::BEGIN, l) if l >= 2 => datums.into_expressions()
                                .map(|mut exprs| {
                                    let expression = exprs.pop().unwrap();
                                    Expression::Derived(Derived::Begin {
                                        commands: exprs,
                                        expression: Box::new(expression),
                                    })
                                }),
        (keywords::DELAY, 1) => parse_expression(datums.pop_front().unwrap())
                                    .map(Box::new)
                                    .map(Derived::Delay)
                                    .map(Expression::Derived),
        _ => return Err(())
    }
}


//
// Subexpressions
//
fn parse_else_clause(datums: &mut VecDeque<Datum>) -> Result<Option<(Vec<Expression>, Box<Expression>)>, ()> {
    let mut else_clause = match datums.back().cloned() {
        Some(Datum::List(l)) => if l.len() > 0 { l } else { return Ok(None); },
        _ => return Ok(None)
    };

    match else_clause.pop_front().map(keyword_name).unwrap() {
        Some(ref s) if &s[..] == keywords::ELSE => {},
        _ => return Ok(None)
    }

    datums.pop_back();

    try![ (else_clause.len() > 0).result() ];

    else_clause.into_expressions().map(|mut exprs| {
        let main = exprs.pop().unwrap();
        (exprs, Box::new(main))
    }).map(Some)
}

fn parse_cond_clause_exp(datum: Datum) -> Result<CondClause, ()> {
    let mut list = try![ datum.list().ok_or(()) ];

    try![(list.len() > 0).result()];

    let test = try![ list.pop_front().map(parse_expression).unwrap().map(Box::new) ];

    match (list.get(0).cloned().map(keyword_name), list.len()) {
        (Some(Some(ref s)), 2) if s == keywords::ARROW => {
            list.pop_back().map(parse_expression).unwrap().map(|exp| {
                CondClause::Arrow {
                    test: test,
                    recipient: Box::new(exp)
                }
            })
        },
        _ => list.into_expressions()
                .map(|exprs| {
                    CondClause::Normal {
                        test: test,
                        expressions: exprs
                    }
                })
    }
}

fn parse_case_clause_exp(datum: Datum) -> Result<CaseClause, ()> {
    let mut list = try![ datum.list().ok_or(()) ];

    try![ (list.len() >= 2).result() ];

    let datums_list = try![ list.pop_front().unwrap().list().ok_or(()) ];

    list.into_expressions().map(|mut exprs| {
        let expression = exprs.pop().unwrap();
        CaseClause {
            datums: datums_list,
            expression: Box::new(expression),
            commands: exprs,
        }
    })
}

// panics unless datums.len() >= 1
fn parse_let_exp(mut datums: VecDeque<Datum>) -> Result<(Vec<Binding>, Body), ()> {
    let binding_list = try![ datums.pop_front().unwrap().list().ok_or(()) ];
    let body = parse_body(datums);
    let bindings = binding_list.into_iter().map(|b| {
        let mut pair = try![ b.list().ok_or(()) ];
        try![ (pair.len() == 2).result() ];

        let head = pair.pop_front().unwrap();
        let expression = pair.pop_front().unwrap();

        (parse_variable(head), parse_expression(expression))
            .result()
            .map(|(var, exp)| {
                Binding {
                    variable: var,
                    init: Box::new(exp)
                }
            })
    })
        .collect();

    (bindings, body).result()
}

fn parse_call_exp(operator: Datum, operands: VecDeque<Datum>) -> Result<Expression, ()> {
    (parse_expression(operator), operands.into_expressions()).result().map(|(exp, exprs)| {
        Expression::Call {
            operator: Box::new(exp),
            operands: exprs
        }
    })
}

fn parse_lambda_exp(mut datums: VecDeque<Datum>) -> Result<Expression, ()> {
    let formals = datums.pop_front().unwrap();

    (parse_lambda_formals_exp(formals), parse_body(datums)).result().map(|(parsed_formals, body)| {
        Expression::Lambda {
            formals: parsed_formals,
            body: body
        }
    })
}

fn parse_body(mut datums: VecDeque<Datum>) -> Result<Body, ()> {
    let definitions = parse_definitions(&mut datums);

    try![(datums.len() > 0).result()];

    datums.into_expressions().map(|mut exprs| {
        let expression = exprs.pop().unwrap();
        Body {
            definitions: definitions,
            commands: exprs,
            expression: Box::new(expression)
        }
    })
}

fn parse_definition(datum: Datum) -> Result<Definition, ()> {
    let mut list = try![ datum.list().ok_or(()) ];

    try![ (list.len() > 0).result() ];

    let symbol = try![ keyword_name(list.pop_front().unwrap()).ok_or(()) ];

    match &symbol[..] {
        keywords::BEGIN => list.into_iter()
                .map(parse_definition)
                .collect::<Result<Vec<Definition>, ()>>()
                .map(Definition::Begin),

        keywords::DEFINE if list.len() >= 2 => match {
            let formals = list.pop_front().map(parse_lambda_formals_exp).unwrap();
            (formals, list.len())
        } {
            (Ok(LambdaFormals::VarArgs(s)), 1) => list.pop_front().map(parse_expression).unwrap().map(|exp| {
                Definition::Define {
                    variable: s,
                    expression: Box::new(exp)
                }
            }),
            (Ok(LambdaFormals::List(mut args)), _) => {
                try![ (args.len() > 0).result() ];

                let var = args.remove(0);
                parse_body(list).map(|body| {
                    Definition::DefineLambda {
                        variable: var,
                        formals: LambdaFormals::List(args),
                        body: body
                    }
                })
            },
            (Ok(LambdaFormals::Rest(mut args, rest)), _) => parse_body(list).map(|body| {
                let (var, formals) = {
                    let v = args.remove(0);
                    (v, if args.len() == 0 {
                        LambdaFormals::VarArgs(rest)
                    } else {
                        LambdaFormals::Rest(args, rest)
                    })
                };
                Definition::DefineLambda {
                    variable: var,
                    formals: formals,
                    body: body,
                }
            }),
            _ => Err(())

        },
        _ => Err(())
    }
}

fn parse_lambda_formals_exp(datum: Datum) -> Result<LambdaFormals, ()> {
    match symbol_type(datum) {
        (Datum::List(l), _) => l.into_variables().map(LambdaFormals::List),
        (Datum::Pair {
            car, cdr
        }, _) => (car.into_variables(), parse_variable(*cdr)).result().map(|(vars, rest)| {
                LambdaFormals::Rest(vars, rest)
        }),
        (Datum::Symbol(s), Symbol::Variable) => Ok(LambdaFormals::VarArgs(s)),
        _ => Err(())
    }
}

fn parse_definitions(datums: &mut VecDeque<Datum>) -> Vec<Definition> {
    let mut definitions = vec![];

    while let Ok(def) = datums.get(0).cloned().ok_or(()).and_then(parse_definition) {
        definitions.push(def);
        datums.pop_front();
    }

    definitions
}

fn parse_variable(datum: Datum) -> Result<String, ()> {
    parse_expression(datum).and_then(|exp| {
        if let Expression::Variable(s) = exp {
            Ok(s)
        } else {
            Err(())
        }
    })
}

fn parse_iteration_spec(datum: Datum) -> Result<IterationSpec, ()> {
    let mut list = try![ datum.list().ok_or(()) ];
    try![ (list.len() == 2 || list.len() == 3).result() ];

    let variable = list.pop_front().map(parse_variable).unwrap();
    let init = list.pop_front().map(parse_expression).unwrap().map(Box::new);
    let step = list.pop_front().map_or(Ok(None), |dat| {
        parse_expression(dat).map(Box::new).map(Some)
    });

    (variable, init, step).result().map(|(var, init_exp, step_exp)| {
        IterationSpec {
            variable: var,
            init: init_exp,
            step: step_exp
        }
    })
}

// TO DO: does this method really take 1 datum? The grammar suggests so.
fn parse_quasiquotation(datum: Datum) -> Result<Expression, ()> {
    println!("{:?}", datum);
    unimplemented!();
}


//
// Helpers
//
enum Symbol {
    Variable,
    Keyword,
    None
}

fn symbol_type(d: Datum) -> (Datum, Symbol) {
    match d {
        Datum::Symbol(s) => if is_syntactic_keyword(&s[..]) {
            (Datum::Symbol(s), Symbol::Keyword)
        } else {
            (Datum::Symbol(s), Symbol::Variable)
        },
        _ => (d, Symbol::None)
    }
}

fn keyword_name(d: Datum) -> Option<String> {
    match symbol_type(d) {
        (Datum::Symbol(s), Symbol::Keyword) => Some(s),
        _ => None
    }
}

trait IntoHelper {
    fn into_expressions(self) -> Result<Vec<Expression>, ()>;
    fn into_variables(self) -> Result<Vec<String>, ()>;
    // fn into_keyword_list(self, keyword: &str) -> Result<Vec<Expression>, ()>;
}

impl IntoHelper for VecDeque<Datum> {
    fn into_expressions(self) -> Result<Vec<Expression>, ()> {
        self.into_iter()
            .map(parse_expression)
            .collect()
    }

    fn into_variables(self) -> Result<Vec<String>, ()> {
        self.into_iter()
            .map(parse_variable)
            .collect()
    }

    // fn into_keyword_list(mut self, keyword: &str) -> Result<Vec<Expression>, ()> {
    //     if self.len() == 0 {
    //         return Err(());
    //     }
    //     match symbol_type(self[0].clone()) {
    //         (Datum::Symbol(ref s), Symbol::Keyword) if s == keyword => {
    //             self.pop_front();
    //             self.into_expressions()
    //         },
    //         _ => Err(())
    //     }
    // }
}