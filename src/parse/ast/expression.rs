use std::collections::VecDeque;
use ::parse::token::{NumberToken};
use super::datum::{Datum, AbbreviationKind};
use ::parse::keywords;
use ::parse::keywords::is_syntactic_keyword;

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
        formals: LambdaFormals,
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

trait IntoExpressions {
    fn into_expressions(self) -> Result<Vec<Expression>, ()>;
}

trait IntoVariables {
    fn into_variables(self) -> Result<Vec<String>, ()>;
}

impl IntoExpressions for VecDeque<Datum> {
    fn into_expressions(self) -> Result<Vec<Expression>, ()> {
        self.into_iter()
            .map(parse_expression)
            .collect::<Result<Vec<Expression>, ()>>()
    }
}

impl IntoVariables for VecDeque<Datum> {
    fn into_variables(self) -> Result<Vec<String>, ()> {
        self.into_iter()
            .map(parse_expression)
            .map(|exp| match exp {
                Ok(Expression::Variable(s)) => Ok(s),
                _ => Err(())
            })
            .collect::<Result<Vec<String>, ()>>()
    }
}


enum Symbol {
    Variable,
    Keyword,
    None
}

impl Symbol {
    fn from_datum(d: Datum) -> (Datum, Symbol) {
        match d {
            Datum::Symbol(s) => if is_syntactic_keyword(&s[..]) {
                (Datum::Symbol(s), Symbol::Keyword)
            } else {
                (Datum::Symbol(s), Symbol::Variable)
            },
            _ => (d, Symbol::None)
        }
    }
}

pub fn parse_expression(d: Datum) -> Result<Expression, ()> {
    // Simple cases
    let (head, mut datums) = match Symbol::from_datum(d) {
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
            if datums.len() == 0 {
                return Err(());
            }
            let head = datums.pop_front().unwrap();
            (head, datums)
        },

        _ => return Err(())
    };

    // Discard function calls, extract symbol
    let symbol = match Symbol::from_datum(head) {
        (Datum::Symbol(s), Symbol::Keyword) => s,
        (d, _) => return parse_call_exp(d, datums)
    };

    match (&symbol[..], datums.len()) {
        // Verbose quotations
        (keywords::QUOTE, 1) => return Ok(Expression::Quotation(datums.pop_front().unwrap())),
        // Verbose quasiquotations
        (keywords::QUASIQUOTE, 1) => return parse_quasiquotation(datums.pop_front().unwrap()),
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
                alternate: alternate.map(|alt| {Box::new(alt)})
            }
        }),
        // Assignment
        (keywords::SET_BANG, 2) => {
            let var = Symbol::from_datum(datums.pop_front().unwrap());
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

        (keywords::COND, l) if l >= 1 => return parse_cond_exp(datums),
        (keywords::CASE, l) if l >= 2 => return parse_case_exp(datums),
        (keywords::LET, l) if l >= 2 => match Symbol::from_datum(datums.pop_front().unwrap()) {
            (Datum::Symbol(s), Symbol::Variable) => parse_let_exp(datums).map(|details| {
                Expression::Derived(Derived::NamedLet {
                    variable: s,
                    bindings: details.0,
                    body: details.1
                })
            }),
            (d @ _, _) => {
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
        (keywords::DELAY, 1) => parse_expression(datums.pop_front().unwrap())
                                    .map(|expr| {
                                        Expression::Derived(Derived::Delay(Box::new(expr)))
                                    }),
        _ => return Err(())
    }
}

fn parse_cond_exp(datums: VecDeque<Datum>) -> Result<Expression, ()> {
    unimplemented!();
}

fn parse_case_exp(datums: VecDeque<Datum>) -> Result<Expression, ()> {
    unimplemented!();
}

fn parse_let_exp(datums: VecDeque<Datum>) -> Result<(Vec<Binding>, Body), ()> {
    unimplemented!();
}

fn parse_call_exp(operator: Datum, operands: VecDeque<Datum>) -> Result<Expression, ()> {
    match (parse_expression(operator), operands.into_expressions()) {
        (Ok(exp), Ok(exps)) => Ok(Expression::Call {
            operator: Box::new(exp),
            operands: exps
        }),
        _ => Err(())
    }
}

fn parse_lambda_exp(mut datums: VecDeque<Datum>) -> Result<Expression, ()> {
    let formals = datums.pop_front().unwrap();

    match (parse_lambda_formals_exp(formals), parse_body(datums)) {
        (Ok(parsed_formals), Ok(body)) => Ok(Expression::Lambda {
            formals: parsed_formals,
            body: body
        }),
        _ => Err(())
    }
}

fn parse_body(mut datums: VecDeque<Datum>) -> Result<Body, ()> {
    if datums.len() == 0 {
        return Err(());
    }

    let definitions = parse_definitions(&mut datums);

    if datums.len() == 0 {
        return Err(());
    }

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
    let mut list = match datum {
        Datum::List(l) => l,
        _ => return Err(())
    };

    if list.len() == 0 {
        return Err(());
    }

    let symbol = match Symbol::from_datum(list.pop_front().unwrap()) {
        (Datum::Symbol(s), Symbol::Keyword) => s,
        _ => return Err(())
    };

    match &symbol[..] {
        keywords::BEGIN => list.into_iter()
                .map(parse_definition)
                .collect::<Result<Vec<Definition>, ()>>()
                .map(|defs| {
                    Definition::Begin(defs)
                }),
        keywords::DEFINE if list.len() >= 2 => match {
            let formals = parse_lambda_formals_exp(list.pop_front().unwrap());
            (formals, list.len())
        } {
            (Ok(LambdaFormals::VarArgs(s)), 1) => parse_expression(list.pop_front().unwrap()).map(|exp| {
                Definition::Define {
                    variable: s,
                    expression: Box::new(exp)
                }
            }),
            (Ok(LambdaFormals::List(mut args)), _) => {
                if args.len() == 0 {
                    return Err(());
                }
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
    match Symbol::from_datum(datum) {
        (Datum::List(l), _) => l.into_variables().map(|vars| {
            LambdaFormals::List(vars)
        }),
        (Datum::Pair {
            mut car, cdr
        }, _) => {
            car.push_back(*cdr);
            car.into_variables().map(|mut vars| {
                let rest = vars.pop().unwrap();
                LambdaFormals::Rest(vars, rest)
            })
        },
        (Datum::Symbol(s), Symbol::Variable) => Ok(LambdaFormals::VarArgs(s)),
        _ => Err(())
    }
}

fn parse_definitions(datums: &mut VecDeque<Datum>) -> Vec<Definition> {
    let mut definitions = vec![];

    while datums.len() > 0 {
        let datum = datums.get(0).unwrap().clone();
        match parse_definition(datum) {
            Ok(def) => {
                datums.pop_front().unwrap();
                definitions.push(def);
            },
            _ => break
        }
    }

    definitions
}

// TO DO: does this method really take 1 datum? The grammar suggests so.
fn parse_quasiquotation(datum: Datum) -> Result<Expression, ()> {
    println!("{:?}", datum);
    unimplemented!();
}
