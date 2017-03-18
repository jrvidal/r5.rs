use std::collections::VecDeque;
// use std::iter::IntoIterator;

use lexer::NumberToken;
use reader::{Datum, AbbreviationKind};
use parser::keywords;
use parser::keywords::is_syntactic_keyword;
use helpers::*;

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

    // Nope
    // http://stackoverflow.com/questions/18641757/unquoted-vectors-in-r5rs-scheme
    // Vector(Vec<Expression>),

    Call {
        operator: Box<Expression>,
        operands: Vec<Expression>,
    },
    Lambda { formals: LambdaFormals, body: Body },
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
        datum: VecDeque<Datum>,
    },
    // TO DO: macro block
    MacroBlock,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Derived {
    Cond {
        head_clause: CondClause,
        tail_clauses: Vec<CondClause>,
    },
    CondElse {
        clauses: Vec<CondClause>,
        else_commands: Vec<Expression>,
        else_expression: Box<Expression>,
    },
    Case {
        key: Box<Expression>,
        head_clause: CaseClause,
        tail_clauses: Vec<CaseClause>,
    },
    CaseElse {
        key: Box<Expression>,
        clauses: Vec<CaseClause>,
        else_commands: Vec<Expression>,
        else_expression: Box<Expression>,
    },
    And(Vec<Expression>),
    Or(Vec<Expression>),
    Let { bindings: Vec<Binding>, body: Body },
    NamedLet {
        variable: String,
        bindings: Vec<Binding>,
        body: Body,
    },
    LetStar { bindings: Vec<Binding>, body: Body },
    LetRec { bindings: Vec<Binding>, body: Body },
    Begin {
        commands: Vec<Expression>,
        expression: Box<Expression>,
    },
    Do {
        iterations: Vec<IterationSpec>,
        test: Box<Expression>,
        result: Vec<Expression>,
        commands: Vec<Expression>,
    },
    Delay(Box<Expression>), 

    // Quasiquotation is at the top level
}

#[derive(Debug, PartialEq, Clone)]
pub struct IterationSpec {
    variable: String,
    init: Box<Expression>,
    step: Option<Box<Expression>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binding {
    pub variable: String,
    pub init: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Definition {
    Define {
        variable: String,
        expression: Box<Expression>,
    },
    DefineLambda {
        variable: String,
        formals: LambdaFormals,
        body: Body,
    },
    Begin(Vec<Definition>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Body {
    pub definitions: Vec<Definition>,
    pub commands: Vec<Expression>,
    pub expression: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CondClause {
    Normal {
        test: Box<Expression>,
        expressions: Vec<Expression>,
    },
    Arrow {
        test: Box<Expression>,
        recipient: Box<Expression>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct CaseClause {
    datums: VecDeque<Datum>,
    commands: Vec<Expression>,
    expression: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LambdaFormals {
    VarArgs(String),
    List(Vec<String>),
    // For rest params, vec.len() *must* be >= 1
    Rest(Vec<String>, String),
}

#[derive(Debug, Eq, PartialEq)]
pub enum ParsingError {
    Illegal,
}

macro_rules! check {
    ($check:expr, $err:expr) => (
        if !$check {
            return Err($err);
        }
    )
}

macro_rules! let_expr {
    ($type:ident, $details:expr) => (
        {

            let (bindings, body) = $details?;
            Ok(Expression::Derived(Derived::$type { bindings, body }))
        }
    )
}

pub fn parse_expression(d: Datum) -> Result<Expression, ParsingError> {
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

        (Datum::Abbreviation { kind: AbbreviationKind::Quote, datum: dat }, _) => {
            return Ok(Expression::Quotation(*dat))
        }

        (Datum::Abbreviation { kind: AbbreviationKind::Quasiquote, datum }, _) => {
            return parse_quasiquotation(*datum)
        }

        // Delegate
        (Datum::List(mut datums), _) => {
            check![datums.len() > 0, ParsingError::Illegal];
            let head = datums.pop_front().unwrap();
            (head, datums)
        }

        _ => {
            return Err(ParsingError::Illegal);
        }

        // _ => return Err(ParsingError::Illegal),
    };

    let symbol = &match keyword_name(head.clone()) {
                      Some(s) => s,
                      _ => return parse_call_exp(head, datums),
                  };

    match (&symbol[..], datums.len()) {
        // Verbose quotations
        (keywords::QUOTE, 1) => Ok(datums.pop_front().map(Expression::Quotation).unwrap()),
        // Verbose quasiquotations
        (keywords::QUASIQUOTE, 1) => {
            Ok(datums.pop_front().map(Expression::QuasiQuotation).unwrap())
        }
        // If
        (keywords::IF, 2) |
        (keywords::IF, 3) => {
            let mut exprs = datums.into_expressions()?;
            let (test, consequent, alternate) = {
                let last = exprs.pop();
                let next_to_last = exprs.pop().unwrap();
                if exprs.len() == 0 {
                    (next_to_last, last.unwrap(), None)
                } else {
                    (exprs.pop().unwrap(), next_to_last, last)
                }
            };
            let conditional = Expression::Conditional {
                test: Box::new(test),
                consequent: Box::new(consequent),
                alternate: alternate.map(Box::new),
            };
            Ok(conditional)
        }
        // Assignment
        (keywords::SET_BANG, 2) => {
            let var = symbol_type(datums.pop_front().unwrap());
            let parsed = parse_expression(datums.pop_front().unwrap());
            match (var.0, var.1, parsed) {
                (Datum::Symbol(s), Symbol::Variable, Ok(expr)) => {
                    Ok(Expression::Assignment {
                           variable: s,
                           expression: Box::new(expr),
                       })
                }
                _ => Err(ParsingError::Illegal),
            }
        }

        // Lambda
        (keywords::LAMBDA, l) if l >= 2 => return parse_lambda_exp(datums),

        (keywords::COND, l) if l >= 1 => {
            let else_expressions = parse_else_clause(&mut datums)?;

            let mut clauses = datums.into_iter()
                .map(parse_cond_clause_exp)
                .collect::<Result<Vec<CondClause>, ParsingError>>()?;

            let derived = match else_expressions {
                Some((else_commands, else_expression)) => {
                    Derived::CondElse {
                        clauses,
                        else_commands,
                        else_expression,
                    }
                }
                None => {
                    let head_clause = clauses.remove(0);
                    Derived::Cond {
                        head_clause,
                        tail_clauses: clauses,
                    }
                }
            };

            Ok(Expression::Derived(derived))
        }
        (keywords::CASE, l) if l >= 2 => {
            let key = Box::new(parse_expression(datums.pop_front().unwrap())?);

            let else_expressions = parse_else_clause(&mut datums)?;
            let mut clauses = datums.into_iter()
                .map(parse_case_clause_exp)
                .collect::<Result<_, _>>()?;

            let derived = match else_expressions {
                Some((commands, expr)) => {
                    Derived::CaseElse {
                        key: key,
                        clauses: clauses,
                        else_commands: commands,
                        else_expression: expr,
                    }
                }
                None => {
                    let head = clauses.remove(0);
                    Derived::Case {
                        key: key,
                        head_clause: head,
                        tail_clauses: clauses,
                    }
                }

            };

            Ok(Expression::Derived(derived))
        }
        (keywords::LET, l) if l >= 2 => {
            let derived = match symbol_type(datums.pop_front().unwrap()) {
                (Datum::Symbol(s), Symbol::Variable) => {
                    let (bindings, body) = parse_let_exp(datums)?;
                    Derived::NamedLet {
                        variable: s,
                        bindings,
                        body,
                    }
                }
                (d, _) => {
                    datums.insert(0, d);
                    return let_expr![Let, parse_let_exp(datums)];
                }
            };
            Ok(Expression::Derived(derived))
        }
        (keywords::LETREC, l) if l >= 2 => let_expr![LetRec, parse_let_exp(datums)],
        (keywords::LET_STAR, l) if l >= 2 => let_expr![LetStar, parse_let_exp(datums)],
        (keywords::DO, l) if l >= 2 => {
            let iterations = datums.pop_front()
                .unwrap()
                .list()
                .ok_or(ParsingError::Illegal)?
                .into_iter()
                .map(parse_iteration_spec)
                .collect::<Result<_, _>>()?;

            let mut list = datums.pop_front()
                .unwrap()
                .list()
                .ok_or(ParsingError::Illegal)?;

            check![list.len() > 0, ParsingError::Illegal];

            let test = list.pop_front()
                .map(parse_expression)
                .unwrap()
                .map(Box::new)?;

            let result = list.into_expressions()?;
            let commands = datums.into_expressions()?;
            let derived = Derived::Do {
                test,
                result,
                commands,
                iterations,
            };

            Ok(Expression::Derived(derived))

        }
        (keywords::AND, _) => datums.into_expressions().map(Derived::And).map(Expression::Derived),

        (keywords::OR, _) => datums.into_expressions().map(Derived::Or).map(Expression::Derived),

        (keywords::BEGIN, l) if l >= 2 => {
            let mut commands = datums.into_expressions()?;
            let expression = Box::new(commands.pop().unwrap());
            let derived = Derived::Begin {
                commands,
                expression,
            };
            Ok(Expression::Derived(derived))
        }
        (keywords::DELAY, 1) => {
            parse_expression(datums.pop_front().unwrap())
                .map(Box::new)
                .map(Derived::Delay)
                .map(Expression::Derived)
        }
        _ => return Err(ParsingError::Illegal),
    }
}


//
// Subexpressions
//
fn parse_else_clause(datums: &mut VecDeque<Datum>)
                     -> Result<Option<(Vec<Expression>, Box<Expression>)>, ParsingError> {
    let mut else_clause = match datums.back().cloned() {
        Some(Datum::List(l)) => {
            if l.len() > 0 {
                l
            } else {
                return Ok(None);
            }
        }
        _ => return Ok(None),
    };

    match keyword_name(else_clause.pop_front().unwrap()) {
        Some(ref s) if &s[..] == keywords::ELSE => {}
        _ => return Ok(None),
    }

    datums.pop_back();

    check![(else_clause.len() > 0), ParsingError::Illegal];

    let mut exprs = else_clause.into_expressions()?;
    let main = exprs.pop().unwrap();
    Ok(Some((exprs, Box::new(main))))
}

fn parse_cond_clause_exp(datum: Datum) -> Result<CondClause, ParsingError> {
    let mut list = datum.list().ok_or(ParsingError::Illegal)?;

    check![(list.len() > 0), ParsingError::Illegal];

    let test = list.pop_front()
        .map(parse_expression)
        .unwrap()
        .map(Box::new)?;

    let clause = match (list.get(0).cloned().map(keyword_name), list.len()) {
        (Some(Some(ref s)), 2) if s == keywords::ARROW => {
            let recipient = list.pop_back()
                .map(parse_expression)
                .unwrap()?;
            CondClause::Arrow {
                test: test,
                recipient: Box::new(recipient),
            }
        }
        _ => {
            let expressions = list.into_expressions()?;
            CondClause::Normal {
                test,
                expressions,
            }
        }
    };

    Ok(clause)
}

fn parse_case_clause_exp(datum: Datum) -> Result<CaseClause, ParsingError> {
    let mut list = datum.list().ok_or(ParsingError::Illegal)?;

    check![(list.len() >= 2), ParsingError::Illegal];

    let datums = list.pop_front()
        .unwrap()
        .list()
        .ok_or(ParsingError::Illegal)?;

    let mut commands = list.into_expressions()?;
    let expression = commands.pop().unwrap();

    let clause = CaseClause {
        datums,
        commands,
        expression: Box::new(expression),
    };

    Ok(clause)
}

// panics unless datums.len() >= 1
fn parse_let_exp(mut datums: VecDeque<Datum>) -> Result<(Vec<Binding>, Body), ParsingError> {
    let binding_list = datums.pop_front()
        .unwrap()
        .list()
        .ok_or(ParsingError::Illegal)?;

    let body = parse_body(datums)?;

    let bindings = binding_list.into_iter()
        .map(|b| {
            let mut pair = b.list().ok_or(ParsingError::Illegal)?;
            check![(pair.len() == 2), ParsingError::Illegal];

            let variable = parse_variable(pair.pop_front().unwrap())?;
            let init = parse_expression(pair.pop_front().unwrap())?;

            let binding = Binding {
                variable,
                init: Box::new(init),
            };
            Ok(binding)
        })
        .collect::<Result<_, _>>()?;


    Ok((bindings, body))
}

fn parse_call_exp(operator_d: Datum,
                  operands_d: VecDeque<Datum>)
                  -> Result<Expression, ParsingError> {
    let operator = Box::new(parse_expression(operator_d)?);
    let operands = operands_d.into_expressions()?;
    let call = Expression::Call {
        operator,
        operands,
    };
    Ok(call)
}

fn parse_lambda_exp(mut datums: VecDeque<Datum>) -> Result<Expression, ParsingError> {
    let formal_datums = datums.pop_front().unwrap();
    let formals = parse_lambda_formals_exp(formal_datums)?;
    let body = parse_body(datums)?;
    let lambda = Expression::Lambda { formals, body };
    Ok(lambda)
}

fn parse_body(mut datums: VecDeque<Datum>) -> Result<Body, ParsingError> {
    let definitions = parse_definitions(&mut datums);

    check![datums.len() > 0, ParsingError::Illegal];

    let mut commands = datums.into_expressions()?;
    let expression = commands.pop().unwrap();
    let body = Body {
        commands,
        definitions,
        expression: Box::new(expression),
    };

    Ok(body)
}

fn parse_definition(datum: Datum) -> Result<Definition, ParsingError> {
    let mut list = datum.list().ok_or(ParsingError::Illegal)?;

    check![list.len() > 0, ParsingError::Illegal];

    let symbol = keyword_name(list.pop_front().unwrap()).ok_or(ParsingError::Illegal)?;

    match &symbol[..] {
        keywords::BEGIN => {
            list.into_iter()
                .map(parse_definition)
                .collect::<Result<_, _>>()
                .map(Definition::Begin)
        }

        keywords::DEFINE if list.len() >= 2 => {
            let formals = list.pop_front().map(parse_lambda_formals_exp).unwrap();
            let definition = match (formals, list.len()) {
                (Ok(LambdaFormals::VarArgs(variable)), 1) => {
                    let expression = list.pop_front()
                        .map(parse_expression)
                        .unwrap()?;

                    Definition::Define {
                        variable,
                        expression: Box::new(expression),
                    }
                }
                (Ok(LambdaFormals::List(mut args)), _) => {
                    check![args.len() > 0, ParsingError::Illegal];

                    let variable = args.remove(0);
                    let body = parse_body(list)?;

                    Definition::DefineLambda {
                        variable,
                        body,
                        formals: LambdaFormals::List(args),
                    }
                }
                (Ok(LambdaFormals::Rest(mut args, rest)), _) => {
                    let body = parse_body(list)?;
                    let variable = args.remove(0);
                    let formals = if args.len() == 0 {
                        LambdaFormals::VarArgs(rest)
                    } else {
                        LambdaFormals::Rest(args, rest)
                    };
                    Definition::DefineLambda {
                        variable,
                        formals,
                        body,
                    }
                }
                _ => return Err(ParsingError::Illegal),

            };
            Ok(definition)
        }
        _ => Err(ParsingError::Illegal),
    }
}

fn parse_lambda_formals_exp(datum: Datum) -> Result<LambdaFormals, ParsingError> {
    match symbol_type(datum) {
        (Datum::List(l), _) => l.into_variables().map(LambdaFormals::List),
        (Datum::Pair { car, cdr }, _) => {
            let vars = car.into_variables()?;
            let rest = parse_variable(*cdr)?;
            let formals = LambdaFormals::Rest(vars, rest);
            Ok(formals)
        }
        (Datum::Symbol(s), Symbol::Variable) => Ok(LambdaFormals::VarArgs(s)),
        _ => Err(ParsingError::Illegal),
    }
}

fn parse_definitions(datums: &mut VecDeque<Datum>) -> Vec<Definition> {
    let mut definitions = vec![];

    loop {
        let maybe_def = datums.get(0)
            .cloned()
            .ok_or(ParsingError::Illegal)
            .and_then(parse_definition);

        if let Ok(def) = maybe_def {
            definitions.push(def);
            datums.pop_front();
        } else {
            break;
        }
    }

    definitions
}

fn parse_variable(datum: Datum) -> Result<String, ParsingError> {
    if let Expression::Variable(s) = parse_expression(datum)? {
        Ok(s)
    } else {
        Err(ParsingError::Illegal)
    }
}

fn parse_iteration_spec(datum: Datum) -> Result<IterationSpec, ParsingError> {
    let mut list = datum.list().ok_or(ParsingError::Illegal)?;
    check![list.len() == 2 || list.len() == 3, ParsingError::Illegal];

    let variable = list.pop_front()
        .map(parse_variable)
        .unwrap()?;

    let init = list.pop_front()
        .map(parse_expression)
        .unwrap()
        .map(Box::new)?;

    let step = match list.pop_front() {
        Some(d) => Some(Box::new(parse_expression(d)?)),
        None => None,
    };


    let spec = IterationSpec {
        variable,
        init,
        step,
    };
    Ok(spec)
}

// TO DO: does this method really take 1 datum? The grammar suggests so.
fn parse_quasiquotation(datum: Datum) -> Result<Expression, ParsingError> {
    panic!("unimplemented: parse quasiquotation expressions");
}


//
// Helpers
//
enum Symbol {
    Variable,
    Keyword,
    None,
}

fn symbol_type(d: Datum) -> (Datum, Symbol) {
    match d {
        Datum::Symbol(s) => {
            if is_syntactic_keyword(&s[..]) {
                (Datum::Symbol(s), Symbol::Keyword)
            } else {
                (Datum::Symbol(s), Symbol::Variable)
            }
        }
        _ => (d, Symbol::None),
    }
}

fn keyword_name(d: Datum) -> Option<String> {
    match symbol_type(d) {
        (Datum::Symbol(s), Symbol::Keyword) => Some(s),
        _ => None,
    }
}

trait IntoHelper {
    fn into_expressions(self) -> Result<Vec<Expression>, ParsingError>;
    fn into_variables(self) -> Result<Vec<String>, ParsingError>;
    // fn into_keyword_list(self, keyword: &str) -> Result<Vec<Expression>, ParsingError>;
}

impl IntoHelper for VecDeque<Datum> {
    fn into_expressions(self) -> Result<Vec<Expression>, ParsingError> {
        self.into_iter().map(parse_expression).collect()
    }

    fn into_variables(self) -> Result<Vec<String>, ParsingError> {
        self.into_iter().map(parse_variable).collect()
    }

    // fn into_keyword_list(mut self, keyword: &str) -> Result<Vec<Expression>, ParsingError> {
    //     if self.len() == 0 {
    //         return Err(ParsingError::Illegal);
    //     }
    //     match symbol_type(self[0].clone()) {
    //         (Datum::Symbol(ref s), Symbol::Keyword) if s == keyword => {
    //             self.pop_front();
    //             self.into_expressions()
    //         },
    //         _ => Err(ParsingError::Illegal)
    //     }
    // }
}
