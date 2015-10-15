use std::collections::VecDeque;
use ::parse::token::{Token, NumberToken};
use super::datum::{Datum, parse_datum};

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
        // If true, formals should contain 1 parameter
        // and rest should be None
        varargs: bool,
        formals: Vec<String>,
        rest: Option<String>,
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

macro_rules! one_of {
    ($x:expr, [$c:expr]) => ($x == $c);
    ($x:expr, [ $c:expr, $( $d:expr ),* ]) => (
        $x == $c || one_of!($x, [$( $d ),* ])
    )
}

mod keywords {
    pub const IF : &'static str = "if";
    pub const OR : &'static str = "or";
    pub const ARROW : &'static str = "=>";
    pub const DO : &'static str = "do";
    pub const AND : &'static str = "and";
    pub const LET : &'static str = "let";
    pub const ELSE : &'static str = "else";
    pub const SET_BANG : &'static str = "set!";
    pub const COND : &'static str = "cond";
    pub const CASE : &'static str = "case";
    pub const LET_STAR : &'static str = "let*";
    pub const QUOTE : &'static str = "quote";
    pub const BEGIN : &'static str = "begin";
    pub const DELAY : &'static str = "delay";
    pub const DEFINE : &'static str = "define";
    pub const LAMBDA : &'static str = "lambda";
    pub const LETREC : &'static str = "letrec";
    pub const UNQUOTE : &'static str = "unquote";
    pub const QUASIQUOTE : &'static str = "quasiquote";
    pub const UNQUOTE_SPLICING : &'static str = "unquote-splicing";
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

pub fn parse_expression(mut stream: &mut VecDeque<Token>) -> Result<Option<Expression>, ()> {
    if stream.get(0).is_none() {
        return Ok(None);
    }


    let t = stream.pop_front().unwrap();

    // Simple cases
    match t {
        Token::Identifier(s) => {
            if is_syntactic_keyword(&s) {
                return Err(());
            } else {
                ret_val!(Expression::Variable(s))
            }
        },
        // TO DO: why doesn't this work??
        // Token::Identifier(s) if is_syntactic_keyword(&s) => {
        //         ret_val!(Expression::Variable(s))
        // },
        Token::Boolean(b) => ret_val!(Expression::Boolean(b)),
        Token::Character(c) => ret_val!(Expression::Character(c)),
        Token::String(s) => ret_val!(Expression::String(s)),
        Token::Number(nt) => ret_val!(Expression::Number(nt)),
        Token::SingleQuote => {
            match parse_datum(&mut stream) {
                Ok(Some(d)) => ret_val!(Expression::Quotation(d)),
                _ => return Err(())
            }
        },

        Token::BackQuote => {
            panic!("quasiquotations");
        },

        // Handle verbose quotations '(quote ...)' here
        Token::Open if is_verbose_quotation(stream.get(0)) => {
            stream.pop_front();
            match (parse_datum(&mut stream), consume_close(&mut stream)) {
                (Ok(Some(d)), true) => ret_val!(Expression::Quotation(d)),
                _ => return Err(())
            }
        },

        // Delegate to list matcher
        Token::Open if stream.get(0).is_some() => {},

        _ => return Err(())
    }

    // let next = stream.get(0).unwrap();

    // Discard function calls
    if match t {
        Token::Identifier(ref s) => {
            !is_syntactic_keyword(s)
        },
        _ => true
    } {
        return parse_call_exp(&mut stream);
    }

    // List matcher
    match t {
        _ => panic!()
    }
}

fn parse_call_exp(mut stream: &mut VecDeque<Token>) -> Result<Option<Expression>, ()> {
    let operator = {
        let operator_res = parse_expression(&mut stream);
        if operator_res.is_err() || operator_res.as_ref().ok().unwrap().is_none() {
            return Err(());
        }
        operator_res.ok().unwrap().unwrap()
    };

    let mut operands = Vec::new();

    loop {
        match stream.get(0) {
            Some(&Token::Close) => return Ok(Some(Expression::Call {
                operands: operands,
                operator: Box::new(operator)
            })),
            _ => match parse_expression(&mut stream) {
                Ok(Some(e)) => operands.push(e),
                _ => return Err(())
            }
        }
    }
}

fn is_verbose_quotation(t: Option<&Token>) -> bool {
    t.is_some() && match t.unwrap() {
        &Token::Identifier(ref s) => s == keywords::QUOTE,
        _ => false
    }
}

fn consume_close(stream: &mut VecDeque<Token>) -> bool {
    match stream.get(0) {
        Some(&Token::Close) => {
            stream.pop_front();
            true
        },
        _ => false
    }
}

#[cfg(test)]

mod test {
    use super::*;
    use std::collections::VecDeque;
    use ::parse::token::{Token, NumberToken};
    use ::parse::ast::datum::{Datum, parse_datum};


    #[test]
    fn parse_variable_test() {
        let mut tokens = VecDeque::new();
        for t in [Token::Identifier("foobar".to_string())].iter().cloned() {
            tokens.push_back(t);
        }

        let result = parse_expression(&mut tokens).ok().unwrap().unwrap();
        assert_eq!(result, Expression::Variable("foobar".to_string()));
    }

    #[test]
    fn parse_reserved_keywords_test() {
        let mut tokens = VecDeque::new();
        for t in [Token::Identifier("letrec".to_string())].iter().cloned() {
            tokens.push_back(t);
        }

        let result = parse_expression(&mut tokens);
        assert!(result.is_err());
    }
}