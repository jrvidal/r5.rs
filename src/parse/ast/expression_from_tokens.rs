use std::collections::VecDeque;
use ::parse::token::{Token, NumberToken};
use super::datum::{Datum, parse_datum};
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

    // Last token was '(' and there is a next token for sure.

    // Discard function calls
    if match stream.get(0).unwrap() {
        &Token::Identifier(ref s) => {
            !is_syntactic_keyword(s)
        },
        _ => true
    } {
        return parse_call_exp(&mut stream);
    }

    let k = match stream.pop_front() {
        Some(Token::Identifier(s)) => s,
        _ => "__error__".to_string()
    };

    match &k[..] {
        keywords::IF => match (
            parse_expression(&mut stream),
            parse_expression(&mut stream),
            parse_expression(&mut stream),
            consume_close(&mut stream)
        ) {
            (Ok(Some(test)), Ok(Some(cons)), Ok(alt), true) => ret_val!(Expression::Conditional {
                test: Box::new(test),
                consequent: Box::new(cons),
                alternate: alt.map(|e| {Box::new(e)})
            }),
            _ => return Err(())
        },
        keywords::SET_BANG => match (
            parse_expression(&mut stream),
            parse_expression(&mut stream),
            consume_close(&mut stream),
        ) {
            (Ok(Some(Expression::Variable(s))), Ok(Some(rv)), true) => ret_val!(Expression::Assignment {
                variable: s,
                expression: Box::new(rv)
            }),
            _ => return Err(())
        },
        keywords::LAMBDA => match (

        ) {
            _ => return Err(())
        },
        _ => panic!()
    }
}

// Never returns Ok(None)
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

// Never returns Ok(None)
fn parse_lambda(mut stream: &mut VecDeque<Token>) -> Result<Option<Expression>, ()> {

    panic!()
}

struct Pair {
    vec: Vec<Expression>,
    tail: Option<Expression>
}

// Opening parenthesis is already consumed
fn parse_cons(mut stream: &mut VecDeque<Token>) -> Result<Pair, ()> {
    let mut result = Pair {
        vec: vec![],
        tail: None
    };

    loop {
        let consume_tail = match stream.get(0) {
            Some(&Token::Dot) if result.tail.is_none() && result.vec.len() > 0 => {
                stream.pop_front();
                true
            },
            Some(&Token::Close) => {
                stream.pop_front();
                return Ok(result);
            },
            _ if result.tail.is_some() => return Err(()),
            _ => false
        };
        match (parse_expression(&mut stream), consume_tail) {
            (Ok(Some(exp)), true) => {
                result.tail = Some(exp);
            },
            (Ok(Some(exp)), false) => {
                result.vec.push(exp);
            },
            _ => return Err(())
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
    use ::parse::token::{Token};
    use ::parse::ast::datum::{Datum};
    use super::super::tokens;

    fn tokens_list(t: &[Token]) -> VecDeque<Token> {
        let mut stream = tokens(t);
        stream.push_front(Token::Open);
        stream.push_back(Token::Close);
        stream
    }


    #[test]
    fn parse_self_evaluating_test() {
        let mut stream = tokens(&[Token::Identifier("foobar".to_string())]);
        assert_eq!(parse_expression(&mut stream), ok_some!(Expression::Variable("foobar".to_string())));

        let mut stream = tokens(&[Token::Boolean(true)]);
        assert_eq!(parse_expression(&mut stream), ok_some!(Expression::Boolean(true)));

        let mut stream = tokens(&[Token::Character('a')]);
        assert_eq!(parse_expression(&mut stream), ok_some!(Expression::Character('a')));

        let mut stream = tokens(&[Token::String("foobar".to_string())]);
        assert_eq!(parse_expression(&mut stream), ok_some!(Expression::String("foobar".to_string())));
    }

    #[test]
    fn parse_reserved_keywords_test() {
        let mut stream = tokens(&[Token::Identifier("letrec".to_string())]);
        assert!(parse_expression(&mut stream).is_err());
    }

    #[test]
    fn parse_simple_datum_test() {
        let mut stream = tokens(&[Token::SingleQuote, Token::Boolean(true)]);
        let expected = Expression::Quotation(Datum::Boolean(true));
        assert_eq!(parse_expression(&mut stream), ok_some!(expected));
    }

    #[test]
    fn parse_list_datum_test() {
        let mut stream = tokens(&[
            Token::SingleQuote,
            Token::Open,
                Token::Identifier("foobar".to_string()),
                Token::Dot,
                Token::Character('a'),
            Token::Close
        ]);
        let expected = Expression::Quotation(Datum::List {
            head: vec![Datum::Symbol("foobar".to_string())],
            last: Some(Box::new(Datum::Character('a')))
        });
        assert_eq!(parse_expression(&mut stream), ok_some!(expected));
    }

    #[test]
    fn parse_verbose_datum_test() {
        let mut stream = tokens_list(&[
            Token::Identifier("quote".to_string()),
            Token::OpenVector,
                Token::Boolean(false),
            Token::Close,
        ]);
        let expected = Expression::Quotation(Datum::Vector(
            vec![Datum::Boolean(false)],
        ));
        assert_eq!(parse_expression(&mut stream), ok_some!(expected));
    }

    #[test]
    fn parse_datum_handle_error() {
        let mut stream = tokens(&[
            Token::SingleQuote, Token::Open, Token::Dot, Token::Close
        ]);
        assert!(parse_expression(&mut stream).is_err());
    }

    #[test]
    fn parse_datum_multiple_error() {
        let mut stream = tokens_list(&[
            Token::Identifier("quote".to_string()),
            Token::OpenVector,
                Token::Boolean(false),
            Token::Close,
            Token::Boolean(true),
        ]);
        assert!(parse_expression(&mut stream).is_err());
    }

    #[test]
    fn parse_datum_none_error() {
        let mut stream = tokens_list(&[
            Token::Identifier("quote".to_string()),
        ]);
        assert!(parse_expression(&mut stream).is_err());
    }

    #[test]
    fn parse_single_open_error() {
        let mut stream = tokens(&[
            Token::Open,
        ]);
        assert!(parse_expression(&mut stream).is_err());
    }

    #[test]
    fn parse_call_expression_test() {
        let mut stream = tokens_list(&[
            Token::Identifier("foobar".to_string()),
            Token::Character('a'),
            Token::SingleQuote,
            Token::Open,
            Token::Close
        ]);
        let expected = Expression::Call {
            operator: Box::new(Expression::Variable("foobar".to_string())),
            operands: vec![
                Expression::Character('a'),
                Expression::Quotation(Datum::List {
                    head: vec![],
                    last: None
                })
            ]
        };
        assert_eq!(parse_expression(&mut stream), ok_some!(expected));
    }

    // Temporary
    #[test]
    #[should_panic]
    fn parse_call_expression_delegation_test() {
        let mut stream = tokens_list(&[
            Token::Identifier("cond".to_string()),
            Token::Character('a'),
            Token::SingleQuote,
            Token::Open,
            Token::Close
        ]);
        assert!(parse_expression(&mut stream).is_err());
    }
}