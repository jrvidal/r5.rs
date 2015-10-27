use std::collections::VecDeque;
use ::parse::token::{Token, NumberToken};

#[derive(Debug, Clone, PartialEq)]
pub enum Datum {
    Boolean(bool),
    Number(NumberToken),
    Character(char),
    String(String),
    Symbol(String),
    // If last is present, head is non-empty!!
    List {
        head: Vec<Datum>,
        last: Option<Box<Datum>>
    },
    Abbreviation {
        kind: AbbreviationKind,
        datum: Box<Datum>
    },
    Vector(Vec<Datum>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum AbbreviationKind {
    Quote,
    Quasiquote,
    Comma,
    CommaAt
}

pub fn parse_datum(stream: &mut VecDeque<Token>) -> Result<Option<Datum>, ()>{
    let x = stream.pop_front();
    if x.is_none() {
        return Ok(None);
    }
    let t = x.unwrap();

    match t {
        Token::Boolean(b) => ret_val!(Datum::Boolean(b)),
        Token::Number(n) => ret_val!(Datum::Number(n)),
        Token::Character(c) => ret_val!(Datum::Character(c)),
        Token::String(s) => ret_val!(Datum::String(s)),
        Token::Identifier(x) => ret_val!(Datum::Symbol(x)),

        Token::Open => {
            let mut head = Vec::new();
            let mut last = None;
            let mut has_dot = false;

            loop {

                match stream.get(0) {
                    Some(&Token::Close) if (
                        (head.len() > 0 || !has_dot) && has_dot == last.is_some()
                    ) => {
                        stream.pop_front();
                        ret_val!(Datum::List {
                            head: head,
                            last: last
                        });
                    },
                    Some(&Token::Dot) if has_dot == false => {
                        has_dot = true;
                        stream.pop_front();
                    },
                    // Close and Dot are errors in any other circumstances
                    // Also interrupted stream or any other token after finishing a pair
                    Some(&Token::Close) | Some(&Token::Dot) | None => return Err(()),
                    _ if has_dot && last.is_some() => return Err(()),

                    _ => {}
                }

                match parse_datum(stream) {
                    Ok(Some(d)) => {
                        if has_dot {
                            last = Some(Box::new(d));
                        } else {
                            head.push(d);
                        }
                    },
                    _ => return Err(())
                }
            }
        },
        Token::Comma | Token::CommaAt | Token::SingleQuote | Token::BackQuote => {
            // TO DO
            // This could be handled by a common type:
            // Token::Abbreviation(AbbreviationKind)
            // *iff* those tokens are not used for anything else
            let abbr = match t {
                Token::Comma => AbbreviationKind::Comma,
                Token::CommaAt => AbbreviationKind::CommaAt,
                Token::SingleQuote => AbbreviationKind::Quote,
                Token::BackQuote => AbbreviationKind::Quasiquote,
                _ => panic!()
            };

            match parse_datum(stream) {
                Ok(Some(d)) => ret_val!(Datum::Abbreviation {
                    datum: Box::new(d),
                    kind: abbr
                }),
                _ => return Err(())
            }
        },
        Token::OpenVector => {
            let mut datums = Vec::new();

            loop {

                match stream.get(0) {
                    None => return Err(()),
                    Some(&Token::Close) => {
                        stream.pop_front();
                        ret_val!(Datum::Vector(datums));
                    },
                    _ => {}
                }

                match parse_datum(stream) {
                    Ok(Some(d)) => datums.push(d),
                    _ => return Err(()),
                }
            }
        },
        _ => return Err(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use ::parse::token::{Token};
    use super::super::tokens;

    #[test]
    fn boolean_datum_test() {
        let mut stream = tokens(&[Token::Boolean(false)]);
        assert_eq!(parse_datum(&mut stream), ok_some!(Datum::Boolean(false)));
    }

    #[test]
    fn list_test() {
        let mut stream = tokens(&[Token::Open, Token::Character('a'), Token::Close]);
        let expected = Datum::List {
            head: vec![Datum::Character('a')],
            last: None
        };
        assert_eq!(parse_datum(&mut stream), ok_some!(expected));
    }

    #[test]
    fn list_pair_test() {
        let s = "b".to_string();
        let mut stream = tokens(&[
            Token::Open, Token::Character('a'), Token::Dot, Token::String(s.clone()), Token::Close
        ]);
        let expected = Datum::List {
            head: vec![Datum::Character('a')],
            last: Some(Box::new(Datum::String(s.clone())))
        };
        assert_eq!(parse_datum(&mut stream), ok_some!(expected));
    }

    #[test]
    fn incomplete_list_test() {
        let mut stream = tokens(&[Token::Open, Token::Identifier("foo".to_string())]);
        assert_eq!(parse_datum(&mut stream), Err(()));
    }


    #[test]
    fn empty_head_list_test() {
        let mut stream = tokens(&[Token::Open, Token::Dot, Token::Character('a'), Token::Close]);
        assert_eq!(parse_datum(&mut stream), Err(()));
    }

    #[test]
    fn empty_tail_list_test() {
        let mut stream = tokens(&[Token::Open, Token::Character('a'), Token::Dot, Token::Close]);
        assert_eq!(parse_datum(&mut stream), Err(()));
    }

    #[test]
    fn double_tail_list_test() {
        let mut stream = tokens(&[
            Token::Open,
               Token::Character('a'),
               Token::Dot,
               Token::Boolean(true),
               Token::String("foo".to_string()),
            Token::Close]);
        assert!(parse_datum(&mut stream).is_err());
    }

    #[test]
    fn vector_test() {
        let mut stream = tokens(&[Token::OpenVector, Token::Boolean(true), Token::Close]);

        assert_eq!(parse_datum(&mut stream), ok_some!(Datum::Vector(vec![Datum::Boolean(true)])));
    }

    #[test]
    fn abbreviation_test() {
        let mut stream = tokens(&[
            Token::BackQuote,
            Token::Open,
                Token::Identifier("foo".to_string()),
                Token::Identifier("bar".to_string()),
            Token::Close
        ]);
        let expected = Datum::Abbreviation {
            kind: AbbreviationKind::Quasiquote,
            datum: Box::new(Datum::List {
                head: vec![
                    Datum::Symbol("foo".to_string()),
                    Datum::Symbol("bar".to_string())
                ],
                last: None
            })
        };
        assert_eq!(parse_datum(&mut stream), ok_some!(expected));
    }

    #[test]
    fn incomplete_abbreviation_test() {
        let mut stream = tokens(&[Token::SingleQuote, Token::Open]);
        assert_eq!(parse_datum(&mut stream), Err(()));
    }

}