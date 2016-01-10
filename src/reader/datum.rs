use std::collections::VecDeque;
use ::lexer::{Token, NumberToken};
use ::helpers::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Datum {
    Boolean(bool),
    // We should parse the number token at this time
    Number(NumberToken),
    Character(char),
    String(String),
    Symbol(String),
    List(VecDeque<Datum>),
    // car is non-empty!
    Pair {
        car: VecDeque<Datum>,
        cdr: Box<Datum>
    },
    Abbreviation {
        kind: AbbreviationKind,
        datum: Box<Datum>
    },
    Vector(VecDeque<Datum>)
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
        Token::Boolean(b) => ok_some!(Datum::Boolean(b)),
        Token::Number(n) => ok_some!(Datum::Number(n)),
        Token::Character(c) => ok_some!(Datum::Character(c)),
        Token::String(s) => ok_some!(Datum::String(s)),
        Token::Identifier(x) => ok_some!(Datum::Symbol(x)),

        Token::Open => {
            let mut datums = VecDeque::new();
            let mut last = None;
            let mut is_pair = false;

            loop {

                match try![ stream.get(0).ok_or(()) ] {
                    &Token::Close if !is_pair && last.is_none() => {
                        stream.pop_front();
                        ret_val!(Datum::List(datums));
                    },
                    &Token::Close if is_pair && datums.len() > 0 && last.is_some() => {
                        stream.pop_front();
                        ret_val!(Datum::Pair {
                            car: datums,
                            cdr: last.unwrap()
                        });
                    },
                    &Token::Dot if is_pair == false => {
                        is_pair = true;
                        stream.pop_front();
                    },
                    // Close and Dot are errors in any other circumstances
                    // Also interrupted stream or any other token after finishing a pair
                    &Token::Close | &Token::Dot => return Err(()),
                    _ => {
                        try![ ( !(last.is_some() && is_pair) ).result() ];
                    }
                }

                try![parse_datum(stream)
                    .and_then(|d| d.ok_or(()))
                    .map(|d| {
                        if is_pair {
                            last = Some(Box::new(d));
                        } else {
                            datums.push_back(d);
                        }
                    })
                ];
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

            parse_datum(stream).and_then(|d| d.ok_or(())).map(|d| {
                Datum::Abbreviation {
                    datum: Box::new(d),
                    kind: abbr
                }
            }).map(Some)
        },
        Token::OpenVector => {
            let mut datums = VecDeque::new();

            loop {

                if try![ stream.get(0).ok_or(()) ] == &Token::Close {
                    stream.pop_front();
                    break;
                }

                datums.push_back(try![
                    parse_datum(stream).and_then(|d| d.ok_or(()))
                ]);
            }

            ok_some!(Datum::Vector(datums))
        },
        _ => Err(())
    }
}

impl Datum {
    pub fn list(self) -> Option<VecDeque<Datum>> {
        match self {
            Datum::List(l) => Some(l),
            _ => None
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::VecDeque;
    use std::iter::FromIterator;
    use super::*;
    use ::lexer::{Token};

    pub fn tokens(t: &[Token]) -> VecDeque<Token> {
        let mut stream = VecDeque::new();
        for token in t.iter().cloned() {
            stream.push_back(token);
        }
        stream
    }

    #[test]
    fn boolean_datum_test() {
        let mut stream = tokens(&[Token::Boolean(false)]);
        assert_eq!(parse_datum(&mut stream), ok_some!(Datum::Boolean(false)));
    }

    #[test]
    fn list_test() {
        let mut stream = tokens(&[Token::Open, Token::Character('a'), Token::Close]);
        let expected = Datum::List(vec_deque![Datum::Character('a')]);
        assert_eq!(parse_datum(&mut stream), ok_some!(expected));
    }

    #[test]
    fn list_pair_test() {
        let s = "b".to_string();
        let mut stream = tokens(&[
            Token::Open, Token::Character('a'), Token::Dot, Token::String(s.clone()), Token::Close
        ]);
        let expected = Datum::Pair {
            car: vec_deque![Datum::Character('a')],
            cdr: Box::new(Datum::String(s.clone()))
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

        assert_eq!(parse_datum(&mut stream), ok_some!(Datum::Vector(vec_deque![Datum::Boolean(true)])));
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
            datum: Box::new(Datum::List(
                vec_deque![
                    Datum::Symbol("foo".to_string()),
                    Datum::Symbol("bar".to_string())
                ]
            ))
        };
        assert_eq!(parse_datum(&mut stream), ok_some!(expected));
    }

    #[test]
    fn incomplete_abbreviation_test() {
        let mut stream = tokens(&[Token::SingleQuote, Token::Open]);
        assert_eq!(parse_datum(&mut stream), Err(()));
    }

}