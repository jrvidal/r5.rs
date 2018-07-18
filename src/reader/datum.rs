use std::fmt;
use std::collections::VecDeque;
use fallible_iterator::{FallibleIterator, Peekable};

use crate::lexer::{NumberToken, Token, TokenizerError};

/// A "datum" is basically a balanced token tree
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
        cdr: Box<Datum>,
    },
    Abbreviation {
        kind: AbbreviationKind,
        datum: Box<Datum>,
    },
    Vector(VecDeque<Datum>),
}

/// The type of a quotation
#[derive(Debug, Clone, PartialEq)]
pub enum AbbreviationKind {
    Quote,
    Quasiquote,
    Comma,
    CommaAt,
}

#[derive(Debug)]
pub enum ReaderError {
    UnexpectedEOF,
    UnexpectedListToken,
    UnexpectedToken,
    Tokenizer(Box<TokenizerError>),
}

macro_rules! take_peek {
    ($iter:expr) => {
        $iter
            .peek()
            .map_err(|err| ReaderError::Tokenizer(Box::new(err)))?
    };
}

/// Parses tokens into datums
pub fn parse_datum<T>(stream: &mut Peekable<T>) -> Result<Option<Datum>, ReaderError>
where
    T: FallibleIterator<Item = Token, Error = TokenizerError>
{
    let token = match stream.next() {
        Ok(None) => return Ok(None),
        Err(e) => Err(ReaderError::Tokenizer(Box::new(e)))?,
        Ok(Some(token)) => token,
    };

    match token {
        Token::Boolean(b) => ok_some!(Datum::Boolean(b)),
        Token::Number(n) => ok_some!(Datum::Number(n)),
        Token::Character(c) => ok_some!(Datum::Character(c)),
        Token::String(s) => ok_some!(Datum::String(s)),
        Token::Identifier(x) => ok_some!(Datum::Symbol(x)),

        Token::Open => parse_list_datum(stream),
        Token::Comma | Token::CommaAt | Token::SingleQuote | Token::BackQuote => {
            // TO DO
            // This could be handled by a common type:
            // *iff* those tokens are not used for anything else
            let abbr = match token {
                Token::Comma => AbbreviationKind::Comma,
                Token::CommaAt => AbbreviationKind::CommaAt,
                Token::SingleQuote => AbbreviationKind::Quote,
                Token::BackQuote => AbbreviationKind::Quasiquote,
                _ => unreachable!(),
            };

            let datum = parse_datum(stream)?.ok_or(ReaderError::UnexpectedEOF)?;
            Ok(Some(Datum::Abbreviation {
                datum: Box::new(datum),
                kind: abbr,
            }))
        }
        Token::OpenVector => {
            let mut datums = VecDeque::new();

            loop {
                if take_peek![stream].ok_or(ReaderError::UnexpectedEOF)? == &Token::Close {
                    let _ = stream.next();
                    break;
                }

                let datum = parse_datum(stream)?.unwrap();
                datums.push_back(datum);
            }

            ok_some!(Datum::Vector(datums))
        }
        _ => Err(ReaderError::UnexpectedToken),
    }
}

// Assumes a stream without the initial Open
fn parse_list_datum<T>(stream: &mut Peekable<T>) -> Result<Option<Datum>, ReaderError>
where
    T: FallibleIterator<Item = Token, Error = TokenizerError>,
{
    let mut datums = VecDeque::new();
    let mut last = None;
    let mut is_pair = false;

    loop {
        match *take_peek![stream].ok_or(ReaderError::UnexpectedEOF)? {
            Token::Close if !is_pair && last.is_none() => {
                let _ = stream.next();
                ret_val!(Datum::List(datums));
            }
            Token::Close if is_pair && !datums.is_empty() && last.is_some() => {
                let _ = stream.next();
                ret_val!(Datum::Pair {
                    car: datums,
                    cdr: last.unwrap(),
                });
            }
            Token::Dot if !is_pair => {
                is_pair = true;
                let _ = stream.next();
            }
            // Close and Dot are errors in any other circumstances
            // Also interrupted stream or any other token after finishing a pair
            Token::Close | Token::Dot => return Err(ReaderError::UnexpectedListToken),
            _ if last.is_some() && is_pair => return Err(ReaderError::UnexpectedListToken),
            _ => {}
        }

        match parse_datum(stream) {
            Ok(Some(d)) => if is_pair {
                last = Some(Box::new(d));
            } else {
                datums.push_back(d);
            },
            _ => return Err(ReaderError::UnexpectedEOF),
        }
    }
}

impl Datum {
    pub fn list(self) -> Option<VecDeque<Datum>> {
        match self {
            Datum::List(l) => Some(l),
            _ => None,
        }
    }

    pub fn symbol(self) -> Option<String> {
        match self {
            Datum::Symbol(s) => Some(s),
            _ => None,
        }
    }
}

impl fmt::Display for Datum {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Datum::List(ref ds) => {
                fmt.write_str("(")?;
                for d in ds {
                    d.fmt(fmt)?;
                }
                fmt.write_str(")")
            }
            Datum::Vector(ref ds) => {
                fmt.write_str("#(")?;
                for d in ds {
                    d.fmt(fmt)?;
                }
                fmt.write_str(")")
            }
            Datum::Boolean(b) => fmt.write_str(if b { "#t" } else { "#f" }),
            Datum::Pair { ref car, ref cdr } => {
                fmt.write_str("(")?;
                for d in car {
                    d.fmt(fmt)?;
                    fmt.write_str(" ")?;
                }
                fmt.write_str(". ")?;
                cdr.fmt(fmt)?;
                fmt.write_str(")")
            }
            Datum::Symbol(ref s) => fmt.write_str(&s[..]),
            Datum::Abbreviation {
                kind: AbbreviationKind::Quote,
                ref datum,
            } => {
                fmt.write_str("'")?;
                datum.fmt(fmt)
            }
            Datum::Character(' ') => fmt.write_str("#\\space"),
            Datum::Character('\n') => fmt.write_str("#\\newline"),
            Datum::Character(c) => fmt.write_fmt(format_args!("#\\{}", c)),
            _ => fmt.write_str("<datum>"),
        }
    }
}

#[cfg(test)]
mod test {
    use fallible_iterator;
    use self::ReaderErrorType::*;
    use super::*;
    use crate::lexer::{Token, TokenizerError};
    use std::collections::VecDeque;
    use std::iter::FromIterator;

    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    enum ReaderErrorType {
        UnexpectedEOF,
        UnexpectedListToken,
        UnexpectedToken,
        Tokenizer,
    }

    impl<'a> From<&'a ReaderError> for ReaderErrorType {
        fn from(error: &ReaderError) -> ReaderErrorType {
            match error {
                ReaderError::UnexpectedEOF => ReaderErrorType::UnexpectedEOF,
                ReaderError::UnexpectedListToken => ReaderErrorType::UnexpectedListToken,
                ReaderError::UnexpectedToken => ReaderErrorType::UnexpectedToken,
                ReaderError::Tokenizer(..) => ReaderErrorType::Tokenizer,
            }
        }
    }

    macro_rules! vec_deque {
        ($( $x:expr ),*) => ({
            let v = vec![$( $x ),*];
            VecDeque::from_iter(v.into_iter())
        });
        // TO DO: WTF??
        ($( $x:expr, )*) => (vec_deque![ $( $x ),* ]);
    }

    macro_rules! parse {
        ($stream:expr) => {{
            let successful = $stream.into_iter().map(|it| -> Result<_, TokenizerError> { Ok(it) });
            let mut fallible = fallible_iterator::convert(successful).peekable();
            parse_datum(&mut fallible).map_err(|e| ReaderErrorType::from(&e))
        }};
    }

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
        assert_eq!(parse!(stream), ok_some!(Datum::Boolean(false)));
    }

    #[test]
    fn list_test() {
        let mut stream = tokens(&[Token::Open, Token::Character('a'), Token::Close]);
        let expected = Datum::List(vec_deque![Datum::Character('a')]);
        assert_eq!(parse!(stream), ok_some!(expected));
    }

    #[test]
    fn list_pair_test() {
        let s = "b".to_string();
        let stream = tokens(&[
            Token::Open,
            Token::Character('a'),
            Token::Dot,
            Token::String(s.clone()),
            Token::Close,
        ]);
        let expected = Datum::Pair {
            car: vec_deque![Datum::Character('a')],
            cdr: Box::new(Datum::String(s.clone())),
        };
        assert_eq!(parse!(stream), ok_some!(expected));
    }

    #[test]
    fn incomplete_list_test() {
        let stream = tokens(&[Token::Open, Token::Identifier("foo".to_string())]);
        assert_eq!(parse!(stream), Err(UnexpectedEOF));
    }

    #[test]
    fn empty_head_list_test() {
        let stream = tokens(&[Token::Open, Token::Dot, Token::Character('a'), Token::Close]);
        assert_eq!(parse!(stream), Err(UnexpectedListToken));
    }

    #[test]
    fn empty_tail_list_test() {
        let stream = tokens(&[Token::Open, Token::Character('a'), Token::Dot, Token::Close]);
        assert_eq!(parse!(stream), Err(UnexpectedEOF));
    }

    #[test]
    fn double_tail_list_test() {
        let stream = tokens(&[
            Token::Open,
            Token::Character('a'),
            Token::Dot,
            Token::Boolean(true),
            Token::String("foo".to_string()),
            Token::Close,
        ]);
        assert!(parse!(stream).is_err());
    }

    #[test]
    fn vector_test() {
        let stream = tokens(&[Token::OpenVector, Token::Boolean(true), Token::Close]);

        assert_eq!(
            parse!(stream),
            ok_some!(Datum::Vector(vec_deque![Datum::Boolean(true)]))
        );
    }

    #[test]
    fn abbreviation_test() {
        let stream = tokens(&[
            Token::BackQuote,
            Token::Open,
            Token::Identifier("foo".to_string()),
            Token::Identifier("bar".to_string()),
            Token::Close,
        ]);
        let expected = Datum::Abbreviation {
            kind: AbbreviationKind::Quasiquote,
            datum: Box::new(Datum::List(vec_deque![
                Datum::Symbol("foo".to_string()),
                Datum::Symbol("bar".to_string())
            ])),
        };
        assert_eq!(parse!(stream), ok_some!(expected));
    }

    #[test]
    fn incomplete_abbreviation_test() {
        let stream = tokens(&[Token::SingleQuote, Token::Open]);
        assert_eq!(parse!(stream), Err(UnexpectedEOF));
    }

}
