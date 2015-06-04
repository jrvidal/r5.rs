
use std::ascii::AsciiExt;
use super::peekable::Peekable;
use super::{Token};

#[derive(Debug, PartialEq, Clone)]
pub struct NumberToken {
    exactness: Option<Exactness>,
    radix: Option<Radix>,
    number: NumberLiteral
}

#[derive(Debug, PartialEq, Clone)]
pub enum NumberLiteral {
    Cartesian(Option<RealLiteral>, RealLiteral),
    Polar(RealLiteral, RealLiteral),
    Real(RealLiteral)
}

#[derive(Debug, PartialEq, Clone)]
pub enum RealLiteral {
    Integer(IntLiteral),
    Fraction(IntLiteral, IntLiteral),
    Decimal(DecLiteral)
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntLiteral {
    digits: String,
    pounds: u8,
    sign: Option<NumSign>
}

#[derive(Debug, PartialEq, Clone)]
pub struct DecLiteral {
    digits: String,
    pounds: u8,
    dot: usize,
    sign: Option<NumSign>,
    suffix: Option<DecSuffix>
}

#[derive(Debug, PartialEq, Clone)]
pub enum NumSign {
    Plus,
    Minus
}

#[derive(Debug, PartialEq, Clone)]
pub struct DecSuffix {
    marker: ExpMarker,
    sign: Option<NumSign>,
    digits: String
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpMarker {
    Short,
    Single,
    Double,
    Long
}

#[derive(Debug, PartialEq, Clone)]
pub enum Radix {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16
}

#[derive(Debug, PartialEq, Clone)]
pub enum Exactness {
    Exact,
    Inexact
}

impl From<char> for NumSign {
    fn from(c: char) -> NumSign {
        match c {
            '+' => NumSign::Plus,
            '-' => NumSign::Minus,
            _ => panic!("Invalid sign!")
        }
    }
}

impl From<u8> for Radix {
    fn from(n: u8) -> Radix {
        match n {
            2 => Radix::Binary,
            8 => Radix::Octal,
            10 => Radix::Decimal,
            16 => Radix::Hexadecimal,
            _ => panic!("Invalid radix!")
        }
    }
}

impl From<char> for Radix {
    fn from(c: char) -> Radix {
        match c.to_ascii_lowercase() {
            'b' => Radix::Binary,
            'o' => Radix::Octal,
            'd' => Radix::Decimal,
            'x' => Radix::Hexadecimal,
            _ => panic!("Invalid radix!")
        }
    }
}

impl From<char> for Exactness {
    fn from(c: char) -> Exactness {
        match c.to_ascii_lowercase() {
            'e' => Exactness::Exact,
            'i' => Exactness::Inexact,
            _ => panic!("Invalid exactness")
        }
    }
}

enum ParsingNumber {

}

type Prefix = (Option<Exactness>, Option<Radix>);

// stream is guaranteed to be non-empty
pub fn parse_number<T: Iterator<Item=char>> (mut stream: &mut Peekable<T>) -> Result<NumberToken, String> {
    read_prefix(stream).and_then(|(e, r)| {
        parse_number_literal(stream).map(|n| (e, r, n))
    }).map(|(e, r, n)| {
        NumberToken {
            exactness: e,
            radix: r,
            number:n
        }
    })
}

fn read_prefix<T: Iterator<Item=char>>(mut stream: &mut Peekable<T>) -> Result<Prefix, String> {
    let mut exactness = None;
    let mut radix = None;

    loop {
        let peek = stream.small_peek();

        match peek[0] {
            Some('#') => match peek[1].map(|c| c.to_ascii_lowercase()) {
                None => return Err("no puedes".to_string()),
                Some(e) => match e {
                    'i' | 'e' if exactness.is_none() => {
                        exactness = Some(Exactness::from(e));
                    },
                    'b' | 'd' | 'x' | 'o' if radix.is_none() => {
                        radix = Some(Radix::from(e))
                    },
                    _ => return Err("no puedes".to_string())
                }
            },
            _ => return Ok((exactness, radix))
        }

    }
}

fn parse_number_literal<T: Iterator<Item=char>>(stream: &mut Peekable<T>) -> Result<NumberLiteral, String> {
    read_real_literal(stream).and_then(|r| {
        let peek = stream.small_peek();

        match peek[0] {
            Some('@') => {
                stream.next();
                read_real_literal(stream).map(|theta| {
                    NumberLiteral::Polar(r, theta)
                })
            },
            Some('+') | Some('-') => {
                let complex_result = read_real_literal(stream).map(|y| {
                    NumberLiteral::Cartesian(Some(r), y)
                });

                match stream.small_peek()[0] {
                    Some('i') | Some('I') => complex_result,
                    _ => Err("wrong complex".to_string())
                }
            },
            Some('i') | Some('I') => {
                stream.next();
                Ok(NumberLiteral::Cartesian(None, r))
            },
            _ => Ok(NumberLiteral::Real(r))
        }
    })
}

fn read_real_literal<T: Iterator<Item=char>>(mut stream: &Peekable<T>) -> Result<RealLiteral, String> {
    unimplemented!();
    // let mut reading = true;
    // let mut literal = IntLiteral {
    //     sign: None,
    //     digits: String::new(),
    //     pounds: 0
    // };

    // loop {
    //     let c = match stream.next() {
    //         None => if reading {
    //             return Err("no literal".to_string());
    //         } else {
    //             // return Ok(literal);
    //         },
    //         Some(d) => d.to_ascii_lowercase()
    //     };

    //     reading = false;

    //     match c {

    //     }


    // }
}
