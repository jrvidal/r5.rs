
use std::ascii::AsciiExt;
use super::peekable::Peekable;

#[derive(Debug, PartialEq, Clone)]
pub struct NumberToken {
    exactness: Option<Exactness>,
    radix: Option<Radix>,
    number: ComplexLiteral
}

#[derive(Debug, PartialEq, Clone)]
pub enum ComplexLiteral {
    Cartesian(Option<NumSign>, Option<RealLiteral>, NumSign, Option<RealLiteral>),
    Polar(Option<NumSign>, RealLiteral, Option<NumSign>, RealLiteral),
    Real(Option<NumSign>, RealLiteral)
}

#[derive(Debug, PartialEq, Clone)]
// Actually an unsigned real
pub enum RealLiteral {
    Integer {
        digits: String,
        pounds: u8,
    },
    Fraction {
        numerator: (String, u8),
        denominator: (String, u8)
    },
    Decimal {
        digits: String,
        point: usize,
        pounds: u8,
        suffix: Option<DecSuffix>
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ExpMarker {
    Short,
    Single,
    Double,
    Long
}

#[derive(Debug, PartialEq, Clone, Copy)]
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

impl RealLiteral {
    fn is_int(&self) -> bool {
        match *self {
            RealLiteral::Integer {..} => true,
            _ => false
        }
    }
    fn is_dec(&self) -> bool {
        match *self {
            RealLiteral::Decimal {..} => true,
            _ => false
        }
    }
    fn is_frac(&self) -> bool {
        match *self {
            RealLiteral::Fraction {..} => true,
            _ => false
        }
    }

    fn digits(&self) -> &str {
        match *self {
            RealLiteral::Integer {ref digits, ..} => &digits,
            RealLiteral::Decimal {ref digits, ..} => &digits,
            RealLiteral::Fraction {
                numerator: (ref digits, _), ..} => &digits,
        }
    }

    fn fuzzy_digits(&self) -> u8 {
        match *self {
            RealLiteral::Decimal {pounds, ..} => pounds,
            RealLiteral::Integer {pounds, ..} => pounds,
            RealLiteral::Fraction {numerator: (_, pounds), ..} => pounds,
        }
    }

    fn decimal_point(&self) -> usize {
        match *self {
            RealLiteral::Decimal {point, ..} => point,
            _ => panic!("Not a decimal!")
        }
    }

    fn decimal_suffix(&self) -> &Option<DecSuffix> {
        match *self {
            RealLiteral::Decimal {ref suffix, ..} => &suffix,
            _ => panic!("Not a decimal!")
        }
    }

    fn num(&self) -> RealLiteral {
        match *self {
            RealLiteral::Fraction {
                numerator: (ref digits, pounds),
                ..
            } => RealLiteral::Integer {
                pounds: pounds,
                digits: digits.clone()
            },
            _ => panic!("Not a fraction")
        }
    }
    fn den(&self) -> RealLiteral {
        match *self {
            RealLiteral::Fraction {
                denominator: (ref digits, pounds),
                ..
            } => RealLiteral::Integer {
                pounds: pounds,
                digits: digits.clone()
            },
            _ => panic!("Not a fraction")
        }
    }
}

type Prefix = (Option<Exactness>, Option<Radix>);

// stream is guaranteed to be non-empty
pub fn parse_number<T: Iterator<Item=char>> (mut stream: &mut Peekable<T>) -> Result<NumberToken, String> {
    parse_prefix(stream).and_then(|(e, r)| {
        parse_complex(stream, &r).map(|n| (e, r, n))
    }).map(|(e, r, n)| {
        NumberToken {
            exactness: e,
            radix: r,
            number:n
        }
    })
}

fn parse_prefix<T: Iterator<Item=char>>(mut stream: &mut Peekable<T>) -> Result<Prefix, String> {
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
        stream.advance(2);
    }
}

fn parse_complex<T: Iterator<Item=char>>(stream: &mut Peekable<T>, rad: &Option<Radix>) -> Result<ComplexLiteral, String> {

    let peek = stream.small_peek();
    let mut first_sign = None;

    if peek[0] == Some('+') || peek[0] == Some('-') {
        first_sign = peek[0].map(NumSign::from);
        stream.next();
    }

    // ±i
    if first_sign.is_some() && peek[1].map(|c| c.to_ascii_lowercase()) == Some('i') {
        return Ok(ComplexLiteral::Cartesian(None, None, first_sign.unwrap(), None));
    }

    let first_real = match parse_real(stream, rad) {
        Ok(l) => l,
        Err(e) => return Err(e)
    };

    let peek = stream.small_peek();
    let mut cartesian = true;
    let mut second_sign = None;

    match peek[0] {
        Some('+') | Some('-') => {
            second_sign = peek[0].map(NumSign::from);
            stream.next();
        },
        Some('@') => {
            cartesian = false;

            match peek[1] {
                Some('+') | Some('-') => {
                    second_sign = peek[1].map(NumSign::from);
                    stream.next();
                },
                _ => {}
            }
        },

        // TO DO: error if no delimiter!
        _ => return Ok(ComplexLiteral::Real(first_sign, first_real))
    }

    let second_real = match parse_real(stream, rad) {
        Ok(l) => l,
        Err(e) => return Err(e)
    };

    let peek = stream.small_peek();

    match peek[0] {
        // TO DO: error if bad delimiter
        _ => if cartesian {
            Ok(ComplexLiteral::Cartesian(
                first_sign, Some(first_real), second_sign.unwrap(), Some(second_real)
            ))
        } else {
            Ok(ComplexLiteral::Polar(first_sign, first_real, second_sign, second_real))
        }
    }
}

// It does not err if last token is not a delimiter
fn parse_real<T: Iterator<Item=char>>(mut stream: &mut Peekable<T>, r: &Option<Radix>) -> Result<RealLiteral, String> {

    enum Number {
        Int,
        Decimal,
        Fraction
    }

    let radix = r.unwrap_or(Radix::Decimal) as u8;

    let mut num_type = Number::Int;
    let mut digits = String::new();
    let mut pounds = 0;
    let mut accepts_pounds = false;
    let mut numerator = None;
    let mut point = 0;
    let mut suffix = None;

    loop {
        let peek = stream.small_peek()[0].map(|c| c.to_ascii_lowercase());

        match peek {
            Some('#') if accepts_pounds => {
                pounds += 1;
            },
            Some('/') => match num_type {
                Number::Int if digits.len() > 0 => {
                    num_type = Number::Fraction;
                    numerator = Some((digits, pounds));
                    digits = String::new();
                    pounds = 0;
                    accepts_pounds = false;
                },
                _ => return Err("bad number".to_string())
            },
            Some('.') => match num_type {
                Number::Int if radix == 10 => {
                    point = digits.len();
                    num_type = Number::Decimal;
                },
                _ => return Err("bad number".to_string())
            },
            Some('0'...'9') | Some('a'...'f') if pounds == 0 && {
                let n = u8::from_str_radix(&peek.unwrap().to_string(), 16).ok().unwrap();
                n < radix
            } => {
                accepts_pounds = true;
                digits.push(peek.unwrap());
            },

            Some('e') | Some('s') | Some('f') | Some('d') | Some('l') => match num_type {
                Number::Decimal => {
                    // Read suffix
                    unimplemented!()
                },
                _ => return Err("bad prefix".to_string())
            },

            Some('0'...'9') | Some('a'...'f') => return Err("bad digit".to_string()),

            _ => break
        }

        stream.next();
    }

    if digits.len() == 0 {
        return Err("bad number".to_string());
    }

    Ok(match num_type {
        Number::Int => RealLiteral::Integer {
            digits: digits,
            pounds: pounds
        },
        Number::Decimal => RealLiteral::Decimal {
            digits: digits,
            pounds: pounds,
            point: point,
            suffix: suffix
        },
        Number::Fraction => RealLiteral::Fraction {
            numerator: numerator.unwrap(),
            denominator: (digits, pounds)
        }
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use super::{parse_prefix, parse_real};
    use super::super::peekable::Peekable;
    use std::str;

    fn stream(s: &str) -> Peekable<str::Chars> {
        Peekable::from_iter(s.chars())
    }

    fn next_token(s: &str) -> NumberToken {
        parse_number(&mut Peekable::from_iter(s.chars())).ok().unwrap()
    }

    #[test]
    fn prefix_test() {
        assert!(parse_prefix(&mut stream("")).ok().unwrap() == (None, None));
        assert!(parse_prefix(&mut stream("#e")).ok().unwrap() == (Some(Exactness::Exact), None));
        assert!(parse_prefix(&mut stream("#o")).ok().unwrap() == (None, Some(Radix::Octal)));
        assert!(parse_prefix(&mut stream("#d#i")).ok().unwrap() == (Some(Exactness::Inexact), Some(Radix::Decimal)));
        assert!(parse_prefix(&mut stream("#d#")).is_err());
        assert!(parse_prefix(&mut stream("#\\")).is_err());
    }

    fn integer_test() {
        let n = "1";
        let t = parse_real(&mut stream(n), &Some(Radix::Decimal)).ok().unwrap();
        assert!(t.is_int());
        assert!(t.fuzzy_digits() == 0);
        assert!(t.digits() == n);

        let n = "123";
        let t = parse_real(&mut stream(n), &Some(Radix::Decimal)).ok().unwrap();
        assert!(t.is_int());
        assert!(t.fuzzy_digits() == 0);
        assert!(t.digits() == n);

        let n = "123#";
        let t = parse_real(&mut stream(n), &Some(Radix::Decimal)).ok().unwrap();
        assert!(t.is_int());
        assert!(t.fuzzy_digits() == 1);
        assert!(t.digits() == "123");
    }

    fn fraction_test() {
        let n = "1/2";
        let t = parse_real(&mut stream(n), &Some(Radix::Decimal)).ok().unwrap();
        assert!(t.is_frac());
        assert!(t.fuzzy_digits() == 0);
        assert!(t.digits() == "1");
        let denominator = t.den();
        assert!(denominator.is_int());
        assert!(denominator.fuzzy_digits() == 0);
        assert!(denominator.digits() == "2");

        let n = "12#/2";
        let t = parse_real(&mut stream(n), &Some(Radix::Decimal)).ok().unwrap();
        assert!(t.is_frac());
        assert!(t.fuzzy_digits() == 1);
        assert!(t.digits() == "12");
        let denominator = t.den();
        assert!(denominator.is_int());
        assert!(denominator.fuzzy_digits() == 0);
        assert!(denominator.digits() == "2");


        let n = "12#/23#";
        let t = parse_real(&mut stream(n), &Some(Radix::Decimal)).ok().unwrap();
        assert!(t.is_frac());
        assert!(t.fuzzy_digits() == 1);
        assert!(t.digits() == "12");
        let denominator = t.den();
        assert!(denominator.is_int());
        assert!(denominator.fuzzy_digits() == 1);
        assert!(denominator.digits() == "23");
    }
}