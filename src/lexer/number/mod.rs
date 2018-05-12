use super::TokenErrorClass;
use super::chars::LexerIterator;
use std::i32;
use std::ops::Neg;
use std::str::FromStr;

#[cfg(test)]
mod test;

/// A token for a number literal
#[derive(Debug, PartialEq, Clone)]
pub struct NumberToken {
    exactness: Option<Exactness>,
    radix: Option<Radix>,
    number: ComplexLiteral,
}

impl NumberToken {
    /// stream is guaranteed to be non-empty
    pub fn parse<T: LexerIterator>(stream: &mut T) -> Result<NumberToken, TokenErrorClass> {
        parse_prefix(stream)
            .and_then(|(e, r)| parse_complex(stream, r).map(|n| (e, r, n)))
            .map(|(e, r, n)| NumberToken {
                exactness: e,
                radix: r,
                number: n,
            })
    }
}

impl From<NumberToken> for Result<Num, InvalidNumber> {
    fn from(nt: NumberToken) -> Result<Num, InvalidNumber> {
        let NumberToken {
            exactness,
            radix,
            number,
        } = nt;
        let exactness = exactness.unwrap_or(Exactness::Exact);
        let radix = radix.unwrap_or(Radix::Decimal);

        match number {
            ComplexLiteral::Real(sign, real) => {
                let num = real.into_num(radix, exactness)?;

                if sign == Some(NumSign::Minus) {
                    Ok(-num)
                } else {
                    Ok(num)
                }
            }
            _ => Err(InvalidNumber),
        }
    }
}

/// The value represented by a number literal
// This is most likely not part of a lexer?
pub enum Num {
    Integer(i32),
    Float(f32),
}

impl Neg for Num {
    type Output = Num;
    fn neg(self: Num) -> Num {
        match self {
            Num::Float(f) => Num::Float(-f),
            Num::Integer(n) => Num::Integer(-n),
        }
    }
}

pub struct InvalidNumber;

#[derive(Debug, PartialEq, Clone)]
enum ComplexLiteral {
    Cartesian(
        Option<(Option<NumSign>, RealLiteral)>,
        NumSign,
        Option<RealLiteral>,
    ),
    Polar(Option<NumSign>, RealLiteral, Option<NumSign>, RealLiteral),
    Real(Option<NumSign>, RealLiteral),
}

#[derive(Debug, PartialEq, Clone)]
// Actually an unsigned real
enum RealLiteral {
    Integer {
        digits: String,
        pounds: u8,
    },
    Fraction {
        numerator: (String, u8),
        denominator: (String, u8),
    },
    Decimal {
        digits: String,
        point: usize,
        pounds: u8,
        suffix: Option<DecSuffix>,
    },
}

impl RealLiteral {
    fn int_to_num(
        mut digits: String,
        pounds: u8,
        radix: Radix,
        exactness: Exactness,
    ) -> Result<Num, InvalidNumber> {
        use std::iter::repeat;
        digits.extend(repeat('0').take(pounds as usize));
        let ret = if pounds > 0 || exactness == Exactness::Inexact {
            Num::Float(f32::from_str(&digits)?)
        } else {
            Num::Integer(i32::from_str_radix(&digits, radix as u32)?)
        };
        Ok(ret)
    }

    fn into_num(self, radix: Radix, exactness: Exactness) -> Result<Num, InvalidNumber> {
        match self {
            RealLiteral::Integer { digits, pounds } => {
                RealLiteral::int_to_num(digits, pounds, radix, exactness)
            }
            RealLiteral::Fraction {
                numerator: (num_digits, num_pounds),
                denominator: (den_digits, den_pounds),
            } => {
                let denominator =
                    RealLiteral::int_to_num(den_digits, den_pounds, radix, exactness)?;
                let numerator = RealLiteral::int_to_num(num_digits, num_pounds, radix, exactness)?;

                let ret = match (numerator, denominator) {
                    (Num::Integer(n), Num::Integer(d)) if n.checked_rem(d) == Some(0) => {
                        Num::Integer(n / d)
                    }
                    (Num::Integer(n), Num::Integer(d)) => Num::Float((n as f32) / (d as f32)),
                    (Num::Integer(n), Num::Float(d)) => Num::Float((n as f32) / d),
                    (Num::Float(n), Num::Integer(d)) => Num::Float(n / (d as f32)),
                    (Num::Float(n), Num::Float(d)) => Num::Float(n / d),
                };
                Ok(ret)
            }
            RealLiteral::Decimal {
                mut digits,
                pounds,
                point,
                suffix,
            } => {
                use std::iter::repeat;
                digits.extend(repeat('0').take(pounds as usize));
                digits.insert(point, '.');

                let has_suffix = suffix
                    .as_ref()
                    .map(|suf| !suf.digits.is_empty())
                    .unwrap_or(false);

                if has_suffix {
                    let suffix = suffix.unwrap();
                    digits.push('e');
                    if suffix.sign.is_some() {
                        let sign: char = suffix.sign.unwrap().into();
                        digits.push(sign);
                    }
                    digits.push_str(&suffix.digits);
                }

                Ok(Num::Float(f32::from_str(&digits)?))
            }
        }
    }
}

use std::num::{ParseFloatError, ParseIntError};
impl From<ParseIntError> for InvalidNumber {
    fn from(_e: ParseIntError) -> InvalidNumber {
        InvalidNumber
    }
}

impl From<ParseFloatError> for InvalidNumber {
    fn from(_e: ParseFloatError) -> InvalidNumber {
        InvalidNumber
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum NumSign {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq, Clone)]
struct DecSuffix {
    marker: ExpMarker,
    sign: Option<NumSign>,
    digits: String,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum ExpMarker {
    Short,
    Single,
    Double,
    Long,
    Default,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Radix {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Exactness {
    Exact,
    Inexact,
}

impl From<char> for NumSign {
    fn from(c: char) -> NumSign {
        match c {
            '+' => NumSign::Plus,
            '-' => NumSign::Minus,
            _ => panic!("Invalid sign!"),
        }
    }
}

impl From<NumSign> for char {
    fn from(s: NumSign) -> char {
        match s {
            NumSign::Plus => '+',
            NumSign::Minus => '-',
        }
    }
}

impl From<char> for Radix {
    fn from(c: char) -> Radix {
        match c {
            'b' | 'B' => Radix::Binary,
            'o' | 'O' => Radix::Octal,
            'd' | 'D' => Radix::Decimal,
            'x' | 'X' => Radix::Hexadecimal,
            _ => panic!("Invalid radix!"),
        }
    }
}

impl From<char> for Exactness {
    fn from(c: char) -> Exactness {
        match c {
            'e' | 'E' => Exactness::Exact,
            'i' | 'I' => Exactness::Inexact,
            _ => panic!("Invalid exactness"),
        }
    }
}

impl From<char> for ExpMarker {
    fn from(c: char) -> ExpMarker {
        match c {
            'e' | 'E' => ExpMarker::Default,
            's' | 'S' => ExpMarker::Short,
            'l' | 'L' => ExpMarker::Long,
            'f' | 'F' => ExpMarker::Single,
            'd' | 'D' => ExpMarker::Double,
            _ => panic!("Invalid exponential marker"),
        }
    }
}

impl RealLiteral {
    // REVISIT
    #[cfg(test)]
    fn is_int(&self) -> bool {
        match *self {
            RealLiteral::Integer { .. } => true,
            _ => false,
        }
    }
    // REVISIT
    #[cfg(test)]
    fn is_dec(&self) -> bool {
        match *self {
            RealLiteral::Decimal { .. } => true,
            _ => false,
        }
    }
    // REVISIT
    #[cfg(test)]
    fn is_frac(&self) -> bool {
        match *self {
            RealLiteral::Fraction { .. } => true,
            _ => false,
        }
    }
    // REVISIT
    #[cfg(test)]
    fn digits(&self) -> &str {
        match *self {
            RealLiteral::Integer { ref digits, .. } => &digits,
            RealLiteral::Decimal { ref digits, .. } => &digits,
            RealLiteral::Fraction {
                numerator: (ref digits, _),
                ..
            } => &digits,
        }
    }
    // REVISIT
    #[cfg(test)]
    fn fuzzy_digits(&self) -> u8 {
        match *self {
            RealLiteral::Decimal { pounds, .. } => pounds,
            RealLiteral::Integer { pounds, .. } => pounds,
            RealLiteral::Fraction {
                numerator: (_, pounds),
                ..
            } => pounds,
        }
    }

    // REVISIT
    #[cfg(test)]
    fn decimal_point(&self) -> usize {
        match *self {
            RealLiteral::Decimal { point, .. } => point,
            _ => panic!("Not a decimal!"),
        }
    }

    // REVISIT
    #[cfg(test)]
    fn decimal_suffix(&self) -> &Option<DecSuffix> {
        match *self {
            RealLiteral::Decimal { ref suffix, .. } => &suffix,
            _ => panic!("Not a decimal!"),
        }
    }
    // REVISIT
    #[cfg(wtf)]
    fn num(&self) -> RealLiteral {
        match *self {
            RealLiteral::Fraction {
                numerator: (ref digits, pounds),
                ..
            } => RealLiteral::Integer {
                pounds: pounds,
                digits: digits.clone(),
            },
            _ => panic!("Not a fraction"),
        }
    }
    // REVISIT
    #[cfg(test)]
    fn den(&self) -> RealLiteral {
        match *self {
            RealLiteral::Fraction {
                denominator: (ref digits, pounds),
                ..
            } => RealLiteral::Integer {
                pounds: pounds,
                digits: digits.clone(),
            },
            _ => panic!("Not a fraction"),
        }
    }
}

impl ComplexLiteral {
    #[cfg(wtf)]
    fn is_cartesian(&self) -> bool {
        match *self {
            ComplexLiteral::Cartesian(..) => true,
            _ => false,
        }
    }
    #[cfg(wtf)]
    fn is_polar(&self) -> bool {
        match *self {
            ComplexLiteral::Polar(..) => true,
            _ => false,
        }
    }
    #[cfg(wtf)]
    fn is_real(&self) -> bool {
        match *self {
            ComplexLiteral::Real(..) => true,
            _ => false,
        }
    }
    // REVISIT
    #[cfg(test)]
    fn real_part(&self) -> Option<(Option<NumSign>, &RealLiteral)> {
        match *self {
            ComplexLiteral::Cartesian(None, _, _) => None,
            ComplexLiteral::Cartesian(Some((s, ref r)), _, _) => Some((s, r)),
            ComplexLiteral::Real(s, ref r) => Some((s, r)),
            _ => panic!("Not a cartesian or real literal"),
        }
    }
    // REVISIT
    #[cfg(test)]
    fn imaginary_part(&self) -> (NumSign, Option<&RealLiteral>) {
        match *self {
            ComplexLiteral::Cartesian(_, s, ref r) => (s, r.as_ref()),
            _ => panic!("Not a cartesian literal"),
        }
    }
    // REVISIT
    #[cfg(test)]
    fn modulus(&self) -> (Option<NumSign>, &RealLiteral) {
        match *self {
            ComplexLiteral::Polar(s, ref r, _, _) => (s, r),
            _ => panic!("Not a polar literal"),
        }
    }
    // REVISIT
    #[cfg(test)]
    fn phase(&self) -> (Option<NumSign>, &RealLiteral) {
        match *self {
            ComplexLiteral::Polar(_, _, s, ref r) => (s, r),
            _ => panic!("Not a polar literal"),
        }
    }
}

type Prefix = (Option<Exactness>, Option<Radix>);

fn parse_prefix<T: LexerIterator>(stream: &mut T) -> Result<Prefix, TokenErrorClass> {
    let mut exactness = None;
    let mut radix = None;

    loop {
        match stream.peek(0) {
            Some('#') => match stream.peek(1) {
                None => return Err(TokenErrorClass::UnfinishedNumber),
                Some(e) => match e {
                    'i' | 'e' if exactness.is_none() => {
                        exactness = Some(Exactness::from(e));
                    }
                    'b' | 'd' | 'x' | 'o' if radix.is_none() => radix = Some(Radix::from(e)),
                    _ => return Err(TokenErrorClass::BadRadix),
                },
            },
            _ => return Ok((exactness, radix)),
        }
        stream.advance(2);
    }
}

fn parse_complex<T: LexerIterator>(
    stream: &mut T,
    rad: Option<Radix>,
) -> Result<ComplexLiteral, TokenErrorClass> {
    let peek = stream.peek(0);
    let mut first_sign = None;

    if peek == Some('+') || peek == Some('-') {
        first_sign = peek.map(NumSign::from);
        stream.next();
    }

    // ±i
    if first_sign.is_some() && stream.peek(1) == Some('i') {
        if is_delimiter!(stream.peek(2)) {
            return Ok(ComplexLiteral::Cartesian(None, first_sign.unwrap(), None));
        } else {
            return Err(TokenErrorClass::BadDelimiter);
        }
    }

    let first_real = match parse_real(stream, rad) {
        Ok(l) => l,
        Err(e) => return Err(e),
    };

    let peek = stream.peek(0);
    let peek2 = stream.peek(1);
    let mut cartesian = true;
    let mut second_sign = None;

    match peek {
        Some('+') | Some('-') => {
            second_sign = peek.map(NumSign::from);
            stream.next();
        }
        Some('@') => {
            cartesian = false;

            match stream.peek(1) {
                Some('+') | Some('-') => {
                    second_sign = stream.peek(1).map(NumSign::from);
                    stream.next();
                }
                _ => {}
            }
            stream.next();
        }

        // ±ai
        Some('i') if first_sign.is_some() && is_delimiter!(peek2) => {
            stream.next();
            return Ok(ComplexLiteral::Cartesian(
                None,
                first_sign.unwrap(),
                Some(first_real),
            ));
        }

        _ if is_delimiter!(peek) => return Ok(ComplexLiteral::Real(first_sign, first_real)),
        _ => return Err(TokenErrorClass::BadDelimiter),
    }

    // a±i
    if cartesian && stream.peek(0) == Some('i') && is_delimiter!(stream.peek(1)) {
        return Ok(ComplexLiteral::Cartesian(
            Some((first_sign, first_real)),
            second_sign.unwrap(),
            None,
        ));
    }

    let second_real = match parse_real(stream, rad) {
        Ok(l) => l,
        Err(e) => return Err(e),
    };

    if cartesian {
        if stream.peek(0) == Some('i') {
            stream.next();
        } else {
            return Err(TokenErrorClass::BadCartesian);
        }
    }

    let peek = stream.peek(0);
    match peek {
        _ if !is_delimiter!(peek) => Err(TokenErrorClass::BadDelimiter),
        _ => if cartesian {
            Ok(ComplexLiteral::Cartesian(
                Some((first_sign, first_real)),
                second_sign.unwrap(),
                Some(second_real),
            ))
        } else {
            Ok(ComplexLiteral::Polar(
                first_sign,
                first_real,
                second_sign,
                second_real,
            ))
        },
    }
}

// It does not err if last char is not a delimiter
fn parse_real<T: LexerIterator>(
    stream: &mut T,
    r: Option<Radix>,
) -> Result<RealLiteral, TokenErrorClass> {
    #[derive(Debug, PartialEq)]
    enum Number {
        Int,
        Dec,
        Frac,
    }

    macro_rules! is_valid_digit {
        ($digit:expr, $radix:expr, $decimal:expr) => {{
            match $digit {
                'a'...'f' if !$decimal && $radix == 16 => true,
                '8' | '9' if $radix >= 10 => true,
                '2'...'7' if $radix >= 8 => true,
                '0' | '1' => true,
                _ => false,
            }
        }};
    }

    let radix = r.unwrap_or(Radix::Decimal) as u8;

    let mut num_type = Number::Int;
    let mut digits = String::new();
    let mut pounds = 0;
    let mut numerator = None;
    let mut point = 0;
    let mut suffix = None;

    loop {
        let peek = stream.peek(0);

        match peek {
            Some('#') if !digits.is_empty() => {
                pounds += 1;
            }
            Some('/') => match num_type {
                Number::Int if !digits.is_empty() => {
                    num_type = Number::Frac;
                    numerator = Some((digits, pounds));
                    digits = String::new();
                    pounds = 0;
                }
                _ => return Err(TokenErrorClass::BadFraction),
            },
            Some('.') => match num_type {
                Number::Int if radix == 10 => {
                    point = digits.len() + (pounds as usize);
                    num_type = Number::Dec;
                }
                _ => return Err(TokenErrorClass::BadDecimal),
            },

            // https://github.com/rust-lang/rust/issues/26251
            Some('0'...'9') | Some('a'...'f') | Some('s') | Some('l') => if pounds == 0
                && is_valid_digit!(peek.unwrap(), radix, num_type == Number::Dec)
            {
                digits.push(peek.unwrap());
            } else if num_type == Number::Dec {
                let marker = peek.unwrap();
                if marker == 'e' || marker == 'f' || marker == 's' || marker == 'l' || marker == 'd'
                {
                    match parse_suffix(stream).ok() {
                        sf @ Some(_) => {
                            suffix = sf;
                            break;
                        }
                        _ => return Err(TokenErrorClass::BadSuffix),
                    }
                } else {
                    return Err(TokenErrorClass::BadMarker);
                }
            } else {
                return Err(TokenErrorClass::BadDigit);
            },

            _ => break,
        }

        stream.next();
    }

    if digits.is_empty() {
        return Err(TokenErrorClass::EmptyNumber);
    }

    Ok(match num_type {
        Number::Int => RealLiteral::Integer {
            digits: digits,
            pounds: pounds,
        },
        Number::Dec => RealLiteral::Decimal {
            digits: digits,
            pounds: pounds,
            point: point,
            suffix: suffix,
        },
        Number::Frac => RealLiteral::Fraction {
            numerator: numerator.unwrap(),
            denominator: (digits, pounds),
        },
    })
}

// It does not err if last char is not a delimiter
// It is always called with a valid marker in the stream
fn parse_suffix<T: LexerIterator>(stream: &mut T) -> Result<DecSuffix, String> {
    let marker = ExpMarker::from(stream.next().unwrap());
    let mut sign = None;
    let mut digits = String::new();

    match stream.peek(0) {
        Some('+') | Some('-') => {
            sign = Some(NumSign::from(stream.peek(0).unwrap()));
            stream.next();
        }
        _ => {}
    }

    loop {
        match stream.peek(0) {
            Some(x) if is_digit!(x) => {
                digits.push(x);
                stream.next();
            }
            _ => break,
        }
    }

    if digits.is_empty() {
        Err("bad marker!".to_string())
    } else {
        Ok(DecSuffix {
            sign: sign,
            marker: marker,
            digits: digits,
        })
    }
}
