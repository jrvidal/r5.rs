
use std::ascii::AsciiExt;
use super::Peekable;

#[cfg(test)]
mod test;

#[derive(Debug, PartialEq, Clone)]
pub struct NumberToken {
    exactness: Option<Exactness>,
    radix: Option<Radix>,
    number: ComplexLiteral
}

#[derive(Debug, PartialEq, Clone)]
pub enum ComplexLiteral {
    Cartesian(Option<(Option<NumSign>, RealLiteral)>, NumSign, Option<RealLiteral>),
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
    Long,
    Default,
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

impl From<char> for ExpMarker {
    fn from(c: char) -> ExpMarker {
        match c.to_ascii_lowercase() {
            'e' => ExpMarker::Default,
            's' => ExpMarker::Short,
            'l' => ExpMarker::Long,
            'f' => ExpMarker::Single,
            'd' => ExpMarker::Double,
            _ => panic!("Invalid exponential marker")
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

impl ComplexLiteral {
    fn is_cartesian(&self) -> bool {
        match *self {
            ComplexLiteral::Cartesian(..) => true,
            _ => false
        }
    }
    fn is_polar(&self) -> bool {
        match *self {
            ComplexLiteral::Polar(..) => true,
            _ => false
        }
    }
    fn is_real(&self) -> bool {
        match *self {
            ComplexLiteral::Real(..) => true,
            _ => false
        }
    }
    fn real_part(&self) -> Option<(Option<NumSign>, &RealLiteral)> {
        match *self {
            ComplexLiteral::Cartesian(None, _, _) => None,
            ComplexLiteral::Cartesian(Some((s, ref r)), _, _) => Some((s, r)),
            ComplexLiteral::Real(s, ref r) => Some((s, r)),
            _ => panic!("Not a cartesian or real literal")
        }
    }
    fn imaginary_part(&self) -> (NumSign, Option<&RealLiteral>) {
        match *self {
            ComplexLiteral::Cartesian(_, s, ref r) => (s, r.as_ref()),
            _ => panic!("Not a cartesian literal")
        }
    }
    fn modulus(&self) -> (Option<NumSign>, &RealLiteral) {
        match *self {
            ComplexLiteral::Polar(s, ref r, _, _) => (s, r),
            _ => panic!("Not a polar literal")
        }
    }
    fn phase(&self) -> (Option<NumSign>, &RealLiteral) {
        match *self {
            ComplexLiteral::Polar(_, _, s, ref r) => (s, r),
            _ => panic!("Not a polar literal")
        }
    }
}

type Prefix = (Option<Exactness>, Option<Radix>);

// stream is guaranteed to be non-empty
pub fn parse_number<T: Peekable> (mut stream: &mut T) -> Result<NumberToken, String> {
    parse_prefix(stream).and_then(|(e, r)| {
        parse_complex(stream, r).map(|n| (e, r, n))
    }).map(|(e, r, n)| {
        NumberToken {
            exactness: e,
            radix: r,
            number:n
        }
    })
}

fn parse_prefix<T: Peekable>(mut stream: &mut T) -> Result<Prefix, String> {
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

fn parse_complex<T: Peekable>(stream: &mut T, rad: Option<Radix>) -> Result<ComplexLiteral, String> {

    let peek = stream.small_peek();
    let mut first_sign = None;

    if peek[0] == Some('+') || peek[0] == Some('-') {
        first_sign = peek[0].map(NumSign::from);
        stream.next();
    }

    // ±i
    if first_sign.is_some() && peek[1].map(|c| c.to_ascii_lowercase()) == Some('i') {
        if is_delimiter!(peek[2]) {
            return Ok(ComplexLiteral::Cartesian(None, first_sign.unwrap(), None));
        } else {
            return Err("bad delimiter".to_string());
        }
    }

    let first_real = match parse_real(stream, rad) {
        Ok(l) => l,
        Err(e) => return Err(e)
    };

    let peek = stream.small_peek();
    let mut cartesian = true;
    let mut second_sign = None;

    match peek[0].map(|c| c.to_ascii_lowercase()) {
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
            stream.next();
        },

        // ±ai
        Some('i') if first_sign.is_some() && is_delimiter!(peek[1]) => {
            stream.next();
            return Ok(ComplexLiteral::Cartesian(None, first_sign.unwrap(), Some(first_real)));
        },

        _ if is_delimiter!(peek[0]) => return Ok(ComplexLiteral::Real(first_sign, first_real)),
        _ => return Err("bad delimiter".to_string()),
    }


    let peek = stream.small_peek();

    // a±i
    if cartesian && peek[0].map(|c| c.to_ascii_lowercase()) == Some('i') && is_delimiter!(peek[1]) {
        return Ok(ComplexLiteral::Cartesian(Some((first_sign, first_real)), second_sign.unwrap(), None));
    }

    let second_real = match parse_real(stream, rad) {
        Ok(l) => l,
        Err(e) => return Err(e)
    };

    if cartesian {
        if stream.small_peek()[0].map(|c| c.to_ascii_lowercase()) == Some('i') {
            stream.next();
        } else {
            return Err("bad cartesian".to_string());
        }
    }

    let peek = stream.small_peek();

    match peek[0] {
        _ if !is_delimiter!(peek[0]) => return Err("bad delimiter".to_string()),
        _ => if cartesian {
            Ok(ComplexLiteral::Cartesian(
                Some((first_sign, first_real)), second_sign.unwrap(), Some(second_real)
            ))
        } else {
            Ok(ComplexLiteral::Polar(first_sign, first_real, second_sign, second_real))
        }
    }
}

// It does not err if last char is not a delimiter
fn parse_real<T: Peekable>(mut stream: &mut T, r: Option<Radix>) -> Result<RealLiteral, String> {
    #[derive(Debug, PartialEq)]
    enum Number {
        Int,
        Decimal,
        Fraction
    }

    macro_rules! is_valid_digit {
        ($digit:expr, $radix:expr, $decimal:expr) => ({
            match $digit {
                'a'...'f' if !$decimal && $radix == 16 => true,
                '8' | '9' if $radix >= 10 => true,
                '2'...'7' if $radix >= 8 => true,
                '0' | '1' => true,
                _ => false
            }
        })
    }

    let radix = r.unwrap_or(Radix::Decimal) as u8;

    let mut num_type = Number::Int;
    let mut digits = String::new();
    let mut pounds = 0;
    // let mut accepts_pounds = false;
    let mut numerator = None;
    let mut point = 0;
    let mut suffix = None;

    loop {
        let peek = stream.small_peek()[0].map(|c| c.to_ascii_lowercase());

        match peek {
            Some('#') if digits.len() > 0 => {
                pounds += 1;
            },
            Some('/') => match num_type {
                Number::Int if digits.len() > 0 => {
                    num_type = Number::Fraction;
                    numerator = Some((digits, pounds));
                    digits = String::new();
                    pounds = 0;
                },
                _ => return Err("bad fraction".to_string())
            },
            Some('.') => match num_type {
                Number::Int if radix == 10 => {
                    point = digits.len() + (pounds as usize);
                    num_type = Number::Decimal;
                },
                _ => return Err("bad decimal".to_string())
            },

            // https://github.com/rust-lang/rust/issues/26251
            Some('0'...'9') | Some('a'...'f') | Some('s') | Some('l')  => {
                if pounds == 0 && is_valid_digit!(peek.unwrap(), radix, num_type == Number::Decimal) {
                    digits.push(peek.unwrap());
                } else if num_type == Number::Decimal {
                    let marker = peek.unwrap();
                    if marker == 'e' || marker == 'f' || marker == 's' || marker == 'l' || marker == 'd' {
                        match parse_suffix(stream).ok() {
                            sf @ Some(_) => {
                                suffix = sf;
                                break;
                            },
                            _ => return Err("bad suffix".to_string()),
                        }
                    } else {
                        return Err("bad marker".to_string());
                    }
                } else {
                    return Err("bad digit".to_string());
                }
            }

            _ => break
        }

        stream.next();
    }

    if digits.len() == 0 {
        return Err("empty number".to_string());
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

// It does not err if last char is not a delimiter
// It is always called with a valid marker in the stream
fn parse_suffix<T: Peekable>(stream: &mut T) -> Result<DecSuffix, String> {
    let mut marker = ExpMarker::from(stream.next().unwrap());
    let mut sign = None;
    let mut digits = String::new();

    let peek = stream.small_peek();

    match peek[0] {
        Some('+') | Some('-') => {
            sign = Some(NumSign::from(peek[0].unwrap()));
            stream.next();
        },
        _ => {}
    }

    loop {
        let peek = stream.small_peek();

        match peek[0] {
            Some(x) if is_digit!(x) => {
                digits.push(x);
                stream.next();
            },
            _ => break
        }
    }

    if digits.len() == 0 {
        return Err("bad marker!".to_string());
    } else {
        return Ok(DecSuffix {
            sign: sign,
            marker: marker,
            digits: digits
        });
    }
}
