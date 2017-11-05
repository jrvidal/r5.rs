use super::*;
use super::super::chars::Chars;
use super::{parse_complex, parse_prefix, parse_real, parse_suffix};

#[test]
fn prefix() {
    assert!(parse_prefix(&mut Chars::from_str("")).ok().unwrap() == (None, None));
    assert!(
        parse_prefix(&mut Chars::from_str("#e")).ok().unwrap() == (Some(Exactness::Exact), None)
    );
    assert!(parse_prefix(&mut Chars::from_str("#O")).ok().unwrap() == (None, Some(Radix::Octal)));
    assert!(
        parse_prefix(&mut Chars::from_str("#d#I")).ok().unwrap()
            == (Some(Exactness::Inexact), Some(Radix::Decimal))
    );
    assert!(parse_prefix(&mut Chars::from_str("#d#")).is_err());
    assert!(parse_prefix(&mut Chars::from_str("#\\")).is_err());
}

#[test]
fn suffix() {
    macro_rules! assert_suffix {
        ($t:ident, {$sign:expr, $marker:expr, $digits:expr}) => ({
            assert!($t.sign == $sign);
            assert!($t.marker == $marker);
            assert!($t.digits == $digits);
        })
    }

    let s = "e+1";
    let t = parse_suffix(&mut Chars::from_str(s)).ok().unwrap();
    assert_suffix!(t, {
        Some(NumSign::Plus),
        ExpMarker::Default,
        "1"
    });

    let s = "f-112";
    let t = parse_suffix(&mut Chars::from_str(s)).ok().unwrap();
    assert_suffix!(t, {
        Some(NumSign::Minus),
        ExpMarker::Single,
        "112"
    });

    let s = "l010";
    let t = parse_suffix(&mut Chars::from_str(s)).ok().unwrap();
    assert_suffix!(t, {
        None,
        ExpMarker::Long,
        "010"
    });
}

#[test]
#[should_panic]
fn suffix_bad_marker_empty() {
    assert!(parse_suffix(&mut Chars::from_str("")).is_err());
}

#[test]
#[should_panic]
fn suffix_bad_marker_nonexistant() {
    assert!(parse_suffix(&mut Chars::from_str("z")).is_err());
}

fn assert_int(t: &RealLiteral, fuzzy: u8, digits: &str) {
    assert!(t.is_int());
    assert!(t.fuzzy_digits() == fuzzy);
    assert!(t.digits() == digits);
}

#[test]
fn parse_real_with_integer() {
    let n = "1";
    let t = parse_real(&mut Chars::from_str(n), None).ok().unwrap();
    // Manual
    assert!(t.is_int());
    assert!(t.fuzzy_digits() == 0);
    assert!(t.digits() == n);

    let n = "123";
    let t = parse_real(&mut Chars::from_str(n), None).ok().unwrap();
    assert_int(&t, 0, n);

    // Other radixes
    let n = "1010111";
    let t = parse_real(&mut Chars::from_str(n), Some(Radix::Binary))
        .ok()
        .unwrap();
    assert_int(&t, 0, n);

    // Other radixes, case-insensitive
    let n = "10A01f1";
    let t = parse_real(&mut Chars::from_str(n), Some(Radix::Hexadecimal))
        .ok()
        .unwrap();
    assert_int(&t, 0, "10a01f1");

    // Fuzzy digits
    let n = "123#";
    let t = parse_real(&mut Chars::from_str(n), None).ok().unwrap();
    assert_int(&t, 1, "123");

    // Invalid symbols don't cause errors
    let n = "123z";
    let t = parse_real(&mut Chars::from_str(n), None).ok().unwrap();
    assert_int(&t, 0, "123");
}

#[test]
fn parse_real_with_integer_error() {
    // Invalid digits
    let n = "123a";
    let t = parse_real(&mut Chars::from_str(n), None);
    assert!(t.is_err());

    let n = "102";
    let t = parse_real(&mut Chars::from_str(n), Some(Radix::Binary));
    assert!(t.is_err());

    // Digits after fuzzy
    let n = "1#2";
    let t = parse_real(&mut Chars::from_str(n), None);
    assert!(t.is_err());

    // Invalid symbols
    let n = "@34";
    let t = parse_real(&mut Chars::from_str(n), None);
    assert!(t.is_err());

    let n = "/34";
    let t = parse_real(&mut Chars::from_str(n), None);
    assert!(t.is_err());
}


macro_rules! assert_frac {
    ($t:expr, [$fuzzy:expr, $digits:expr], [$fuzzy_d:expr, $digits_d:expr]) => ({
        assert!($t.is_frac());
        assert!($t.fuzzy_digits() == $fuzzy);
        assert!($t.digits() == $digits);
        let denominator = $t.den();
        assert!(denominator.is_int());
        assert!(denominator.fuzzy_digits() == $fuzzy_d);
        assert!(denominator.digits() == $digits_d);
    })
}
#[test]
fn parse_real_with_fraction() {
    let n = "1/2";
    let t = parse_real(&mut Chars::from_str(n), None).ok().unwrap();
    assert_frac!(t, [0, "1"], [0, "2"]);

    // Fuzzy digits
    let n = "12#/2";
    let t = parse_real(&mut Chars::from_str(n), None).ok().unwrap();
    assert_frac!(t, [1, "12"], [0, "2"]);


    let n = "12#/23#";
    let t = parse_real(&mut Chars::from_str(n), None).ok().unwrap();
    assert_frac!(t, [1, "12"], [1, "23"]);
}

#[test]
fn parse_real_with_fraction_error() {
    // No denominator
    let n = "1/";
    let t = parse_real(&mut Chars::from_str(n), None);
    assert!(t.is_err());

    // Two denominators
    let n = "1/2/";
    let t = parse_real(&mut Chars::from_str(n), None);
    assert!(t.is_err());
}

macro_rules! assert_dec {
    ($t:expr, $fuzzy:expr, $digits:expr, $point:expr, BASIC) => ({
        assert!($t.is_dec());
        assert!($t.fuzzy_digits() == $fuzzy);
        assert!($t.digits() == $digits);
        assert!($t.decimal_point() == $point);
    });
    ($t:expr, $fuzzy:expr, $digits:expr, $point:expr) => ({
        assert_dec!($t, $fuzzy, $digits, $point, BASIC);
        assert!($t.decimal_suffix().is_none());
    });
    ($t:expr, $fuzzy:expr, $digits:expr, $point:expr, $marker:expr) => ({
        assert_dec!($t, $fuzzy, $digits, $point, BASIC);
        let marker = $t.decimal_suffix().as_ref().unwrap();
        assert!(&$marker == marker);
    });
}

#[test]
fn parse_real_with_decimal() {
    let n = "1.2";
    let t = parse_real(&mut Chars::from_str(n), None).ok().unwrap();
    assert_dec!(t, 0, "12", 1);


    let n = "0.12";
    let t = parse_real(&mut Chars::from_str(n), None).ok().unwrap();
    assert_dec!(t, 0, "012", 1);

    let n = ".12";
    let t = parse_real(&mut Chars::from_str(n), None).ok().unwrap();
    assert_dec!(t, 0, "12", 0);

    let n = "12.";
    let t = parse_real(&mut Chars::from_str(n), None).ok().unwrap();
    assert_dec!(t, 0, "12", 2);

    let n = "1.2#";
    let t = parse_real(&mut Chars::from_str(n), None).ok().unwrap();
    assert_dec!(t, 1, "12", 1);

    let n = "12#.#";
    let t = parse_real(&mut Chars::from_str(n), None).ok().unwrap();
    assert_dec!(t, 2, "12", 3);

    let n = "12#.";
    let t = parse_real(&mut Chars::from_str(n), None).ok().unwrap();
    assert_dec!(t, 1, "12", 3);

    let n = "12.E12";
    let t = parse_real(&mut Chars::from_str(n), None).ok().unwrap();
    assert_dec!(
        t,
        0,
        "12",
        2,
        DecSuffix {
            sign: None,
            digits: "12".to_string(),
            marker: ExpMarker::Default,
        }
    );

    let n = "12.3#f-44";
    let t = parse_real(&mut Chars::from_str(n), None).ok().unwrap();
    assert_dec!(
        t,
        1,
        "123",
        2,
        DecSuffix {
            sign: Some(NumSign::Minus),
            digits: "44".to_string(),
            marker: ExpMarker::Single,
        }
    );
}

#[test]
fn parse_real_with_decimal_error() {
    // Non-decimal radix
    let n = "12.a";
    let t = parse_real(&mut Chars::from_str(n), None);
    assert!(t.is_err());

    let n = "1a.3";
    let t = parse_real(&mut Chars::from_str(n), None);
    assert!(t.is_err());

    let n = "12.3";
    let t = parse_real(&mut Chars::from_str(n), Some(Radix::Octal));
    assert!(t.is_err());

    // No non-fuzzy digits
    let n = ".#";
    let t = parse_real(&mut Chars::from_str(n), None);
    assert!(t.is_err());

    // Mixing fractions and decs
    let n = "1.2/";
    let t = parse_real(&mut Chars::from_str(n), None);
    assert!(t.is_err());
}

#[test]
fn parse_complex_with_cartesian() {
    let n = "1+i";
    let t = parse_complex(&mut Chars::from_str(n), None).ok().unwrap();
    let (s, r) = t.real_part().unwrap();
    assert!(s == None);
    assert_int(r, 0, "1");
    let (s, r) = t.imaginary_part();
    assert!(s == NumSign::Plus);
    assert!(r == None);

    let n = "1#-2I";
    let t = parse_complex(&mut Chars::from_str(n), None).ok().unwrap();
    let (s, r) = t.real_part().unwrap();
    assert!(s == None);
    assert_int(r, 1, "1");
    let (s, r) = t.imaginary_part();
    assert!(s == NumSign::Minus);
    assert_int(r.unwrap(), 0, "2");

    let n = "-1.3+2/1i";
    let t = parse_complex(&mut Chars::from_str(n), None).ok().unwrap();
    let (s, r) = t.real_part().unwrap();
    assert!(s == Some(NumSign::Minus));
    assert_dec!(r, 0, "13", 1);
    let (s, r) = t.imaginary_part();
    assert!(s == NumSign::Plus);
    assert_frac!(r.unwrap(), [0, "2"], [0, "1"]);

    let n = "+3aFi";
    let t = parse_complex(&mut Chars::from_str(n), Some(Radix::Hexadecimal))
        .ok()
        .unwrap();
    assert!(None == t.real_part());
    let (s, r) = t.imaginary_part();
    assert!(s == NumSign::Plus);
    assert_int(r.unwrap(), 0, "3af");

    // With delimiter
    let n = "-1.3+2/1I)";
    let t = parse_complex(&mut Chars::from_str(n), None).ok().unwrap();
    let (s, r) = t.real_part().unwrap();
    assert!(s == Some(NumSign::Minus));
    assert_dec!(r, 0, "13", 1);
    let (s, r) = t.imaginary_part();
    assert!(s == NumSign::Plus);
    assert_frac!(r.unwrap(), [0, "2"], [0, "1"]);
}

#[test]
fn parse_complex_with_polar() {
    let n = "+1@2";
    let t = parse_complex(&mut Chars::from_str(n), None).ok().unwrap();
    let (s, r) = t.modulus();
    assert!(s == Some(NumSign::Plus));
    assert_int(r, 0, "1");
    let (s, r) = t.phase();
    assert!(s == None);
    assert_int(r, 0, "2");

    let n = "+12#.#@-23";
    let t = parse_complex(&mut Chars::from_str(n), None).ok().unwrap();
    let (s, r) = t.modulus();
    assert!(s == Some(NumSign::Plus));
    assert_dec!(r, 2, "12", 3);
    let (s, r) = t.phase();
    assert!(s == Some(NumSign::Minus));
    assert_int(r, 0, "23");

    let n = "3/A#@3#";
    let t = parse_complex(&mut Chars::from_str(n), Some(Radix::Hexadecimal))
        .ok()
        .unwrap();
    let (s, r) = t.modulus();
    assert!(s == None);
    assert_frac!(r, [0, "3"], [1, "a"]);
    let (s, r) = t.phase();
    assert!(s == None);
    assert_int(r, 1, "3");

    // With delimiter
    let n = "+12#.#@-23\"";
    let t = parse_complex(&mut Chars::from_str(n), None).ok().unwrap();
    let (s, r) = t.modulus();
    assert!(s == Some(NumSign::Plus));
    assert_dec!(r, 2, "12", 3);
    let (s, r) = t.phase();
    assert!(s == Some(NumSign::Minus));
    assert_int(r, 0, "23");
}

#[test]
fn parse_complex_error() {
    // Bad delimiters
    let n = "13&";
    let t = parse_complex(&mut Chars::from_str(n), None);
    assert!(t.is_err());

    let n = "13+i@";
    let t = parse_complex(&mut Chars::from_str(n), None);
    assert!(t.is_err());

    // Bad cartesian
    let n = "13+2/1";
    let t = parse_complex(&mut Chars::from_str(n), None);
    assert!(t.is_err());

    // Imaginary without sign
    let n = "13i";
    let t = parse_complex(&mut Chars::from_str(n), None);
    assert!(t.is_err());
}
