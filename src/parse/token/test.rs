use super::*;
use super::{next_token};

fn assert_next(code: &str, token: Token) {
    let result = next_token(&mut Chars::from_str(code)).ok().unwrap().unwrap();
    println!("{:?}", result);
    assert!(result == token);
}

fn assert_next_identifier(code: &str, identifier: &str) {
    match next_token(&mut Chars::from_str(code)).ok().unwrap().unwrap() {
        Token::Identifier(id) => {
            println!("{:?} {:?}", id, identifier);
            assert!(id == identifier);
        },
        _ => panic!("Not an identifier")
    }
}

#[test]
fn whitespace_test() {
    assert_next(" (", Token::Open);
    assert_next("\n (", Token::Open);
}

#[test]
fn clopen_test() {
    assert_next("(asdd", Token::Open);
    assert_next(")#12", Token::Close);
}

#[test]
fn booleans_test() {
    assert_next("#t 13", Token::Boolean(true));
    assert_next("#T(1)", Token::Boolean(true));
    assert_next("#F 4t8i90", Token::Boolean(false));
    assert_next("#f)@s89g", Token::Boolean(false));
}

#[test]
fn open_vector_test() {
    assert_next("#(@klsdj", Token::OpenVector);
}

#[test]
fn strings_test() {
    assert_next("\"asdf\"", Token::String("asdf".to_string()));
    assert_next("\"foo\\\"#bar\"", Token::String("foo\"#bar".to_string()));
    assert_next("\"foo bar\"", Token::String("foo bar".to_string()));
    assert_next("\"foo\nbar\"", Token::String("foo\nbar".to_string()));
}

#[test]
fn backquote_test() {
    assert_next("`#24@", Token::BackQuote);
}

#[test]
fn dot_test() {
    assert_next(".(#24@", Token::Dot);
}

#[test]
fn single_quote_test() {
    assert_next("'.(#24@", Token::SingleQuote);
}

#[test]
fn commas_test() {
    assert_next(",(#skdf", Token::Comma);
    assert_next(",@)#skdf", Token::CommaAt);
}

#[test]
fn chars_test() {
    assert_next("#\\a", Token::Character('a'));
    assert_next("#\\s", Token::Character('s'));
    assert_next("#\\S", Token::Character('S'));
    assert_next("#\\s foo", Token::Character('s'));
    assert_next("#\\S foo", Token::Character('S'));
    assert_next("#\\space", Token::Character(' '));
    assert_next("#\\space 2", Token::Character(' '));
    assert_next("#\\sPaCe 2", Token::Character(' '));
    assert_next("#\\n", Token::Character('n'));
    assert_next("#\\N", Token::Character('N'));
    assert_next("#\\n foo", Token::Character('n'));
    assert_next("#\\N foo", Token::Character('N'));
    assert_next("#\\newline", Token::Character('\n'));
    assert_next("#\\newline 2", Token::Character('\n'));

    // assert_next("#\\spac", Token::Character('s'));
    // assert_next("#\\spac1", Token::Character('s'));
    // assert_next("#\\newlin", Token::Character('n'));
    // assert_next("#\\newlin1", Token::Character('n'));
    // assert_next("#\\Newlin1", Token::Character('N'));
}

#[test]
fn special_identifiers_test() {
    assert_next_identifier("+", "+");
    assert_next_identifier("+ a", "+");
    assert_next_identifier("-", "-");
    assert_next_identifier("- a", "-");
    assert_next_identifier("...", "...");
    assert_next_identifier("... a", "...");
}

#[test]
fn identifiers_test() {
    assert_next_identifier("a", "a");
    assert_next_identifier("a;", "a");
    assert_next_identifier("asdf", "asdf");
    assert_next_identifier("asdf;", "asdf");
    assert_next_identifier("a0", "a0");

    for &c in [
        // Special initials
        '!', '$', '%', '&', '*',
        '/', ':', '<', '=', '>',
        '?', '^', '_', '~',
        // Special subsequents
        '+', '-', '.', '@'
    ].iter() {
        let mut s = "a".to_string();
        s.push(c);
        assert_next_identifier(&s[..], &s[..])
    }

    for &c in [
        // Special initials
        '!', '$', '%', '&', '*',
        '/', ':', '<', '=', '>',
        '?', '^', '_', '~',
        // Special subsequents
        '+', '-', '.', '@'
    ].iter() {
        let mut s = "a".to_string();
        s.push(c);
        s.push(';');
        assert_next_identifier(&s[..], &s[0..2])
    }

    #[test]
    fn comments_test() {
        assert_next(";foobar\n#t", Token::Boolean(true));
    }

    #[test]

    fn comments_whitespace_test() {
        let mut stream = Chars::from_str("#\\spa;foobar\nce");
        assert!(next_token(&mut stream).is_err());
    }
}
