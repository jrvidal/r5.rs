use crate::lexer::chars::Chars;
use super::next_token;
use super::*;

fn assert_next(code: &str, token_type: TokenType) {
    let result = next_token(&mut Chars::from_str(code))
        .ok()
        .unwrap()
        .unwrap();
    println!("{:?}", result);
    assert!(result.ty == token_type);
}

fn assert_next_identifier(code: &str, identifier: &str) {
    match next_token(&mut Chars::from_str(code))
        .expect("valid token")
        .expect("non-empty")
        .ty
    {
        TokenType::Identifier(id) => {
            println!("{:?} {:?}", id, identifier);
            assert!(id == identifier);
        }
        _ => panic!("Not an identifier"),
    }
}

fn assert_tokens(code: &str, token_types: Vec<TokenType>) {
    let mut stream = Chars::from_str(code);
    for ty in token_types.into_iter() {
        let result = next_token(&mut stream).ok().unwrap().unwrap();
        println!("{:?}", result);
        assert!(result.ty == ty);
    }
    let next = next_token(&mut stream).ok().unwrap();
    println!("{:?}", next);
    assert!(next.is_none(), "Unfinished stream!");
}

#[test]
fn whitespace_test() {
    assert_next(" (", TokenType::Open);
    assert_next("\n (", TokenType::Open);
}

#[test]
fn clopen_test() {
    assert_next("(asdd", TokenType::Open);
    assert_next(")#12", TokenType::Close);
}

#[test]
fn booleans_test() {
    assert_next("#t 13", TokenType::Boolean(true));
    assert_next("#T(1)", TokenType::Boolean(true));
    assert_next("#F 4t8i90", TokenType::Boolean(false));
    assert_next("#f)@s89g", TokenType::Boolean(false));
}

#[test]
fn open_vector_test() {
    assert_next("#(@klsdj", TokenType::OpenVector);
}

#[test]
fn strings_test() {
    assert_next("\"asdf\"", TokenType::String("asdf".to_string()));
    assert_next("\"foo\\\"#bar\"", TokenType::String("foo\"#bar".to_string()));
    assert_next("\"foo bar\"", TokenType::String("foo bar".to_string()));
    assert_next("\"foo\nbar\"", TokenType::String("foo\nbar".to_string()));
}

#[test]
fn backquote_test() {
    assert_next("`#24@", TokenType::BackQuote);
}

#[test]
fn dot_test() {
    assert_next(".(#24@", TokenType::Dot);
}

#[test]
fn single_quote_test() {
    assert_next("'.(#24@", TokenType::SingleQuote);
}

#[test]
fn commas_test() {
    assert_next(",(#skdf", TokenType::Comma);
    assert_next(",@)#skdf", TokenType::CommaAt);
}

#[test]
fn chars_test() {
    assert_next("#\\a", TokenType::Character('a'));
    assert_next("#\\s", TokenType::Character('s'));
    assert_next("#\\S", TokenType::Character('S'));
    assert_next("#\\s foo", TokenType::Character('s'));
    assert_next("#\\S foo", TokenType::Character('S'));
    assert_next("#\\space", TokenType::Character(' '));
    assert_next("#\\space 2", TokenType::Character(' '));
    assert_next("#\\sPaCe 2", TokenType::Character(' '));
    assert_next("#\\n", TokenType::Character('n'));
    assert_next("#\\N", TokenType::Character('N'));
    assert_next("#\\n foo", TokenType::Character('n'));
    assert_next("#\\N foo", TokenType::Character('N'));
    assert_next("#\\newline", TokenType::Character('\n'));
    assert_next("#\\newline 2", TokenType::Character('\n'));

    assert_tokens("#\\a", vec![TokenType::Character('a')]);
    assert_tokens("#\\S", vec![TokenType::Character('S')]);

    // assert_next("#\\spac", TokenType::Character('s'));
    // assert_next("#\\spac1", TokenType::Character('s'));
    // assert_next("#\\newlin", TokenType::Character('n'));
    // assert_next("#\\newlin1", TokenType::Character('n'));
    // assert_next("#\\Newlin1", TokenType::Character('N'));
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
        '!',
        '$',
        '%',
        '&',
        '*',
        '/',
        ':',
        '<',
        '=',
        '>',
        '?',
        '^',
        '_',
        '~',
        // Special subsequents
        '+',
        '-',
        '.',
        '@',
    ].iter()
    {
        let mut s = "a".to_string();
        s.push(c);
        assert_next_identifier(&s[..], &s[..])
    }

    for &c in [
        // Special initials
        '!',
        '$',
        '%',
        '&',
        '*',
        '/',
        ':',
        '<',
        '=',
        '>',
        '?',
        '^',
        '_',
        '~',
        // Special subsequents
        '+',
        '-',
        '.',
        '@',
    ].iter()
    {
        let mut s = "a".to_string();
        s.push(c);
        s.push(';');
        assert_next_identifier(&s[..], &s[0..2])
    }
}

#[test]
fn comments_test() {
    assert_next(";foobar\n#t", TokenType::Boolean(true));
}

#[test]
fn comments_whitespace_test() {
    let mut stream = Chars::from_str("#\\spa;foobar\nce");
    assert!(next_token(&mut stream).is_err());
}


#[test]
fn simple_pos() {
    let mut stream = Chars::from_str("#\\a");
    assert_eq!(next_token(&mut stream).unwrap().unwrap().span, (0, 3));

    let mut stream = Chars::from_str("\"123456\"");
    assert_eq!(next_token(&mut stream).unwrap().unwrap().span, (0, 8));

    let mut stream = Chars::from_str("\"千葉\"");
    assert_eq!(next_token(&mut stream).unwrap().unwrap().span, (0, 8));

    let mut stream = Chars::from_str(";foobar\na ;quzbaz\nb");
    assert_eq!(next_token(&mut stream).unwrap().unwrap().span, (8, 9));
    assert_eq!(next_token(&mut stream).unwrap().unwrap().span, (18, 19));

    let mut stream = Chars::from_str("a1234567 (");
    assert_eq!(next_token(&mut stream).unwrap().unwrap().span, (0, 8));
    assert_eq!(next_token(&mut stream).unwrap().unwrap().span, (9, 10));

    let mut stream = Chars::from_str("a b");
    assert_eq!(next_token(&mut stream).unwrap().unwrap().span, (0, 1));
    assert_eq!(next_token(&mut stream).unwrap().unwrap().span, (2, 3));

    let mut stream = Chars::from_str("a #\\t");
    assert_eq!(next_token(&mut stream).unwrap().unwrap().span, (0, 1));
    assert_eq!(next_token(&mut stream).unwrap().unwrap().span, (2, 5));

    let mut stream = Chars::from_str("#\\space \ta");
    assert_eq!(next_token(&mut stream).unwrap().unwrap().span, (0, 7));
    assert_eq!(next_token(&mut stream).unwrap().unwrap().span, (9, 10));

    let mut stream = Chars::from_str("a +12.3 b");
    assert_eq!(next_token(&mut stream).unwrap().unwrap().span, (0, 1));
    assert_eq!(next_token(&mut stream).unwrap().unwrap().span, (2, 7));
    assert_eq!(next_token(&mut stream).unwrap().unwrap().span, (8, 9));
}