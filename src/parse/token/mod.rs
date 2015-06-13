

use std::ascii::AsciiExt;
use std::iter;
pub use super::peekable::Peekable;

macro_rules! ret_val {
    ($x:expr) => (return Ok(Some($x)));
    ($x:expr, $s:ident, $n:expr) => ({
        $s.advance($n);
        return Ok(Some($x))
    });
}

macro_rules! delegate_parse_number {
    ($s:expr) => (
        return parse_number($s).map(|x| {
            Some(Token::Number(x))
        })
    )
}

macro_rules! is_whitespace {
    ($x:expr) => ($x == ' ' || $x == '\n')
}

macro_rules! is_delimiter {
    ($x:expr) => (
        $x.is_none() || {
            let y = $x.unwrap();
            y == '(' || (y == ')' || ( y == '"' || ( y == ';' || is_whitespace!(y))))
        }
    )
}

macro_rules! is_digit {
    ($c:expr) => (
        match $c {
            '0'...'9' => true,
            _ => false
        }
    )
}

mod number;
use self::number::{parse_number, NumberToken};

#[cfg(test)]
mod test;

/**
    Tokenizer

    The R5RS specs states:

    Tokens which require implicit termination (identifiers,
    numbers, characters, and dot) may be terminated by any
    delimiter, but not necessarily by anything else

    We're going to interpret it as:

    Tokens which require implicit termination (identifiers,
    numbers, characters, and dot) must be terminated by a
    delimiter.
*/

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Identifier(String),
    Boolean(bool),
    Number(NumberToken),
    Character(char),
    String(String),
    OpenVector,
    Comma,
    CommaAt,

    Open,
    Close,
    BackQuote,
    SingleQuote,
    Dot,
}

#[derive(PartialEq, Debug)]
enum ParsingState {
    Normal,
    Identifier,
    Comment,
    Character(char),
    String,
}

const CHAR_NAME_SPACE : &'static [char] = &['s', 'p', 'a', 'c', 'e'];
const CHAR_NAME_NEWLINE : &'static [char] = &['n', 'e', 'w', 'l', 'i', 'n', 'e'];

#[allow(dead_code)]
pub fn next_token<T: Peekable>(mut stream: T) -> Result<Option<Token>, String> 
    where T: Iterator<Item=char>
{
    let mut state = ParsingState::Normal;
    let mut string_buf = String::new();

    loop {

        // Numbers are handled by peeking to delegate properly to parse_number
        if state == ParsingState::Normal {
            let peek = stream.small_peek();
            match (peek[0], peek[1].map(lc)) {
                (None, _) => return Ok(None),

                // Pound
                (Some('#'), Some('t')) if is_delimiter!(peek[2]) => ret_val!(Token::Boolean(true), stream, 2),
                (Some('#'), Some('f')) if is_delimiter!(peek[2]) => ret_val!(Token::Boolean(false), stream, 2),
                (Some('#'), Some('(')) => ret_val!(Token::OpenVector, stream, 2),
                (Some('#'), Some('\\')) => {/*delegate to main loop*/},

                (Some('#'), Some('e')) |
                (Some('#'), Some('i')) |
                (Some('#'), Some('b')) |
                (Some('#'), Some('o')) |
                (Some('#'), Some('d')) |
                (Some('#'), Some('x')) |
                (Some('0'...'9'), _) |
                (Some('+'), Some('0'...'9')) |
                (Some('-'), Some('0'...'9')) |
                (Some('.'), Some('0'...'9'))
                    => delegate_parse_number!(&mut stream),

                (Some('#'), _) => return Err("no puedes".to_string()),

                _ => {/*delegate to main loop*/}
            }
        }

        let c = match stream.next() {
            Some(d) => d,

            None => match state {
                ParsingState::Character(_) => {
                    return Err("no hay siguiente".to_string())
                },
                ParsingState::String => {
                    return Err("no hay siguiente".to_string())
                },
                _ => return Ok(None)
            }
        };

        let peek = stream.small_peek();

        match state {
            ParsingState::Normal => {
                if c.is_whitespace() {
                    continue;
                }

                match c.to_ascii_lowercase() {
                    '(' => ret_val!(Token::Open),
                    ')' => ret_val!(Token::Close),
                    '`' => ret_val!(Token::BackQuote),
                    '\'' => ret_val!(Token::SingleQuote),

                    // Numbers are already handled
                    '.' => {
                        match (peek[0], peek[1]) {
                            (Some('.'), Some('.')) if is_delimiter!(peek[2]) => {
                                stream.advance(2);
                                ret_val!(Token::Identifier("...".to_string()));
                            },
                            (x, _) if is_delimiter!(x) => ret_val!(Token::Dot),
                            _ => return Err("bad dot".to_string())
                        }
                    },
                    ';' => {
                        state = ParsingState::Comment;
                    },
                    '"' => {
                        string_buf.clear();
                        state = ParsingState::String;
                    },

                    // It's '#\' for sure
                    '#' => {
                        stream.next();
                        let next = stream.small_peek();
                        match next[0].map(lc) {
                            None => return Err("bad character".to_string()),
                            Some('s') | Some('n') if !is_delimiter!(next[1]) => {
                                state = ParsingState::Character(next[0].map(lc).unwrap());
                            },
                            Some(_) if is_delimiter!(next[1]) => ret_val!(Token::Character(next[0].unwrap())),
                            _ => return Err("bad character!".to_string())
                        }
                    },

                    // It's an identifier
                    '+' | '-' if is_delimiter!(peek[0]) => ret_val!(Token::Identifier(c.to_string())),
                    ',' => {
                        match peek[0] {
                            Some('@') => {
                                stream.next();
                                ret_val!(Token::CommaAt)
                            },
                            _ => ret_val!(Token::Comma)
                        }
                    },
                    d if is_initial(d) => {
                        match stream.small_peek()[0].map(lc) {
                            Some(e) if is_subsequent(e) => {
                                string_buf.clear();
                                string_buf.push(d);
                                state = ParsingState::Identifier;
                            },
                            _ => ret_val!(Token::Identifier(d.to_string()))
                        }
                    },
                    _ => return Err("no puedes".to_string())
                }
            },
            ParsingState::Comment => {
                if c == '\n' {
                    state = ParsingState::Normal;
                }
                continue;
            },
            ParsingState::Character(d) => {
                let char_name = if d.to_ascii_lowercase() == 's' {
                    CHAR_NAME_SPACE
                } else {
                    CHAR_NAME_NEWLINE
                };

                let big_peek = stream.peek().map(lc);
                let l = char_name.len() - 1;

                for (i, x) in big_peek.map(|c| Some(c))
                        .chain(iter::repeat(None))
                        .enumerate() {

                    match (i, x) {
                        (_, Some(c)) if i < l && c == char_name[i + 1] => {},
                        _ if i == l && is_delimiter!(x)  => {
                            stream.advance(l);
                            ret_val!(Token::Character(if char_name == CHAR_NAME_SPACE {
                                ' '
                            } else {
                                '\n'
                            }));
                        },
                        _ => return Err("bad character name".to_string())
                    }

                }
            },
            ParsingState::String => {
                match c {
                    '"' => ret_val!(Token::String(string_buf.clone())),
                    '\\' => {
                        match stream.next() {
                            Some('"') => {
                                string_buf.push('"');
                            },
                            Some('\\') => {
                                string_buf.push('\\')
                            }
                            _ => return Err("bad escaping".to_string()),
                        }
                    },
                    _ => string_buf.push(c)
                }
            },
            ParsingState::Identifier => {
                string_buf.push(c.to_ascii_lowercase());

                match peek[0].map(lc) {
                    Some (d) if is_subsequent(d) => {},
                    _ => ret_val!(Token::Identifier(string_buf.clone()))
                }

            }
        }

    }
}

#[inline]
fn lc(c: char) -> char {
    c.to_ascii_lowercase()
}

#[inline]
fn is_initial(c: char) -> bool {
    is_letter(c) || is_special_initial(c)
}

#[inline]
fn is_subsequent(c: char) -> bool {
    is_initial(c) || is_digit!(c) || is_special_subsequent(c)
}

#[inline]
fn is_letter(c: char) -> bool {
    match c {
        'a'...'z' => true,
        _ => false
    }
}

#[inline]
fn is_special_initial(c: char) -> bool {
    match c {
        '!' | '$' | '%' | '&' | '*' |
        '/' | ':' | '<' | '=' | '>' |
        '?' | '^' | '_' | '~' => true,
        _ => false
    }
}

#[inline]
fn is_special_subsequent(c: char) -> bool {
    match c {
        '+' | '-' | '.' | '@' => true,
        _ => false
    }
}
