use fallible_iterator::FallibleIterator;

use super::chars::{Chars, LexerIterator};

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
use super::number::NumberToken;

/// A Scheme token
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

#[derive(Debug, Clone)]
pub struct TokenizerError {
    error: TokenErrorClass,
}

/// The possible errors while tokenizing the input
#[derive(Debug, Clone)]
pub enum TokenErrorClass {
    InvalidPound,
    InvalidCharName,
    UnclosedString,
    InvalidDot,
    UnfinishedChar,
    UnexpectedCharacter,
    InvalidScaping,
    UnfinishedNumber,
    BadRadix,
    BadCartesian,
    BadDelimiter,
    BadFraction,
    BadSuffix,
    BadDigit,
    BadMarker,
    BadDecimal,
    EmptyNumber,
}

/// A stream of tokens
pub struct Tokens<I: Iterator<Item = char>> {
    source: Chars<I>,
}

impl<I: Iterator<Item = char>> FallibleIterator for Tokens<I> {
    type Item = Token;
    type Error = TokenizerError;

    fn next(&mut self) -> Result<Option<Token>, TokenizerError> {
        next_token(&mut self.source)
    }
}

impl<I: Iterator<Item = char>> Tokens<I> {
    pub fn new(source: I) -> Tokens<I> {
        Tokens {
            source: source.into(),
        }
    }
}

#[derive(PartialEq, Debug)]
enum ParsingState {
    Normal,
    Identifier,
    Comment,
    Character(char),
    String,
}

const CHAR_NAME_SPACE: &[char] = &['s', 'p', 'a', 'c', 'e'];
const CHAR_NAME_NEWLINE: &[char] = &['n', 'e', 'w', 'l', 'i', 'n', 'e'];

pub fn next_token<T: LexerIterator>(stream: &mut T) -> Result<Option<Token>, TokenizerError> {
    let mut state = ParsingState::Normal;
    let mut string_buf = String::new();

    loop {
        let peek3 = stream.peek(2);
        // Numbers are handled by peeking to delegate properly to parse_number
        if state == ParsingState::Normal {
            match (stream.peek(0), stream.peek(1)) {
                (None, _) => return Ok(None),

                // Pound
                (Some('#'), Some('t')) if is_delimiter!(peek3) => {
                    ret_val!(Token::Boolean(true), stream, 2)
                }
                (Some('#'), Some('f')) if is_delimiter!(peek3) => {
                    ret_val!(Token::Boolean(false), stream, 2)
                }
                (Some('#'), Some('(')) => ret_val!(Token::OpenVector, stream, 2),
                (Some('#'), Some('\\')) => { /*char, delegate to main loop*/ }

                (Some('#'), Some('e'))
                | (Some('#'), Some('i'))
                | (Some('#'), Some('b'))
                | (Some('#'), Some('o'))
                | (Some('#'), Some('d'))
                | (Some('#'), Some('x'))
                | (Some('0'...'9'), _)
                | (Some('+'), Some('0'...'9'))
                | (Some('-'), Some('0'...'9'))
                | (Some('.'), Some('0'...'9')) => match NumberToken::parse(stream) {
                    Ok(nt) => ret_val!(Token::Number(nt)),
                    Err(error) => {
                        return Err(TokenizerError { error });
                    }
                },

                (Some('#'), _) => ret_err!(InvalidPound),

                _ => { /*delegate to main loop*/ }
            }
        }

        let c = match stream.next() {
            Some(d) => d,

            None => match state {
                ParsingState::Character(_) => ret_err!(InvalidCharName),
                ParsingState::String => ret_err!(UnclosedString),
                _ => return Ok(None),
            },
        };

        match state {
            ParsingState::Normal => {
                if c.is_whitespace() {
                    continue;
                }

                let peek3 = stream.peek(2);
                match c {
                    '(' => ret_val!(Token::Open),
                    ')' => ret_val!(Token::Close),
                    '`' => ret_val!(Token::BackQuote),
                    '\'' => ret_val!(Token::SingleQuote),

                    // Numbers are already handled
                    '.' => match (stream.peek(0), stream.peek(1)) {
                        (Some('.'), Some('.')) if is_delimiter!(peek3) => {
                            stream.advance(2);
                            ret_val!(Token::Identifier("...".to_string()));
                        }
                        (x, _) if is_delimiter!(x) => ret_val!(Token::Dot),
                        _ => ret_err!(InvalidDot),
                    },
                    ';' => {
                        state = ParsingState::Comment;
                    }
                    '"' => {
                        string_buf.clear();
                        state = ParsingState::String;
                        stream.case_sensitive(true);
                    }

                    // It's '#\' for sure
                    '#' => {
                        stream.next();
                        let next = stream.peek_sensitive(0);
                        let peek = stream.peek_sensitive(1);
                        match next.map(|c| c.to_ascii_lowercase()) {
                            None => ret_err!(UnfinishedChar),
                            Some('s') | Some('n') if !is_delimiter!(peek) => {
                                state = ParsingState::Character(next.unwrap());
                            }
                            Some(_) if is_delimiter!(peek) => {
                                stream.next();
                                ret_val!(Token::Character(next.unwrap()))
                            }
                            _ => ret_err!(InvalidCharName),
                        }
                    }

                    // It's an identifier
                    '+' | '-' => if is_delimiter!(stream.peek(0)) {
                        ret_val!(Token::Identifier(c.to_string()))
                    } else {
                        ret_err!(UnexpectedCharacter)
                    },
                    ',' => match stream.peek(0) {
                        Some('@') => {
                            stream.next();
                            ret_val!(Token::CommaAt)
                        }
                        _ => ret_val!(Token::Comma),
                    },
                    d if is_initial(d) => match stream.peek(0) {
                        Some(e) if is_subsequent(e) => {
                            string_buf.clear();
                            string_buf.push(d);
                            state = ParsingState::Identifier;
                        }
                        _ => ret_val!(Token::Identifier(d.to_string())),
                    },
                    _ => ret_err!(UnexpectedCharacter),
                }
            }
            ParsingState::Comment => {
                if c == '\n' {
                    state = ParsingState::Normal;
                }
                continue;
            }
            ParsingState::Character(d) => {
                let char_name = if d == 's' {
                    CHAR_NAME_SPACE
                } else {
                    CHAR_NAME_NEWLINE
                };

                let l = char_name.len() - 1;
                let mut i = 0;

                loop {
                    let c = stream.peek(i);
                    if i == l && is_delimiter!(c) {
                        stream.advance(l);
                        ret_val!(Token::Character(if char_name == CHAR_NAME_SPACE {
                            ' '
                        } else {
                            '\n'
                        }));
                    } else if i != l && c == Some(char_name[i + 1]) {
                        i += 1;
                        continue;
                    } else {
                        ret_err!(InvalidCharName);
                    }
                }
            }
            ParsingState::String => match c {
                '"' => ret_val!(Token::String(string_buf.clone())),
                '\\' => match stream.next() {
                    Some('"') => {
                        string_buf.push('"');
                    }
                    Some('\\') => string_buf.push('\\'),
                    _ => ret_err!(InvalidScaping),
                },
                _ => string_buf.push(c),
            },
            ParsingState::Identifier => {
                string_buf.push(c);

                match stream.peek(0) {
                    Some(d) if is_subsequent(d) => {}
                    _ => ret_val!(Token::Identifier(string_buf.clone())),
                }
            }
        }
    }
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
        _ => false,
    }
}

#[inline]
fn is_special_initial(c: char) -> bool {
    match c {
        '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~' => true,
        _ => false,
    }
}

#[inline]
fn is_special_subsequent(c: char) -> bool {
    match c {
        '+' | '-' | '.' | '@' => true,
        _ => false,
    }
}
