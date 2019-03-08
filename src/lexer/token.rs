use std::error::Error;
use std::fmt;

use super::chars::{LexerIterator};

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

#[cfg(test)]
#[path = "token_test.rs"]
mod token_test;

/// A Scheme token
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub ty: TokenType,
    span: (usize, usize)
}

impl Token {
    pub fn fake(ty: TokenType) -> Token {
        Token { ty, span: (0, 0) }
    }
}


/// The type of a Scheme token
#[derive(Clone, PartialEq, Debug)]
pub enum TokenType {
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

#[derive(Debug, Clone, PartialEq)]
pub struct TokenizerError {
    error: TokenErrorClass,
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(&format!("{:?}", self.error))
    }
}

impl Error for TokenizerError {
    fn description(&self) -> &str {
        "Tokenizer error"
    }
}

/// The possible errors while tokenizing the input
#[derive(Debug, Clone, Copy, PartialEq)]
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


#[derive(PartialEq, Debug)]
enum ParsingState {
    Normal,
    Identifier(usize),
    Comment,
    Character(char, usize),
    String(usize),
}

const CHAR_NAME_SPACE: &[char] = &['s', 'p', 'a', 'c', 'e'];
const CHAR_NAME_NEWLINE: &[char] = &['n', 'e', 'w', 'l', 'i', 'n', 'e'];

macro_rules! tok_ret {
    (internal; $value:expr; $lo:expr; $hi:expr) => ({
        return Ok(Some(Token { ty: $value, span: ($lo, $hi)  }))
    });
    ($value:expr; advance $stream:ident; $amount:expr) => ({
        let source_char = $stream.next().unwrap();
        if $amount > 1 {
            $stream.advance($amount - 1);
        }
        tok_ret!(internal; $value; source_char.pos; source_char.pos + $amount)
    });

    ($value:expr; $lo:expr => $hi:expr) => ({
        tok_ret!(internal; $value; $lo; $hi)
    });

    ($value:expr, on $lo:expr) => ({
        tok_ret!(internal; $value; $lo; $lo + 1)
    });

    // ($value:expr) => ({
    //     return Ok(Some(Token { ty: $value, span: (0, 0) }))
    // });
    // ($value:expr, $stream:ident, $amount:expr) => ({
    //     $stream.advance($amount);
    //     return Ok(Some(Token { ty: $value, span: (0, 0) }))
    // })
}
pub fn next_token<T: LexerIterator>(stream: &mut T) -> Result<Option<Token>, TokenizerError> {
    let mut state = ParsingState::Normal;
    let mut string_buf = String::new();

    loop {
        let starting_pos = stream.pos();

        let peek3 = stream.peek(2);
        // Numbers are handled by peeking to delegate properly to parse_number
        if state == ParsingState::Normal {
            match (stream.peek(0), stream.peek(1)) {
                (None, _) => return Ok(None),

                // Pound
                (Some('#'), Some('t')) if is_delimiter!(peek3) => {
                    tok_ret!(TokenType::Boolean(true); advance stream; 2)
                }
                (Some('#'), Some('f')) if is_delimiter!(peek3) => {
                    tok_ret!(TokenType::Boolean(false); advance stream; 2)
                }
                (Some('#'), Some('(')) => tok_ret!(TokenType::OpenVector; advance stream; 2),
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
                    Ok(nt) => tok_ret!(TokenType::Number(nt); starting_pos => stream.pos()),
                    Err(error) => {
                        return Err(TokenizerError { error });
                    }
                },

                (Some('#'), _) => ret_err!(InvalidPound),

                _ => { /*delegate to main loop*/ }
            }
        }

        let sch = match stream.next() {
            Some(d) => d,

            None => match state {
                ParsingState::Character(..) => ret_err!(InvalidCharName),
                ParsingState::String(_) => ret_err!(UnclosedString),
                _ => return Ok(None),
            },
        };

        let c = sch.c;

        match state {
            ParsingState::Normal => {
                if c.is_whitespace() {
                    continue;
                }

                let peek3 = stream.peek(2);
                match c {
                    '(' => tok_ret!(TokenType::Open, on sch.pos),
                    ')' => tok_ret!(TokenType::Close, on sch.pos),
                    '`' => tok_ret!(TokenType::BackQuote, on sch.pos),
                    '\'' => tok_ret!(TokenType::SingleQuote, on sch.pos),

                    // Numbers are already handled
                    '.' => match (stream.peek(0), stream.peek(1)) {
                        (Some('.'), Some('.')) if is_delimiter!(peek3) => {
                            stream.advance(2);
                            tok_ret!(TokenType::Identifier("...".to_string()); sch.pos => sch.pos + 3);
                        }
                        (x, _) if is_delimiter!(x) => tok_ret!(TokenType::Dot, on sch.pos),
                        _ => ret_err!(InvalidDot),
                    },
                    ';' => {
                        state = ParsingState::Comment;
                    }
                    '"' => {
                        string_buf.clear();
                        state = ParsingState::String(sch.pos);
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
                                state = ParsingState::Character(next.unwrap(), sch.pos);
                            }
                            Some(_) if is_delimiter!(peek) => {
                                stream.next();
                                tok_ret!(TokenType::Character(next.unwrap()); sch.pos => sch.pos + 3)
                            }
                            _ => ret_err!(InvalidCharName),
                        }
                    }

                    // It's an identifier
                    '+' | '-' => if is_delimiter!(stream.peek(0)) {
                        tok_ret!(TokenType::Identifier(c.to_string()), on sch.pos)
                    } else {
                        ret_err!(UnexpectedCharacter)
                    },
                    ',' => match stream.peek(0) {
                        Some('@') => {
                            stream.next();
                            tok_ret!(TokenType::CommaAt; sch.pos => sch.pos + 2)
                        }
                        _ => tok_ret!(TokenType::Comma, on sch.pos),
                    },
                    d if is_initial(d) => match stream.peek(0) {
                        Some(e) if is_subsequent(e) => {
                            string_buf.clear();
                            string_buf.push(d);
                            state = ParsingState::Identifier(sch.pos);
                        }
                        _ => tok_ret!(TokenType::Identifier(d.to_string()), on sch.pos),
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
            ParsingState::Character(d, character_starting_pos) => {
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
                        let character = if char_name == CHAR_NAME_SPACE {
                            ' '
                        } else {
                            '\n'
                        };
                        tok_ret!(TokenType::Character(character); character_starting_pos => character_starting_pos + char_name.len() + 2);
                    } else if i != l && c == Some(char_name[i + 1]) {
                        i += 1;
                        continue;
                    } else {
                        ret_err!(InvalidCharName);
                    }
                }
            }
            ParsingState::String(string_starting_pos) => match c {
                '"' => tok_ret!(TokenType::String(string_buf.clone()); string_starting_pos => sch.pos + 1),
                '\\' => match stream.next().map(|sch| sch.c) {
                    Some('"') => {
                        string_buf.push('"');
                    }
                    Some('\\') => string_buf.push('\\'),
                    _ => ret_err!(InvalidScaping),
                },
                _ => string_buf.push(c),
            },
            ParsingState::Identifier(identifier_starting_pos) => {
                string_buf.push(c);

                match stream.peek(0) {
                    Some(d) if is_subsequent(d) => {}
                    _ => tok_ret!(TokenType::Identifier(string_buf.clone()); identifier_starting_pos => sch.pos + 1),
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
