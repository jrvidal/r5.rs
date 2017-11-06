
macro_rules! ret_err {
    ($err:ident) => ({
        return Err(TokenizerError {error: TokenErrorClass::$err})
    })
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
