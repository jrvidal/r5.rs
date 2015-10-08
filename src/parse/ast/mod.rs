
use super::token::Token;


pub enum Expression {
    Variable(String),
    // Literal
    Call {
        operator: Box<Expression>,
        operands: Vec<Expression>
    },
    Lambda {
        formals: Vec<String>,
        rest: Option<String>,
        // Body...
    },
    Conditional {
        test: Box<Expression>,
        consequent: Box<Expression>,
        alternate: Option<Box<Expression>>,
    },
    Assignment {
        variable: String,
        expression: Box<Expression>,
    },
    Derived,
    MacroUse,
    MacroBlock,
}

macro_rules! one_of {
    ($x:expr, [$c:expr]) => ($x == $c);
    ($x:expr, [ $c:expr, $( $d:expr ),* ]) => (
        $x == $c || one_of!($x, [$( $d ),* ])
    )
}

macro_rules! ret_val {
    ($x:expr) => ({
        return Some(Ok($x));
    })
}

mod keywords {
    pub const IF : &'static str = "if";
    pub const OR : &'static str = "or";
    pub const ARROW : &'static str = "=>";
    pub const DO : &'static str = "do";
    pub const AND : &'static str = "and";
    pub const LET : &'static str = "let";
    pub const ELSE : &'static str = "else";
    pub const SET_BANG : &'static str = "set!";
    pub const COND : &'static str = "cond";
    pub const CASE : &'static str = "case";
    pub const LET_STAR : &'static str = "let*";
    pub const QUOTE : &'static str = "quote";
    pub const BEGIN : &'static str = "begin";
    pub const DELAY : &'static str = "delay";
    pub const DEFINE : &'static str = "define";
    pub const LAMBDA : &'static str = "lambda";
    pub const LETREC : &'static str = "letrec";
    pub const UNQUOTE : &'static str = "unquote";
    pub const QUASIQUOTE : &'static str = "quasiquote";
    pub const UNQUOTE_SPLICING : &'static str = "unquote-splicing";
}

fn is_syntactic_keyword(name: &str) -> bool {
    match name.len() {
        2 => one_of!(name, [keywords::IF, keywords::OR, keywords::ARROW, keywords::DO]),
        3 => one_of!(name, [keywords::AND, keywords::LET]),
        4 => one_of!(name, [keywords::ELSE, keywords::SET_BANG, keywords::COND, keywords::CASE, keywords::LET_STAR]),
        5 => one_of!(name, [keywords::QUOTE, keywords::BEGIN, keywords::DELAY]),
        6 => one_of!(name, [keywords::DEFINE, keywords::LAMBDA, keywords::LETREC]),
        7 => one_of!(name, [keywords::UNQUOTE]),
        10 => one_of!(name, [keywords::QUASIQUOTE]),
        16 => one_of!(name, [keywords::UNQUOTE_SPLICING]),
        _ => false
    }
}

struct Tokens {
    vec: Vec<Token>,
    index: usize,
}

fn parse_expression(stream: &mut Tokens) -> Result<Option<Expression>, ()> {
    unimplemented!()
    // loop {
    //     let t = match stream.next() {
    //         None => return None,
    //         Some(Err(_)) => return Some(Err(())),
    //         Some(Ok(x)) => x
    //     };


    //     match t {
    //         Token::Identifier(ident) => if !is_syntactic_keyword(&ident) {
    //             ret_val!(Expression::Variable(ident));
    //         },
    //         _ => {}
    //     }
    // }

    // panic!()
}