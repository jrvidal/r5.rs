use std::collections::VecDeque;
use std::rc::Rc;

use reader::{AbbreviationKind, Datum};
use self::keywords::is_syntactic_keyword;
use helpers::*;

#[derive(Debug, Clone)]
pub enum Instruction {
    // *** Push simple scalars
    Integer(isize),
    String(CowString),
    Character(char),
    Boolean(bool),
    Symbol(ImmutableString),
    Vector(usize),
    Number,
    Nil,
    EmptyList,
    // ***
    // Call(is_tail), save return environment if it's not tail
    Call(bool),
    // Return, restore return environment
    Ret,
    // Check arity, load rest arguments
    Arity(usize, bool),
    // Branch +n unconditionally
    Branch(usize),
    // Branch +n if stack pop is truthy
    BranchIf(usize),
    // Branch +n if stack pop is falsy
    BranchUnless(usize),
    // Non-popping versions
    ROBranchIf(usize),
    ROBranchUnless(usize),
    // Push compiled lambda
    Lambda(Rc<Vec<Instruction>>),
    // Make pair from 2 pops and push
    Pair,
    // Make list from n pops and push. Bool indicates improper list (+2 pops)
    List(usize, bool),
    // ...
    Pop,
    // Load variable from environment
    LoadVar(ImmutableString),
    // Define variable in environment
    DefineVar(ImmutableString),
    // Set variable in environment (up to root)
    SetVar(ImmutableString),
    NewEnv,
    PopEnv,
    And,
    Or,
}

mod keywords;

// #[cfg(test)]
// #[path = "expression_test.rs"]
// mod test;

// #[derive(Debug, Clone, PartialEq)]
// pub enum Expression {
//     // max(String, bool, char, NumberToken, Datum, Vec)
//     Variable(String),

//     Boolean(bool),
//     Character(char),
//     String(String),
//     // TO DO: are we going to store the real number or this?
//     Number(NumberToken),
//     Quotation(Datum),
//     QuasiQuotation(Datum),

//     // Nope
//     // http://stackoverflow.com/questions/18641757/unquoted-vectors-in-r5rs-scheme
//     // Vector(Vec<Expression>),
//     Call {
//         operator: Box<Expression>,
//         operands: Vec<Expression>,
//     },
//     Lambda { formals: LambdaFormals, body: Body },
//     Conditional {
//         test: Box<Expression>,
//         consequent: Box<Expression>,
//         alternate: Option<Box<Expression>>,
//     },
//     Assignment {
//         variable: String,
//         expression: Box<Expression>,
//     },
//     Derived(Derived),
//     MacroUse {
//         keyword: String,
//         datum: VecDeque<Datum>,
//     },
//     // TO DO: macro block
//     MacroBlock,
// }

// #[derive(Debug, PartialEq, Clone)]
// pub enum Derived {
//     Cond {
//         head_clause: CondClause,
//         tail_clauses: Vec<CondClause>,
//     },
//     CondElse {
//         clauses: Vec<CondClause>,
//         else_commands: Vec<Expression>,
//         else_expression: Box<Expression>,
//     },
//     Case {
//         key: Box<Expression>,
//         head_clause: CaseClause,
//         tail_clauses: Vec<CaseClause>,
//     },
//     CaseElse {
//         key: Box<Expression>,
//         clauses: Vec<CaseClause>,
//         else_commands: Vec<Expression>,
//         else_expression: Box<Expression>,
//     },
//     And(Vec<Expression>),
//     Or(Vec<Expression>),
//     Let { bindings: Vec<Binding>, body: Body },
//     NamedLet {
//         variable: String,
//         bindings: Vec<Binding>,
//         body: Body,
//     },
//     LetStar { bindings: Vec<Binding>, body: Body },
//     LetRec { bindings: Vec<Binding>, body: Body },
//     Begin {
//         commands: Vec<Expression>,
//         expression: Box<Expression>,
//     },
//     Do {
//         iterations: Vec<IterationSpec>,
//         test: Box<Expression>,
//         result: Vec<Expression>,
//         commands: Vec<Expression>,
//     },
//     Delay(Box<Expression>),

//     // Quasiquotation is at the top level
// }

// #[derive(Debug, PartialEq, Clone)]
// pub struct IterationSpec {
//     variable: String,
//     init: Box<Expression>,
//     step: Option<Box<Expression>>,
// }

// #[derive(Debug, PartialEq, Clone)]
// pub struct Binding {
//     pub variable: String,
//     pub init: Box<Expression>,
// }

// #[derive(Debug, PartialEq, Clone)]
// pub enum Definition {
//     Define {
//         variable: String,
//         expression: Box<Expression>,
//     },
//     DefineLambda {
//         variable: String,
//         formals: LambdaFormals,
//         body: Body,
//     },
//     Begin(Vec<Definition>),
// }

// #[derive(Debug, PartialEq, Clone)]
// pub struct Body {
//     pub definitions: Vec<Definition>,
//     pub commands: Vec<Expression>,
//     pub expression: Box<Expression>,
// }

// #[derive(Debug, PartialEq, Clone)]
// pub enum CondClause {
//     Normal {
//         test: Box<Expression>,
//         expressions: Vec<Expression>,
//     },
//     Arrow {
//         test: Box<Expression>,
//         recipient: Box<Expression>,
//     },
// }

// #[derive(Debug, PartialEq, Clone)]
// pub struct CaseClause {
//     datums: VecDeque<Datum>,
//     commands: Vec<Expression>,
//     expression: Box<Expression>,
// }

#[derive(Debug, Clone, PartialEq)]
pub enum LambdaFormals {
    VarArgs(String),
    List(Vec<String>),
    // For rest params, vec.len() *must* be >= 1
    Rest(Vec<String>, String),
}

#[derive(Debug, Eq, PartialEq)]
pub enum ParsingError {
    Illegal,
}

macro_rules! check {
    ($check:expr, $err:expr) => (
        if !$check {
            return Err($err);
        }
    )
}

#[derive(Debug)]
enum LetExp {
    Let,
    LetRec,
    LetStar,
    NamedLet,
}

// macro_rules! let_expr {
//     ($type:ident, $details:expr) => (
//         {

//             let (bindings, body) = $details?;
//             Ok(Expression::Derived(Derived::$type { bindings, body }))
//         }
//     )
// }

pub fn compile_expression(d: Datum) -> Option<Vec<Instruction>> {
    parse_expression_inner(d).ok()
}

fn parse_expression_inner(d: Datum) -> Result<Vec<Instruction>, ParsingError> {
    macro_rules! simple_datum {
        ($T:ident, $c:expr) => (
            Ok(vec![Instruction::$T($c)])
        )
    }
    // let matcher = {
    //     let st = symbol_type(d);
    //     (st.0, st.1, symbolic)
    // };
    // Simple cases
    let mut datums = match (symbol_type(&d), d) {
        (Symbol::Variable, Datum::Symbol(s)) => return simple_datum!(LoadVar, s.into()),
        // TO DO: why doesn't this work??
        // Datum::Symbol(s) if keywords::is_syntactic_keyword(&s) => {
        //         ret_val!(Expression::Variable(s))
        // },
        (_, Datum::Boolean(b)) => return simple_datum![Boolean, b],
        (_, Datum::Character(c)) => return simple_datum![Character, c],
        (_, Datum::String(s)) => return simple_datum![String, s.into()],
        (_, Datum::Number(_)) => return Ok(vec![Instruction::Number]),
        (
            _,
            Datum::Abbreviation {
                kind: AbbreviationKind::Quote,
                datum,
            },
        ) => return compile_quotation(*datum),

        (
            _,
            Datum::Abbreviation {
                kind: AbbreviationKind::Quasiquote,
                datum,
            },
        ) => return parse_quasiquotation(*datum),
        (_, Datum::Vector(datums)) => {
            let mut instructions = vec![];
            let n = datums.len();

            for d in datums {
                instructions.extend(compile_quotation(d)?);
            }
            instructions.push(Instruction::Vector(n));
            return Ok(instructions);
        }

        // Delegate
        (_, Datum::List(datums)) => datums,
        _ => return Err(ParsingError::Illegal),
    };

    check![datums.len() > 0, ParsingError::Illegal];

    let head = datums.pop_front().unwrap();
    let symbol = &match keyword_name(head.clone()) {
        Some(s) => s,
        _ => return parse_call_exp(head, datums),
    };

    match (&symbol[..], datums.len()) {
        // // Verbose quotations
        (keywords::QUOTE, 1) => compile_quotation(datums.pop_front().unwrap()),
        // // Verbose quasiquotations
        // (keywords::QUASIQUOTE, 1) => {
        //     Ok(datums.pop_front().map(Expression::QuasiQuotation).unwrap())
        // }
        // // If
        (keywords::IF, 2) | (keywords::IF, 3) => {
            let mut exprs: Vec<Vec<_>> = datums
                .into_iter()
                .map(|d| parse_expression_inner(d))
                .collect::<Result<_, _>>()?;
            let (test, consequent, alternate) = {
                let last = exprs.pop();
                let next_to_last = exprs.pop().unwrap();
                if exprs.len() == 0 {
                    (next_to_last, last.unwrap(), None)
                } else {
                    (exprs.pop().unwrap(), next_to_last, last)
                }
            };

            let test_offset = consequent.len();
            let consequent_offset = alternate
                .as_ref()
                .map(|alternate| alternate.len())
                .unwrap_or(0);

            let mut instructions = test;
            instructions.push(Instruction::BranchUnless(test_offset + 2));
            instructions.extend(consequent);
            instructions.push(Instruction::Branch(consequent_offset + 1));

            instructions.extend(alternate.unwrap_or_else(|| vec![Instruction::Nil]));

            Ok(instructions)
        }
        // Assignment
        (keywords::SET_BANG, 2) => {
            let d = datums.pop_front().unwrap();
            let var = symbol_type(&d);
            let parsed = parse_expression_inner(datums.pop_front().unwrap());
            match (d, var, parsed) {
                (Datum::Symbol(s), Symbol::Variable, Ok(mut instructions)) => {
                    instructions.push(Instruction::SetVar(s.into()));
                    Ok(instructions)
                }
                _ => Err(ParsingError::Illegal),
            }
        }

        // Lambda
        (keywords::LAMBDA, l) if l >= 2 => {
            let formals = parse_lambda_formals_exp(datums.pop_front().unwrap())?;
            return parse_lambda_exp(formals, datums);
        }

        // (keywords::COND, l) if l >= 1 => {
        //     let else_expressions = parse_else_clause(&mut datums)?;

        //     let mut clauses = datums
        //         .into_iter()
        //         .map(parse_cond_clause_exp)
        //         .collect::<Result<Vec<CondClause>, ParsingError>>()?;

        //     let derived = match else_expressions {
        //         Some((else_commands, else_expression)) => Derived::CondElse {
        //             clauses,
        //             else_commands,
        //             else_expression,
        //         },
        //         None => {
        //             let head_clause = clauses.remove(0);
        //             Derived::Cond {
        //                 head_clause,
        //                 tail_clauses: clauses,
        //             }
        //         }
        //     };

        //     Ok(Expression::Derived(derived))
        // }
        // (keywords::CASE, l) if l >= 2 => {
        //     let key = Box::new(parse_expression(datums.pop_front().unwrap())?);

        //     let else_expressions = parse_else_clause(&mut datums)?;
        //     let mut clauses = datums
        //         .into_iter()
        //         .map(parse_case_clause_exp)
        //         .collect::<Result<_, _>>()?;

        //     let derived = match else_expressions {
        //         Some((commands, expr)) => Derived::CaseElse {
        //             key: key,
        //             clauses: clauses,
        //             else_commands: commands,
        //             else_expression: expr,
        //         },
        //         None => {
        //             let head = clauses.remove(0);
        //             Derived::Case {
        //                 key: key,
        //                 head_clause: head,
        //                 tail_clauses: clauses,
        //             }
        //         }

        //     };

        //     Ok(Expression::Derived(derived))
        // }
        (keywords::LET, l) if l >= 2 => match symbol_type(&datums[0]) {
            Symbol::Variable => parse_let_exp(datums, LetExp::NamedLet),
            _ => parse_let_exp(datums, LetExp::Let),
        },
        (keywords::LETREC, l) if l >= 2 => parse_let_exp(datums, LetExp::LetRec),
        (keywords::LET_STAR, l) if l >= 2 => parse_let_exp(datums, LetExp::LetStar),
        // (keywords::DO, l) if l >= 2 => {
        //     let iterations = datums
        //         .pop_front()
        //         .unwrap()
        //         .list()
        //         .ok_or(ParsingError::Illegal)?
        //         .into_iter()
        //         .map(parse_iteration_spec)
        //         .collect::<Result<_, _>>()?;

        //     let mut list = datums
        //         .pop_front()
        //         .unwrap()
        //         .list()
        //         .ok_or(ParsingError::Illegal)?;

        //     check![list.len() > 0, ParsingError::Illegal];

        //     let test = list.pop_front()
        //         .map(parse_expression)
        //         .unwrap()
        //         .map(Box::new)?;

        //     let result = list.into_expressions()?;
        //     let commands = datums.into_expressions()?;
        //     let derived = Derived::Do {
        //         test,
        //         result,
        //         commands,
        //         iterations,
        //     };

        //     Ok(Expression::Derived(derived))

        // }
        (keywords::AND, _) => {
            // Push true
            // <test_1>, And, BranchUnless to <next>
            // <test 2>, And, BranchUnless to <next>
            // ...
            // <test k>, And, but no BranchUnless
            // <next>
            let tests: Vec<_> = datums
                .into_iter()
                .map(|d| parse_expression_inner(d))
                .collect::<Result<_, _>>()?;

            let length = tests
                .iter()
                .fold(0, |acc, test: &Vec<_>| acc + test.len() + 2);
            let n_of_tests = tests.len();

            let mut instructions = vec![Instruction::Boolean(true)];
            let mut traveled = 0;
            for (_, test) in tests.into_iter().enumerate() {
                let test_len = test.len();
                instructions.extend(test);
                instructions.push(Instruction::And);
                instructions.push(Instruction::ROBranchUnless(
                    length - traveled - test_len - 1,
                ));
                traveled += test_len;
            }
            // The last Branch is redundant
            if n_of_tests > 0 {
                instructions.pop();
            }
            Ok(instructions)
        }
        (keywords::OR, _) => {
            let tests: Vec<_> = datums
                .into_iter()
                .map(|d| parse_expression_inner(d))
                .collect::<Result<_, _>>()?;

            let n_of_tests = tests.len();
            let length = tests
                .iter()
                .fold(0, |acc, test: &Vec<_>| acc + test.len() + 2)
                - if n_of_tests > 0 { 1 } else { 0 };

            let mut instructions = vec![Instruction::Boolean(false)];
            let mut traveled = 0;
            for (_, test) in tests.into_iter().enumerate() {
                let test_len = test.len();
                instructions.extend(test);
                instructions.push(Instruction::Or);
                instructions.push(Instruction::ROBranchIf(length - traveled - test_len - 1));
                traveled += test_len + 2;
            }
            // The last Branch is redundant
            if n_of_tests > 0 {
                instructions.pop();
            }
            Ok(instructions)
        }

        // (keywords::BEGIN, l) if l >= 2 => {
        //     let mut commands = datums.into_expressions()?;
        //     let expression = Box::new(commands.pop().unwrap());
        //     let derived = Derived::Begin {
        //         commands,
        //         expression,
        //     };
        //     Ok(Expression::Derived(derived))
        // }
        // (keywords::DELAY, 1) => parse_expression(datums.pop_front().unwrap())
        //     .map(Box::new)
        //     .map(Derived::Delay)
        //     .map(Expression::Derived),
        _ => return Err(ParsingError::Illegal),
    }
}


fn compile_quotation(d: Datum) -> Result<Vec<Instruction>, ParsingError> {
    let instructions = match d {
        Datum::Symbol(s) => vec![Instruction::Symbol(s.into())],
        Datum::List(datums) => {
            let mut instructions = vec![];
            let n = datums.len();
            for d in datums.into_iter() {
                instructions.extend(compile_quotation(d)?);
            }
            instructions.push(Instruction::List(n, false));
            instructions
        }
        Datum::Pair { car, cdr } => {
            let mut instructions = vec![];
            let n = car.len() - 1;
            for d in car.into_iter() {
                instructions.extend(compile_quotation(d)?);
            }
            instructions.extend(compile_quotation(*cdr)?);
            instructions.push(Instruction::List(n, true));
            instructions
        }
        Datum::Abbreviation {
            kind: AbbreviationKind::Quote,
            datum,
        } => {
            let mut instructions = compile_quotation(*datum)?;
            instructions.insert(0, Instruction::Symbol(keywords::QUOTE.into()));
            instructions.push(Instruction::List(2, false));
            instructions
        }
        Datum::Abbreviation {
            kind: AbbreviationKind::Quasiquote,
            ..
        } => panic!("todo quasiquote"),
        d => parse_expression_inner(d)?,
    };

    Ok(instructions)
}

// //
// // Subexpressions
// //
// fn parse_else_clause(
//     datums: &mut VecDeque<Datum>,
// ) -> Result<Option<(Vec<Expression>, Box<Expression>)>, ParsingError> {
//     let mut else_clause = match datums.back().cloned() {
//         Some(Datum::List(l)) => if l.len() > 0 {
//             l
//         } else {
//             return Ok(None);
//         },
//         _ => return Ok(None),
//     };

//     match keyword_name(else_clause.pop_front().unwrap()) {
//         Some(ref s) if &s[..] == keywords::ELSE => {}
//         _ => return Ok(None),
//     }

//     datums.pop_back();

//     check![(else_clause.len() > 0), ParsingError::Illegal];

//     let mut exprs = else_clause.into_expressions()?;
//     let main = exprs.pop().unwrap();
//     Ok(Some((exprs, Box::new(main))))
// }

// fn parse_cond_clause_exp(datum: Datum) -> Result<CondClause, ParsingError> {
//     let mut list = datum.list().ok_or(ParsingError::Illegal)?;

//     check![(list.len() > 0), ParsingError::Illegal];

//     let test = list.pop_front()
//         .map(parse_expression)
//         .unwrap()
//         .map(Box::new)?;

//     let clause = match (list.get(0).cloned().map(keyword_name), list.len()) {
//         (Some(Some(ref s)), 2) if s == keywords::ARROW => {
//             let recipient = list.pop_back().map(parse_expression).unwrap()?;
//             CondClause::Arrow {
//                 test: test,
//                 recipient: Box::new(recipient),
//             }
//         }
//         _ => {
//             let expressions = list.into_expressions()?;
//             CondClause::Normal { test, expressions }
//         }
//     };

//     Ok(clause)
// }

// fn parse_case_clause_exp(datum: Datum) -> Result<CaseClause, ParsingError> {
//     let mut list = datum.list().ok_or(ParsingError::Illegal)?;

//     check![(list.len() >= 2), ParsingError::Illegal];

//     let datums = list.pop_front()
//         .unwrap()
//         .list()
//         .ok_or(ParsingError::Illegal)?;

//     let mut commands = list.into_expressions()?;
//     let expression = commands.pop().unwrap();

//     let clause = CaseClause {
//         datums,
//         commands,
//         expression: Box::new(expression),
//     };

//     Ok(clause)
// }

// panics unless datums.len() >= 1
fn parse_let_exp(
    mut datums: VecDeque<Datum>,
    let_type: LetExp,
) -> Result<Vec<Instruction>, ParsingError> {
    if let LetExp::NamedLet = let_type {
        panic!("named let");
    }

    let bindings_list: Vec<(ImmutableString, Vec<Instruction>)> = datums
        .pop_front()
        .unwrap()
        .list()
        .ok_or(ParsingError::Illegal)?
        .into_iter()
        .map(|binding| {
            let mut pair = binding.list().ok_or(ParsingError::Illegal)?;
            check![(pair.len() == 2), ParsingError::Illegal];

            let variable = parse_variable(pair.pop_front().unwrap())?;
            let init = parse_expression_inner(pair.pop_front().unwrap())?;

            Ok((variable.into(), init))
        })
        .collect::<Result<Vec<(_, _)>, _>>()?;

    let body = parse_body(datums)?;

    let n_of_bindings = bindings_list.len();
    let mut instructions = vec![];
    match let_type {
        LetExp::Let => {
            let mut definitions = vec![];
            for (v, init) in bindings_list.into_iter() {
                instructions.extend(init);
                definitions.push(Instruction::DefineVar(v));
            }

            instructions.push(Instruction::NewEnv);

            let mut definitions = definitions.into_iter();
            while let Some(def) = definitions.next_back() {
                instructions.push(def);
            }
        }
        LetExp::LetStar => for (v, init) in bindings_list.into_iter() {
            instructions.push(Instruction::NewEnv);
            instructions.extend(init);
            instructions.push(Instruction::DefineVar(v));
        },
        LetExp::LetRec => {
            instructions.push(Instruction::NewEnv);
            for &(ref v, _) in bindings_list.iter() {
                instructions.push(Instruction::Nil);
                instructions.push(Instruction::DefineVar(v.clone()));
            }
            for (v, init) in bindings_list.into_iter() {
                instructions.extend(init);
                instructions.push(Instruction::DefineVar(v));
            }
        }
        LetExp::NamedLet => unreachable!(),
    }

    instructions.extend(body);
    let pops = if let LetExp::LetStar = let_type {
        n_of_bindings
    } else {
        1
    };

    use std::iter::repeat;
    instructions.extend(repeat(Instruction::PopEnv).take(pops));
    Ok(instructions)
}

fn parse_call_exp(
    operator_d: Datum,
    mut operands_d: VecDeque<Datum>,
) -> Result<Vec<Instruction>, ParsingError> {
    let n_of_args = operands_d.len();
    operands_d.push_front(operator_d);

    // Ugh, VecDeque
    operands_d = {
        let mut vec: Vec<_> = operands_d.into_iter().collect();
        vec.reverse();
        vec.into_iter().collect()
    };

    let mut instructions = operands_d.compiled()?;

    instructions.push(Instruction::Integer(n_of_args as isize));
    instructions.push(Instruction::Call(false));
    Ok(instructions)
}

fn parse_lambda_exp(
    formals: LambdaFormals,
    datums: VecDeque<Datum>,
) -> Result<Vec<Instruction>, ParsingError> {
    let mut instructions = vec![];

    match formals {
        LambdaFormals::List(args) => {
            instructions.push(Instruction::Arity(args.len(), false));
            let load_instructions = args.into_iter()
                .map(|arg| Instruction::DefineVar(arg.into()));
            instructions.extend(load_instructions);
        }
        LambdaFormals::VarArgs(arg) => instructions.extend(vec![
            Instruction::Arity(0, false),
            Instruction::DefineVar(arg.into()),
        ]),
        LambdaFormals::Rest(mut args, rest) => {
            instructions.push(Instruction::Arity(args.len(), true));
            args.push(rest);
            instructions.extend(
                args.into_iter()
                    .map(|arg| Instruction::DefineVar(arg.into())),
            );
        }
    }

    let body = parse_body(datums)?;
    instructions.extend(body);
    instructions.push(Instruction::Ret);
    Ok(vec![Instruction::Lambda(Rc::new(instructions))])
}

fn parse_body(mut datums: VecDeque<Datum>) -> Result<Vec<Instruction>, ParsingError> {
    let definitions = parse_definitions(&mut datums);

    check![datums.len() > 0, ParsingError::Illegal];

    let mut commands: Vec<Vec<_>> = datums
        .into_iter()
        .map(|d| parse_expression_inner(d))
        .collect::<Result<_, _>>()?;
    let expression = commands.pop().unwrap();

    let mut instructions = vec![];

    for def in definitions.into_iter().chain(commands.into_iter()) {
        instructions.extend(def);
        instructions.push(Instruction::Pop);
    }
    instructions.extend(expression);

    Ok(instructions)
}

fn parse_definition(datum: Datum) -> Result<Vec<Instruction>, ParsingError> {
    let mut list = datum.list().ok_or(ParsingError::Illegal)?;

    check![list.len() > 0, ParsingError::Illegal];

    let symbol = keyword_name(list.pop_front().unwrap()).ok_or(ParsingError::Illegal)?;

    match &symbol[..] {
        keywords::BEGIN => {
            let mut instructions = vec![];
            for d in list.into_iter() {
                instructions.extend(parse_expression_inner(d)?);
                instructions.push(Instruction::Pop);
            }
            instructions.pop();
            Ok(instructions)
        }

        keywords::DEFINE if list.len() >= 2 => {
            let formals = list.pop_front().map(parse_lambda_formals_exp).unwrap();
            let instructions = match (formals, list.len()) {
                (Ok(LambdaFormals::VarArgs(variable)), 1) => {
                    let mut instructions = parse_expression_inner(list.pop_front().unwrap())?;
                    instructions.push(Instruction::DefineVar(variable.into()));
                    instructions
                }
                (Ok(LambdaFormals::List(mut args)), _) => {
                    check![args.len() > 0, ParsingError::Illegal];

                    let variable = args.remove(0);
                    let formals = LambdaFormals::List(args);
                    let mut instructions = parse_lambda_exp(formals, list)?;
                    instructions.push(Instruction::DefineVar(variable.into()));
                    instructions
                }
                (Ok(LambdaFormals::Rest(mut args, rest)), _) => {
                    check![args.len() == 1, ParsingError::Illegal];

                    let variable = args.remove(0);
                    let formals = LambdaFormals::VarArgs(rest);
                    let mut instructions = parse_lambda_exp(formals, list)?;
                    instructions.push(Instruction::DefineVar(variable.into()));
                    instructions
                }
                _ => return Err(ParsingError::Illegal),
            };
            Ok(instructions)
        }
        _ => Err(ParsingError::Illegal),
    }
}

fn parse_lambda_formals_exp(datum: Datum) -> Result<LambdaFormals, ParsingError> {
    match (symbol_type(&datum), datum) {
        (_, Datum::List(l)) => l.into_variables().map(LambdaFormals::List),
        (_, Datum::Pair { car, cdr }) => {
            let vars = car.into_variables()?;
            let rest = parse_variable(*cdr)?;
            let formals = LambdaFormals::Rest(vars, rest);
            Ok(formals)
        }
        (Symbol::Variable, Datum::Symbol(s)) => Ok(LambdaFormals::VarArgs(s)),
        _ => Err(ParsingError::Illegal),
    }
}

fn parse_definitions(datums: &mut VecDeque<Datum>) -> Vec<Vec<Instruction>> {
    let mut definitions = vec![];

    loop {
        let maybe_def = datums
            .get(0)
            .cloned()
            .ok_or(ParsingError::Illegal)
            .and_then(|d| parse_definition(d));

        if let Ok(def) = maybe_def {
            definitions.push(def);
            datums.pop_front();
        } else {
            break;
        }
    }

    definitions
}

fn parse_variable(datum: Datum) -> Result<String, ParsingError> {
    if let (Symbol::Variable, Datum::Symbol(s)) = (symbol_type(&datum), datum) {
        Ok(s)
    } else {
        Err(ParsingError::Illegal)
    }
}

// fn parse_iteration_spec(datum: Datum) -> Result<IterationSpec, ParsingError> {
//     let mut list = datum.list().ok_or(ParsingError::Illegal)?;
//     check![list.len() == 2 || list.len() == 3, ParsingError::Illegal];

//     let variable = list.pop_front().map(parse_variable).unwrap()?;

//     let init = list.pop_front()
//         .map(parse_expression)
//         .unwrap()
//         .map(Box::new)?;

//     let step = match list.pop_front() {
//         Some(d) => Some(Box::new(parse_expression(d)?)),
//         None => None,
//     };


//     let spec = IterationSpec {
//         variable,
//         init,
//         step,
//     };
//     Ok(spec)
// }

// TO DO: does this method really take 1 datum? The grammar suggests so.
fn parse_quasiquotation(_datum: Datum) -> Result<Vec<Instruction>, ParsingError> {
    panic!("unimplemented: parse quasiquotation expressions");
}


//
// Helpers
//
enum Symbol {
    Variable,
    Keyword,
    None,
}

fn symbol_type(d: &Datum) -> Symbol {
    match *d {
        Datum::Symbol(ref s) => if is_syntactic_keyword(&s[..]) {
            Symbol::Keyword
        } else {
            Symbol::Variable
        },
        _ => Symbol::None,
    }
}


fn keyword_name(d: Datum) -> Option<String> {
    match (symbol_type(&d), d) {
        (Symbol::Keyword, Datum::Symbol(s)) => Some(s),
        _ => None,
    }
}

trait CompilerHelper: Sized {
    fn compiled(self) -> Result<Vec<Instruction>, ParsingError>;
    fn into_variables(self) -> Result<Vec<String>, ParsingError>;
}

impl CompilerHelper for VecDeque<Datum> {
    fn compiled(mut self) -> Result<Vec<Instruction>, ParsingError> {
        let mut acc = if let Some(d) = self.pop_front() {
            parse_expression_inner(d)?
        } else {
            return Ok(vec![]);
        };

        for d in self.into_iter() {
            acc.extend(parse_expression_inner(d)?);
        }

        Ok(acc)
    }

    fn into_variables(self) -> Result<Vec<String>, ParsingError> {
        self.into_iter()
            .map(|d| match (symbol_type(&d), d) {
                (Symbol::Variable, Datum::Symbol(s)) => Ok(s),
                _ => Err(ParsingError::Illegal),
            })
            .collect()
    }
}
