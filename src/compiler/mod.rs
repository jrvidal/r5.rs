use std::collections::VecDeque;
use std::rc::Rc;

use reader::{AbbreviationKind, Datum};
use self::keywords::is_syntactic_keyword;
use helpers::*;

#[derive(Debug, Clone, PartialEq)]
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
    // Call(is_tail, n_of_args), save return environment if it's not tail
    Call(bool, usize),
    // Arity check for a procedure, read-only
    Arity(usize, bool),
    // Return, restore return environment
    Ret,
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
    Lambda {
        code: Rc<Vec<Instruction>>,
        arity: (usize, bool),
    },
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
    compile_expression_inner(d, false).ok()
}

fn compile_expression_inner(d: Datum, tail: bool) -> Result<Vec<Instruction>, ParsingError> {
    macro_rules! simple_datum {
        ($T:ident, $c:expr) => (
            Ok(vec![Instruction::$T($c)])
        )
    }

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
        _ => return compile_call_exp(head, datums, tail),
    };

    match (&symbol[..], datums.len()) {
        // // Verbose quotations
        (keywords::QUOTE, 1) => compile_quotation(datums.pop_front().unwrap()),
        // // Verbose quasiquotations
        // (keywords::QUASIQUOTE, 1) => {
        //     Ok(datums.pop_front().map(Expression::QuasiQuotation).unwrap())
        // }
        // If
        (keywords::IF, 2) | (keywords::IF, 3) => {
            let test = compile_expression_inner(datums.pop_front().unwrap(), false)?;
            let consequent = compile_expression_inner(datums.pop_front().unwrap(), tail)?;
            let alternate = match datums
                .pop_front()
                .map(|d| compile_expression_inner(d, tail))
            {
                Some(Ok(bc)) => Some(bc),
                None => None,
                Some(err) => return err,
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
            let parsed = compile_expression_inner(datums.pop_front().unwrap(), false);
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
            return compile_lambda_exp(formals, datums);
        }

        (keywords::COND, l) if l >= 1 => {
            let else_expressions = compile_else_clause(&mut datums, tail)?;

            let mut clauses = vec![];
            let mut total_len = else_expressions.as_ref().map(|e| e.len()).unwrap_or(1);

            for datum in datums.into_iter() {
                let mut datums = if let Datum::List(datums) = datum {
                    datums
                } else {
                    return Err(ParsingError::Illegal);
                };

                check![datums.len() >= 1, ParsingError::Illegal];

                let arrow = match datums.get(1) {
                    Some(&Datum::Symbol(ref s)) if s == keywords::ARROW => true,
                    _ => false
                };

                check![!(arrow && datums.len() != 3), ParsingError::Illegal];

                if arrow {
                    datums.remove(1);
                }

                // Weird, but `test` is *never* in tail position, even if it's the only expression in a clause
                let test = compile_expression_inner(datums.pop_front().unwrap(), false)?;
                // <test> plus conditional branch
                total_len += test.len() + 1;

                let sequence = if arrow {
                    // Extra instructions: arity, call, pop
                    total_len += 3;
                    datums.compiled()?
                } else {
                    compile_sequence(datums, tail)?
                };

                // Sequence plus branch to end
                total_len += sequence.len() + 1;

                if sequence.is_empty() {
                    // Extra instruction: pop
                    total_len += 1;
                }

                clauses.push((test, sequence, arrow));
            }

            // Normal clause:
            // <test> | branch_unless | <sequence> | branch* (to end)
            // Test-only clause:
            // <test> | ro_branch_unless | branch (to end)* | pop
            // Arrow clause
            // <test> | ro_branch_unless | <expr> | arity_check | call | branch* (to end) | pop
            // (*) not present in the last clause without an else

            let mut instructions = vec![];
            let mut walked_distance = 0;

            for (test, sequence, arrow) in clauses.into_iter() {
                let test_len = test.len();
                let seq_len = sequence.len();
                let test_only_clause = sequence.is_empty();

                instructions.extend(test);

                let diff_to_next = if arrow {
                    seq_len + 4
                } else if test_only_clause {
                    2
                } else {
                    seq_len + 2
                };

                let branch_to_next = if arrow || test_only_clause {
                    Instruction::ROBranchUnless(diff_to_next)
                } else {
                    Instruction::BranchUnless(diff_to_next)
                };

                instructions.push(branch_to_next);
                instructions.extend(sequence);


                let offset = test_len + 1 + seq_len + if arrow { 2 } else { 0 };

                let diff_to_end = total_len - walked_distance - offset;

                if arrow {
                    instructions.extend(vec![
                        Instruction::Arity(1, false),
                        Instruction::Call(tail, 1),
                        Instruction::Branch(diff_to_end),
                        Instruction::Pop,
                    ]);
                } else {
                    instructions.push(Instruction::Branch(diff_to_end));
                    if test_only_clause { instructions.push(Instruction::Pop); }
                }

                let step = offset + 1 + if arrow || test_only_clause {
                    1
                } else {
                    0
                };

                walked_distance += step;
            }

            match else_expressions {
                Some(ins) => instructions.extend(ins),
                None => instructions.push(Instruction::Nil)
            }

            Ok(instructions)
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
        }
        (keywords::LET, l) if l >= 2 => match symbol_type(&datums[0]) {
            Symbol::Variable => compile_let_exp(datums, LetExp::NamedLet, tail),
            _ => compile_let_exp(datums, LetExp::Let, tail),
        },
        (keywords::LETREC, l) if l >= 2 => compile_let_exp(datums, LetExp::LetRec, tail),
        (keywords::LET_STAR, l) if l >= 2 => compile_let_exp(datums, LetExp::LetStar, tail),
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
            let last_exp = datums.len() - 1;
            let tests: Vec<_> = datums
                .into_iter()
                .enumerate()
                .map(|(i, d)| compile_expression_inner(d, tail && i == last_exp))
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
            let last_exp = datums.len() - 1;
            let tests: Vec<_> = datums
                .into_iter()
                .enumerate()
                .map(|(i, d)| compile_expression_inner(d, tail && i == last_exp))
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
        d => compile_expression_inner(d, false)?,
    };

    Ok(instructions)
}

//
// Subexpressions
//
fn compile_else_clause(
    datums: &mut VecDeque<Datum>,
    tail: bool
) -> Result<Option<(Vec<Instruction>)>, ParsingError> {
    let mut else_clause = match datums.back().cloned() {
        Some(Datum::List(l)) => if l.len() > 0 {
            l
        } else {
            return Ok(None);
        },
        _ => return Ok(None),
    };

    match keyword_name(else_clause.pop_front().unwrap()) {
        Some(ref s) if &s[..] == keywords::ELSE => {}
        _ => return Ok(None),
    }

    datums.pop_back();

    check![else_clause.len() > 0, ParsingError::Illegal];

    let main = else_clause.pop_back().unwrap();
    let mut instructions = else_clause.compiled()?;
    instructions.extend(compile_expression_inner(main, tail)?);

    Ok(Some(instructions))
}

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
fn compile_let_exp(
    mut datums: VecDeque<Datum>,
    let_type: LetExp,
    tail: bool,
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
            let init = compile_expression_inner(pair.pop_front().unwrap(), false)?;

            Ok((variable.into(), init))
        })
        .collect::<Result<Vec<(_, _)>, _>>()?;

    let body = compile_body(datums, tail)?;

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

// Order of expressions: arg_1, ..., arg_n, operator
// Order in stack: operator, arg_n, ..., arg_1
fn compile_call_exp(
    operator_d: Datum,
    operands_d: VecDeque<Datum>,
    tail: bool,
) -> Result<Vec<Instruction>, ParsingError> {
    let n_of_args = operands_d.len();
    let mut instructions = operands_d.compiled()?;
    instructions.extend(compile_expression_inner(operator_d, false)?);

    instructions.push(Instruction::Call(tail, n_of_args));
    Ok(instructions)
}

// Order of formals: DefineVar(arg_n), ... DefineVar(arg_1)
fn compile_lambda_exp(
    formals: LambdaFormals,
    datums: VecDeque<Datum>,
) -> Result<Vec<Instruction>, ParsingError> {
    let mut instructions = vec![];

    let (arity, mut args) = match formals {
        LambdaFormals::List(args) => {
            let n = args.len();
            ((n, false), args)
        }
        LambdaFormals::VarArgs(arg) => ((0, true), vec![arg]),
        LambdaFormals::Rest(mut args, rest) => {
            let n = args.len();
            args.push(rest);
            ((n, true), args)
        }
    };

    while let Some(arg) = args.pop() {
        instructions.push(Instruction::DefineVar(arg.into()))
    }

    let body = compile_body(datums, true)?;
    instructions.extend(body);
    instructions.push(Instruction::Ret);
    Ok(vec![
        Instruction::Lambda {
            code: Rc::new(instructions),
            arity,
        },
    ])
}

fn compile_body(mut datums: VecDeque<Datum>, tail: bool) -> Result<Vec<Instruction>, ParsingError> {
    let definitions = compile_definitions(&mut datums);

    check![datums.len() > 0, ParsingError::Illegal];

    let sequence = compile_sequence(datums, tail)?;

    let mut instructions = vec![];

    for def in definitions.into_iter() {
        instructions.extend(def);
    }

    instructions.extend(sequence);

    Ok(instructions)
}

fn compile_sequence(mut datums: VecDeque<Datum>, tail: bool) -> Result<Vec<Instruction>, ParsingError> {
    if datums.len() == 0 {
        return Ok(vec![]);
    }

    let expression = compile_expression_inner(datums.pop_back().unwrap(), tail)?;

    let commands: Vec<Vec<_>> = datums
        .into_iter()
        .map(|d| compile_expression_inner(d, false))
        .collect::<Result<_, _>>()?;

    let mut instructions = vec![];

    for command in commands.into_iter() {
        instructions.extend(command);
        instructions.push(Instruction::Pop);
    }

    instructions.extend(expression);

    Ok(instructions)
}

fn compile_definition(datum: Datum, tail: bool) -> Result<Vec<Instruction>, ParsingError> {
    let mut list = datum.list().ok_or(ParsingError::Illegal)?;

    check![list.len() > 0, ParsingError::Illegal];

    let symbol = keyword_name(list.pop_front().unwrap()).ok_or(ParsingError::Illegal)?;

    match &symbol[..] {
        // TODO: top-level begin wat
        keywords::BEGIN if list.is_empty() => Ok(vec![Instruction::Nil]),
        keywords::BEGIN => {
            let mut instructions = vec![];
            let last_exp = list.len() - 1;
            for (i, d) in list.into_iter().enumerate() {
                instructions.extend(compile_expression_inner(d, tail && i == last_exp)?);
                instructions.push(Instruction::Pop);
            }
            instructions.pop();
            Ok(instructions)
        }

        keywords::DEFINE if list.len() >= 2 => {
            let formals = list.pop_front().map(parse_lambda_formals_exp).unwrap();
            let instructions = match (formals, list.len()) {
                (Ok(LambdaFormals::VarArgs(variable)), 1) => {
                    let mut instructions =
                        compile_expression_inner(list.pop_front().unwrap(), false)?;
                    instructions.push(Instruction::DefineVar(variable.into()));
                    instructions
                }
                (Ok(LambdaFormals::List(mut args)), _) => {
                    check![args.len() > 0, ParsingError::Illegal];

                    let variable = args.remove(0);
                    let formals = LambdaFormals::List(args);
                    let mut instructions = compile_lambda_exp(formals, list)?;
                    instructions.push(Instruction::DefineVar(variable.into()));
                    instructions
                }
                (Ok(LambdaFormals::Rest(mut args, rest)), _) => {
                    check![args.len() == 1, ParsingError::Illegal];

                    let variable = args.remove(0);
                    let formals = LambdaFormals::VarArgs(rest);
                    let mut instructions = compile_lambda_exp(formals, list)?;
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

fn compile_definitions(datums: &mut VecDeque<Datum>) -> Vec<Vec<Instruction>> {
    let mut definitions = vec![];

    loop {
        let maybe_def = datums
            .get(0)
            .cloned()
            .ok_or(ParsingError::Illegal)
            .and_then(|d| compile_definition(d, false));

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
            compile_expression_inner(d, false)?
        } else {
            return Ok(vec![]);
        };

        for d in self.into_iter() {
            acc.extend(compile_expression_inner(d, false)?);
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
