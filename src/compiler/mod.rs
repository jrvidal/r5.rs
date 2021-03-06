//! Convert expression(s) into bytecode

use std::collections::VecDeque;
use std::fmt::Debug;
use std::fmt::{self, Display};
use std::rc::Rc;

use self::keywords::is_syntactic_keyword;
use helpers::*;
use lexer::Num;
use reader::{AbbreviationKind, Datum};

mod keywords;

/// The "ISA" of the interpreter
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    /// Pushes a string into the stack
    String(CowString),
    /// Pushes a character into the stack
    Character(char),
    /// Pushes a boolean into the stack
    Boolean(bool),
    /// Pushes a string into the stack
    Symbol(ImmutableString),
    /// Pushes an integer into the stack
    Integer(i32),
    /// Pushes a float into the stack
    Float(f32),
    /// Pushes an invalid number into the stack
    InvalidNumber,
    /// Pushes the Nil value into the stack
    Nil,
    /// Pushes an empty list
    EmptyList,
    /// Call the top-most value of the stack with (`<is tail call>`, `<number of arguments in the stack>`)
    Call(bool, usize),
    /// Arity check for a procedure, read-only
    Arity(usize, bool),
    /// Return from procedure call
    Ret,
    /// Branch `+n` instructions unconditionally
    Branch(isize),
    /// Branch `+n` instructions if popped value is truthy
    BranchIf(isize),
    /// Branch `+n` instructions if popped value is falsy
    BranchUnless(isize),
    /// Branch `+n` instructions if top-most stack value is truthy (without popping)
    ROBranchIf(isize),
    /// Branch `+n` instructions if top-most stack value is falsy (without popping)
    ROBranchUnless(isize),
    /// Push compiled lambda into the stack
    Lambda {
        code: Rc<Vec<Instruction>>,
        arity: (usize, bool),
    },
    /// Push compiled promise into the stack
    Promise(Rc<Vec<Instruction>>),
    /// Push pair of two popped values into the stack
    Pair,
    /// Push list from popped values into the stack, with `(<number of elements>, <improper>)`.
    /// An improper list pops 2 extra values out of the stack.
    List(usize, bool),
    /// Push vector of `n` popped values into the stack.
    Vector(usize),
    /// Flattens the top-most list on the stack (failing if it's not a list), pushing every value
    /// back into it again, and pushing last an integer count of the elements on the list
    Flatten,
    /// Pushes a dynamically sized list from popped values on the stack, with `(<n_of_segments>, <improper>)`.
    /// Each segment is preluded by a "header" in the stack indicating how many elements are to be popped.
    /// An `improper` list pops the par `cdr` first.
    DynList(usize, bool),
    /// Pushes a dynamically sized vector from popped values on the stack, using `n` segments with a similar
    /// strategy to `DynList`.
    DynVector(usize),
    // Pop a value out of the stack
    Pop,
    /// Push variable from current environment into the stack
    LoadVar(ImmutableString),
    /// Add popped value from the stack as a variable to the current environment
    DefineVar(ImmutableString),
    /// `set!` popped value from the stack in the current environment
    SetVar(ImmutableString),
    /// Create a new child from the current environment and replace it
    NewEnv,
    /// Replace the current environment with its parent
    PopEnv,
    /// Push the logical, lazy `and` of the two top-most values.
    And,
    /// Push the logical, lazy `or` of the two top-most values.
    Or,
    /// Compare popping _only_ the first operand
    Eq,
}

/// A lightweight identifier of an instruction type
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct InstructionRef(&'static str);

impl<'a> From<&'a Instruction> for InstructionRef {
    fn from(instruction: &Instruction) -> InstructionRef {
        InstructionRef(instruction.variant_name())
    }
}

impl Display for InstructionRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.pad(&self.0)
    }
}

impl Debug for InstructionRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.0)
    }
}

impl Instruction {
    // TODO: this should be a custome derive or something
    fn variant_name(&self) -> &'static str {
        use self::Instruction::*;

        match *self {
            String(..) => "String",
            Character(..) => "Character",
            Boolean(..) => "Boolean",
            Symbol(..) => "Symbol",
            Integer(..) => "Integer",
            Float(..) => "Float",
            InvalidNumber => "InvalidNumber",
            Nil => "Nil",
            EmptyList => "EmpytList",
            Call(..) => "Call",
            Arity(..) => "Arity",
            Ret => "Ret",
            Branch(..) => "Branch",
            BranchIf(..) => "BranchIf",
            BranchUnless(..) => "BranchUnless",
            ROBranchIf(..) => "ROBranchIf",
            ROBranchUnless(..) => "ROBranchUnless",
            Lambda { .. } => "Lambda",
            Promise { .. } => "Promise",
            Pair => "Pair",
            List(..) => "List",
            Vector(..) => "Vector",
            Flatten => "Flatten",
            DynList(..) => "DynList",
            DynVector(..) => "DynVector",
            Pop => "Pop",
            LoadVar(..) => "LoadVar",
            DefineVar(..) => "DefineVar",
            SetVar(..) => "SetVar",
            NewEnv => "NewEnv",
            PopEnv => "PopEnv",
            And => "And",
            Or => "Or",
            Eq => "Eq",
        }
    }
}

type LambdaFormals = (Vec<String>, Option<String>);

#[derive(Debug, Eq, PartialEq)]
pub enum CompilerError {
    Illegal,
}

macro_rules! check {
    ($check:expr, $err:expr) => {
        if !$check {
            return Err($err);
        }
    };
}

#[derive(Debug, Clone, Copy)]
enum LetExp {
    Let,
    LetRec,
    LetStar,
    NamedLet,
}

/// Compiles a datum into executable bytecode
pub fn compile_expression(d: Datum) -> Option<Vec<Instruction>> {
    compile_expression_inner(d, false).ok()
}

fn compile_expression_inner(d: Datum, tail: bool) -> Result<Vec<Instruction>, CompilerError> {
    macro_rules! simple_datum {
        ($T:ident, $c:expr) => {
            Ok(vec![Instruction::$T($c)])
        };
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
        (_, Datum::Number(nt)) => match nt.into() {
            Ok(Num::Integer(n)) => return simple_datum![Integer, n],
            Ok(Num::Float(f)) => return simple_datum![Float, f],
            Err(_) => return Ok(vec![Instruction::InvalidNumber]),
        },
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
        ) => return compile_quasiquotation(*datum),

        // Nope
        // http://stackoverflow.com/questions/18641757/unquoted-vectors-in-r5rs-scheme
        // (_, Datum::Vector(datums)) => {}

        // Delegate
        (_, Datum::List(datums)) => datums,
        _ => return Err(CompilerError::Illegal),
    };

    check![!datums.is_empty(), CompilerError::Illegal];

    let head = datums.pop_front().unwrap();
    let symbol = &match keyword_name(head.clone()) {
        Some(s) => s,
        _ => return compile_call_exp(head, datums, tail),
    };

    match (&symbol[..], datums.len()) {
        // // Verbose quotations
        (keywords::QUOTE, 1) => compile_quotation(datums.pop_front().unwrap()),
        // Verbose quasiquotations
        (keywords::QUASIQUOTE, 1) => compile_quasiquotation(datums.pop_front().unwrap()),
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
            instructions.push(Instruction::BranchUnless(test_offset as isize + 2));
            instructions.extend(consequent);
            instructions.push(Instruction::Branch(consequent_offset as isize + 1));

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
                _ => Err(CompilerError::Illegal),
            }
        }

        // Lambda
        (keywords::LAMBDA, l) if l >= 2 => {
            let formals = parse_lambda_formals_exp(datums.pop_front().unwrap())?;
            compile_lambda_exp(formals, datums)
        }

        (keywords::COND, l) if l >= 1 => {
            let else_expressions = compile_else_clause(&mut datums, tail)?;

            let mut clauses = vec![];
            let mut total_len = else_expressions.as_ref().map(|e| e.len()).unwrap_or(1);

            for datum in datums {
                let mut datums = if let Datum::List(datums) = datum {
                    datums
                } else {
                    return Err(CompilerError::Illegal);
                };

                check![!datums.is_empty(), CompilerError::Illegal];

                let arrow = match datums.get(1) {
                    Some(&Datum::Symbol(ref s)) if s == keywords::ARROW => true,
                    _ => false,
                };

                check![!(arrow && datums.len() != 3), CompilerError::Illegal];

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

            for (test, sequence, arrow) in clauses {
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
                    Instruction::ROBranchUnless(diff_to_next as isize)
                } else {
                    Instruction::BranchUnless(diff_to_next as isize)
                };

                instructions.push(branch_to_next);
                instructions.extend(sequence);

                let offset = test_len + 1 + seq_len + if arrow { 2 } else { 0 };

                let diff_to_end = total_len - walked_distance - offset;

                if arrow {
                    instructions.extend(vec![
                        Instruction::Arity(1, false),
                        Instruction::Call(tail, 1),
                        Instruction::Branch(diff_to_end as isize),
                        Instruction::Pop,
                    ]);
                } else {
                    instructions.push(Instruction::Branch(diff_to_end as isize));
                    if test_only_clause {
                        instructions.push(Instruction::Pop);
                    }
                }

                let step = offset + 1 + if arrow || test_only_clause { 1 } else { 0 };

                walked_distance += step;
            }

            match else_expressions {
                Some(ins) => instructions.extend(ins),
                None => instructions.push(Instruction::Nil),
            }

            Ok(instructions)
        }
        (keywords::CASE, l) if l >= 2 => {
            let key = compile_expression_inner(datums.pop_front().unwrap(), false)?;
            let else_expressions = compile_else_clause(&mut datums, tail)?;
            let mut total_len = else_expressions.as_ref().map(|ins| ins.len()).unwrap_or(1) + 1;

            let mut clauses = vec![];

            for datum in datums {
                let mut datums = if let Datum::List(l) = datum {
                    l
                } else {
                    return Err(CompilerError::Illegal);
                };

                check![datums.len() >= 2, CompilerError::Illegal];

                let cases: Vec<Vec<Instruction>> =
                    if let Datum::List(l) = datums.pop_front().unwrap() {
                        l.into_iter()
                            .map(compile_quotation)
                            .collect::<Result<_, _>>()?
                    } else {
                        return Err(CompilerError::Illegal);
                    };

                // If no cases, we don't need to emit this branch
                if cases.is_empty() {
                    continue;
                }

                // Cases plus Eq, BranchIf per case
                let cases_len = cases.iter().fold(0, |acc, d| acc + d.len() + 2);
                total_len += cases_len;

                let sequence = compile_sequence(datums, tail)?;

                total_len += sequence.len();
                // Prefix and suffix: pop, branch, pop / and branch
                total_len += 3;

                clauses.push((cases, sequence, cases_len));
            }

            // <key>, <d1>, Eq, BranchIf, <d2>, Eq, BranchIf, ..., <dn>, Eq, BranchIf, Branch(to next), Pop, <sequence>, Branch (to end)

            let mut instructions = key;
            let mut traveled = 0;

            for (cases, sequence, cases_len) in clauses {
                let mut diff_to_sequence = cases_len + 2;

                for case in cases {
                    diff_to_sequence -= case.len();
                    instructions.extend(case);
                    instructions.push(Instruction::Eq);
                    diff_to_sequence -= 1;
                    instructions.push(Instruction::BranchIf(diff_to_sequence as isize - 1));
                    diff_to_sequence -= 1;
                }

                let seq_len = sequence.len();
                instructions.push(Instruction::Branch(seq_len as isize + 3));
                instructions.push(Instruction::Pop);

                let offset = cases_len + seq_len + 3;
                traveled += offset;

                instructions.extend(sequence);
                instructions.push(Instruction::Branch((total_len - traveled) as isize + 1));
            }

            instructions.push(Instruction::Pop);

            match else_expressions {
                Some(ins) => instructions.extend(ins),
                None => instructions.push(Instruction::Nil),
            };

            Ok(instructions)
        }
        (keywords::LET, l) if l >= 2 => match symbol_type(&datums[0]) {
            Symbol::Variable => compile_let_exp(datums, LetExp::NamedLet, tail),
            _ => compile_let_exp(datums, LetExp::Let, tail),
        },
        (keywords::LETREC, l) if l >= 2 => compile_let_exp(datums, LetExp::LetRec, tail),
        (keywords::LET_STAR, l) if l >= 2 => compile_let_exp(datums, LetExp::LetStar, tail),
        (keywords::DO, l) if l >= 2 => {
            let variables = datums
                .pop_front()
                .unwrap()
                .list()
                .ok_or(CompilerError::Illegal)?;

            let (mut vars, inits, steps) = {
                let mut vars = vec![];
                let mut inits = VecDeque::new();
                let mut steps = vec![];

                for variable in variables {
                    let mut parts = variable.list().ok_or(CompilerError::Illegal)?;
                    check![parts.len() == 2 || parts.len() == 3, CompilerError::Illegal];
                    let var = parts.pop_front().unwrap();

                    if let (Symbol::Variable, Datum::Symbol(s)) = (symbol_type(&var), var) {
                        vars.push(s);
                    } else {
                        return Err(CompilerError::Illegal);
                    };
                    inits.push_back(parts.pop_front().unwrap());
                    let step = match parts.pop_front() {
                        Some(d) => Some(compile_expression_inner(d, false)?),
                        None => None,
                    };
                    steps.push(step);
                }

                (vars, inits, steps)
            };

            let mut test_result = datums
                .pop_front()
                .unwrap()
                .list()
                .ok_or(CompilerError::Illegal)?;

            check![!test_result.is_empty(), CompilerError::Illegal];
            let test = compile_expression_inner(test_result.pop_front().unwrap(), false)?;
            let sequence = compile_sequence(test_result, tail)?;

            let commands = compile_sequence(datums, false)?;

            // <init>, <test>, BranchIf (to sequence), <commands>, <steps>, <refresh vars>, Branch(to test), <sequence>

            let mut instructions = vec![];

            instructions.extend(inits.compiled()?);

            for i in 0..vars.len() {
                instructions.push(Instruction::DefineVar(
                    (&vars[vars.len() - i - 1])[..].into(),
                ));
            }

            let test_len = test.len();
            instructions.extend(test);

            let diff_to_sequence = commands.len() + 1 + vars.len()
                + steps.iter().fold(0, |acc, step| {
                    acc + step.as_ref().map(|s| s.len()).unwrap_or(0)
                }) + 1;

            instructions.push(Instruction::BranchIf((diff_to_sequence) as isize + 1));

            instructions.extend(commands);
            instructions.push(Instruction::Pop);
            for step in steps
                .into_iter()
                .filter(|s| s.is_some())
                .map(|s| s.unwrap())
            {
                instructions.extend(step);
            }

            while let Some(var) = vars.pop() {
                instructions.push(Instruction::DefineVar(var.into()));
            }

            instructions.push(Instruction::Branch(
                -((diff_to_sequence + test_len) as isize),
            ));

            if sequence.is_empty() {
                instructions.push(Instruction::Nil);
            } else {
                instructions.extend(sequence)
            }

            Ok(instructions)
        }

        (keywords::AND, _) => {
            // Push true
            // <test_1>, And, BranchUnless to <next>
            // <test 2>, And, BranchUnless to <next>
            // ...
            // <test k>, And, but no BranchUnless
            // <next>
            let n_of_tests = datums.len();
            let tests: Vec<_> = datums
                .into_iter()
                .enumerate()
                .map(|(i, d)| compile_expression_inner(d, tail && i == n_of_tests - 1))
                .collect::<Result<_, _>>()?;

            let n_of_tests = tests.len();
            let length = tests
                .iter()
                .fold(0, |acc, test: &Vec<_>| acc + test.len() + 2)
                - if n_of_tests > 0 { 1 } else { 0 };

            let mut instructions = vec![Instruction::Boolean(true)];
            let mut traveled = 0;
            for (_, test) in tests.into_iter().enumerate() {
                let test_len = test.len();
                instructions.extend(test);
                instructions.push(Instruction::And);
                instructions.push(Instruction::ROBranchUnless(
                    (length - traveled - test_len) as isize - 1,
                ));
                traveled += test_len + 2;
            }
            // The last Branch is redundant
            if n_of_tests > 0 {
                instructions.pop();
            }
            Ok(instructions)
        }
        (keywords::OR, _) => {
            let n_of_tests = datums.len();
            let tests: Vec<_> = datums
                .into_iter()
                .enumerate()
                .map(|(i, d)| compile_expression_inner(d, tail && i == n_of_tests - 1))
                .collect::<Result<_, _>>()?;

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
                instructions.push(Instruction::ROBranchIf(
                    (length - traveled - test_len) as isize - 1,
                ));
                traveled += test_len + 2;
            }
            // The last Branch is redundant
            if n_of_tests > 0 {
                instructions.pop();
            }
            Ok(instructions)
        }

        // TODO: unclear if an empty begin is valid
        (keywords::BEGIN, _) => {
            if datums.is_empty() {
                return Ok(vec![Instruction::Nil]);
            }

            compile_sequence(datums, tail)
        }

        (keywords::DELAY, 1) => {
            let instructions = compile_expression_inner(datums.pop_front().unwrap(), false)?;
            let (settled, value) = find_unused_vars(&instructions);

            let (settled, value): (ImmutableString, ImmutableString) =
                (settled.into(), value.into());

            // (let
            //      ((settled #f) (value #f))
            //      (lambda () (if settled
            //                      value
            //                      (begin (set! value <expr>) (set! settled #t) value))))

            let mut compiled = vec![
                Instruction::NewEnv,
                Instruction::Boolean(false),
                Instruction::DefineVar(settled.clone()),
                Instruction::Boolean(false),
                Instruction::DefineVar(value.clone()),
            ];

            let jump_to_end = instructions.len() + 6;

            let mut body = vec![
                Instruction::LoadVar(settled.clone()),
                Instruction::BranchUnless(3),
                Instruction::LoadVar(value.clone()),
                Instruction::Branch(jump_to_end as isize),
            ];
            body.extend(instructions);
            body.extend(vec![
                Instruction::SetVar(value.clone()),
                Instruction::Pop,
                Instruction::Boolean(true),
                Instruction::SetVar(settled.clone()),
                Instruction::LoadVar(value),
                Instruction::Ret,
            ]);

            compiled.push(Instruction::Promise(Rc::new(body)));
            compiled.push(Instruction::PopEnv);
            Ok(compiled)
        }
        _ => Err(CompilerError::Illegal),
    }
}

fn compile_quotation(d: Datum) -> Result<Vec<Instruction>, CompilerError> {
    let instructions = match d {
        Datum::Symbol(s) => vec![Instruction::Symbol(s.into())],
        Datum::List(datums) => {
            let mut instructions = vec![];
            let n = datums.len();
            for d in datums {
                instructions.extend(compile_quotation(d)?);
            }
            instructions.push(Instruction::List(n, false));
            instructions
        }
        Datum::Pair { car, cdr } => {
            let mut instructions = vec![];
            let n = car.len() - 1;
            for d in car {
                instructions.extend(compile_quotation(d)?);
            }
            instructions.extend(compile_quotation(*cdr)?);
            instructions.push(Instruction::List(n, true));
            instructions
        }
        Datum::Abbreviation { kind, datum } => {
            let mut instructions = compile_quotation(*datum)?;
            let keyword: &str = kind.into();
            instructions.insert(0, Instruction::Symbol(keyword.into()));
            instructions.push(Instruction::List(2, false));
            instructions
        }
        Datum::Vector(datums) => {
            let mut instructions = vec![];
            let n = datums.len();

            let mut iter = datums.into_iter();

            while let Some(d) = iter.next_back() {
                instructions.extend(compile_quotation(d)?);
            }

            instructions.push(Instruction::Vector(n));
            return Ok(instructions);
        }
        d => compile_expression_inner(d, false)?,
    };

    Ok(instructions)
}

//
// Subexpressions
//
fn compile_else_clause(
    datums: &mut VecDeque<Datum>,
    tail: bool,
) -> Result<Option<(Vec<Instruction>)>, CompilerError> {
    let mut else_clause = match datums.back().cloned() {
        Some(Datum::List(l)) => if l.is_empty() {
            return Ok(None);
        } else {
            l
        },
        _ => return Ok(None),
    };

    match keyword_name(else_clause.pop_front().unwrap()) {
        Some(ref s) if &s[..] == keywords::ELSE => {}
        _ => return Ok(None),
    }

    datums.pop_back();

    check![!else_clause.is_empty(), CompilerError::Illegal];

    let main = else_clause.pop_back().unwrap();
    let mut instructions = else_clause.compiled()?;
    instructions.extend(compile_expression_inner(main, tail)?);

    Ok(Some(instructions))
}

// panics unless datums.len() >= 1
fn compile_let_exp(
    mut datums: VecDeque<Datum>,
    let_type: LetExp,
    tail: bool,
) -> Result<Vec<Instruction>, CompilerError> {
    let variable = match let_type {
        LetExp::NamedLet if datums.len() >= 3 => datums.pop_front().unwrap().symbol(),
        LetExp::NamedLet => return Err(CompilerError::Illegal),
        _ => None,
    };

    // (let fn ((x 'xinit) ...) <body>)
    // (let ((x 'xinit) ...) (letrec ((fn (lambda (x ...) <body>)) (fn x ...))))

    let bindings_list: VecDeque<(String, Vec<Instruction>)> = datums
        .pop_front()
        .unwrap()
        .list()
        .ok_or(CompilerError::Illegal)?
        .into_iter()
        .map(|binding| {
            let mut pair = binding.list().ok_or(CompilerError::Illegal)?;
            check![(pair.len() == 2), CompilerError::Illegal];

            let variable = parse_variable(pair.pop_front().unwrap())?;
            let init = compile_expression_inner(pair.pop_front().unwrap(), false)?;

            Ok((variable, init))
        })
        .collect::<Result<_, _>>()?;

    let n_of_bindings = bindings_list.len();
    let mut instructions = vec![];
    match let_type {
        LetExp::Let => {
            let mut definitions = vec![];
            for (v, init) in bindings_list {
                instructions.extend(init);
                definitions.push(Instruction::DefineVar(v.into()));
            }

            instructions.push(Instruction::NewEnv);

            let mut definitions = definitions.into_iter();
            while let Some(def) = definitions.next_back() {
                instructions.push(def);
            }
        }
        LetExp::LetStar => for (v, init) in bindings_list {
            instructions.push(Instruction::NewEnv);
            instructions.extend(init);
            instructions.push(Instruction::DefineVar(v.into()));
        },
        LetExp::LetRec => {
            instructions.push(Instruction::NewEnv);
            for &(ref v, _) in &bindings_list {
                instructions.push(Instruction::Nil);
                instructions.push(Instruction::DefineVar(v.clone().into()));
            }
            for (v, init) in bindings_list {
                instructions.extend(init);
                instructions.push(Instruction::DefineVar(v.into()));
            }
        }
        LetExp::NamedLet => {
            let mut definitions = vec![];
            let mut bindings = VecDeque::new();
            for (v, init) in bindings_list {
                instructions.extend(init);
                definitions.push(Instruction::DefineVar(v.clone().into()));
                bindings.push_back(v.clone())
            }

            instructions.push(Instruction::NewEnv);

            let mut definitions = definitions.into_iter();
            while let Some(def) = definitions.next_back() {
                instructions.push(def);
            }

            let variable = variable.unwrap();
            datums = named_let_body(variable, bindings, datums);
        }
    }

    let body = compile_body(datums, tail)?;
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

// (let fn ((x 'xinit) ...) <body>) is equivalent to:
// (let ((x 'xinit) ...) (letrec ((fn (lambda (x ...) <body>))) (fn x ...)))
fn named_let_body(
    variable: String,
    bindings: VecDeque<String>,
    body: VecDeque<Datum>,
) -> VecDeque<Datum> {
    let call = {
        let mut vec = VecDeque::new();
        vec.push_back(Datum::Symbol(variable.clone()));
        vec.extend(bindings.clone().into_iter().map(Datum::Symbol));
        Datum::List(vec)
    };
    let lambda = {
        let mut vec = VecDeque::new();
        vec.push_back(Datum::Symbol(keywords::LAMBDA.to_owned()));
        vec.push_back(Datum::List(
            bindings.into_iter().map(Datum::Symbol).collect(),
        ));
        vec.extend(body);
        Datum::List(vec)
    };

    let let_bindings = {
        let mut vec = VecDeque::new();
        vec.push_back(Datum::Symbol(variable));
        vec.push_back(lambda);
        Datum::List(vec![Datum::List(vec)].into_iter().collect())
    };

    let mut vec = VecDeque::new();
    vec.push_back(Datum::Symbol(keywords::LETREC.to_owned()));
    vec.push_back(let_bindings);
    vec.push_back(call);
    vec![Datum::List(vec)].into_iter().collect()
}

// Order of expressions: arg_1, ..., arg_n, operator
// Order in stack: operator, arg_n, ..., arg_1
fn compile_call_exp(
    operator_d: Datum,
    operands_d: VecDeque<Datum>,
    tail: bool,
) -> Result<Vec<Instruction>, CompilerError> {
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
) -> Result<Vec<Instruction>, CompilerError> {
    let mut instructions = vec![];

    let arity = (formals.0.len(), formals.1.is_some());
    let mut args = {
        let mut args = formals.0;
        args.extend(formals.1);
        args
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

fn compile_body(
    mut datums: VecDeque<Datum>,
    tail: bool,
) -> Result<Vec<Instruction>, CompilerError> {
    let definitions = compile_definitions(&mut datums);

    check![!datums.is_empty(), CompilerError::Illegal];

    let sequence = compile_sequence(datums, tail)?;

    let mut instructions = vec![];

    for def in definitions {
        instructions.extend(def);
    }

    instructions.extend(sequence);

    Ok(instructions)
}

fn compile_sequence(
    mut datums: VecDeque<Datum>,
    tail: bool,
) -> Result<Vec<Instruction>, CompilerError> {
    if datums.is_empty() {
        return Ok(vec![]);
    }

    let expression = compile_expression_inner(datums.pop_back().unwrap(), tail)?;

    let commands: Vec<Vec<_>> = datums
        .into_iter()
        .map(|d| compile_expression_inner(d, false))
        .collect::<Result<_, _>>()?;

    let mut instructions = vec![];

    for command in commands {
        instructions.extend(command);
        instructions.push(Instruction::Pop);
    }

    instructions.extend(expression);

    Ok(instructions)
}

fn compile_definition(datum: Datum, tail: bool) -> Result<Vec<Instruction>, CompilerError> {
    let mut list = datum.list().ok_or(CompilerError::Illegal)?;

    check![!list.is_empty(), CompilerError::Illegal];

    let symbol = keyword_name(list.pop_front().unwrap()).ok_or(CompilerError::Illegal)?;

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
            let formals = list.pop_front().map(parse_lambda_formals_exp).unwrap()?;
            let instructions = match (formals.0.len(), formals, list.len()) {
                (0, (_, Some(variable)), 1) => {
                    let mut instructions =
                        compile_expression_inner(list.pop_front().unwrap(), false)?;
                    instructions.push(Instruction::DefineVar(variable.into()));
                    instructions
                }
                (_, (mut args, None), _) => {
                    check![!args.is_empty(), CompilerError::Illegal];

                    let variable = args.remove(0);
                    let mut instructions = compile_lambda_exp((args, None), list)?;
                    instructions.push(Instruction::DefineVar(variable.into()));
                    instructions
                }
                (_, (mut args, Some(rest)), _) => {
                    check![args.len() == 1, CompilerError::Illegal];

                    let variable = args.remove(0);
                    let mut instructions = compile_lambda_exp((vec![], Some(rest)), list)?;
                    instructions.push(Instruction::DefineVar(variable.into()));
                    instructions
                }
            };
            Ok(instructions)
        }
        _ => Err(CompilerError::Illegal),
    }
}

fn parse_lambda_formals_exp(datum: Datum) -> Result<LambdaFormals, CompilerError> {
    match (symbol_type(&datum), datum) {
        (_, Datum::List(l)) => Ok((l.into_variables()?, None)),
        (_, Datum::Pair { car, cdr }) => {
            let vars = car.into_variables()?;
            let rest = parse_variable(*cdr)?;
            Ok((vars, Some(rest)))
        }
        (Symbol::Variable, Datum::Symbol(s)) => Ok((vec![], Some(s))),
        _ => Err(CompilerError::Illegal),
    }
}

fn compile_definitions(datums: &mut VecDeque<Datum>) -> Vec<Vec<Instruction>> {
    let mut definitions = vec![];

    loop {
        let maybe_def = datums
            .get(0)
            .cloned()
            .ok_or(CompilerError::Illegal)
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

fn parse_variable(datum: Datum) -> Result<String, CompilerError> {
    if let (Symbol::Variable, Datum::Symbol(s)) = (symbol_type(&datum), datum) {
        Ok(s)
    } else {
        Err(CompilerError::Illegal)
    }
}

fn compile_quasiquotation(datum: Datum) -> Result<Vec<Instruction>, CompilerError> {
    compile_quasiquotation_at_level(datum, QuasiquoteState::default())
        .map(|result| result.instructions)
}

#[derive(Default, Clone, Copy, Debug)]
struct QuasiquoteState {
    level: usize,
    spreadable: bool,
}

impl QuasiquoteState {
    fn nest(&self) -> QuasiquoteState {
        let mut clone = *self;
        clone.level += 1;
        clone
    }

    fn unnest(&self) -> QuasiquoteState {
        let mut clone = *self;
        clone.level -= 1;
        clone
    }

    fn spreadable(&self) -> QuasiquoteState {
        let mut clone = *self;
        clone.spreadable = true;
        clone
    }
}

#[derive(Debug)]
struct QuasiquoteResult {
    instructions: Vec<Instruction>,
    splice: bool,
}

impl From<Vec<Instruction>> for QuasiquoteResult {
    fn from(instructions: Vec<Instruction>) -> QuasiquoteResult {
        QuasiquoteResult {
            instructions,
            splice: false,
        }
    }
}

// TO DO: does this method really take 1 datum? The grammar suggests so.
fn compile_quasiquotation_at_level(
    datum: Datum,
    qq_state: QuasiquoteState,
) -> Result<QuasiquoteResult, CompilerError> {
    let mut splice = false;
    let instructions = match datum {
        Datum::Symbol(s) => vec![Instruction::Symbol(s.into())],
        Datum::List(datums) => {
            let compiled = datums
                .into_iter()
                .map(|d| compile_quasiquotation_at_level(d, qq_state.spreadable()));
            let (mut instructions, lists) = unroll_qquoted_list_elements(compiled)?;
            instructions.push(Instruction::DynList(lists, false));
            instructions
        }
        Datum::Pair { car, cdr } => {
            let compiled = car.into_iter()
                .map(|d| compile_quasiquotation_at_level(d, qq_state.spreadable()));
            let (mut instructions, lists) = unroll_qquoted_list_elements(compiled)?;
            instructions.extend(compile_quasiquotation_at_level(*cdr, qq_state)?.instructions);
            instructions.push(Instruction::DynList(lists, true));
            instructions
        }
        Datum::Abbreviation {
            kind: kind @ AbbreviationKind::Quote,
            datum,
        } => verbose_quotation(
            compile_quasiquotation_at_level(*datum, qq_state)?.instructions,
            kind,
        ),
        Datum::Abbreviation {
            kind: kind @ AbbreviationKind::Quasiquote,
            datum,
        } => verbose_quotation(
            compile_quasiquotation_at_level(*datum, qq_state.nest())?.instructions,
            kind,
        ),
        Datum::Abbreviation {
            kind: kind @ AbbreviationKind::Comma,
            datum,
        } => if qq_state.level == 0 {
            // TODO: we could optimize this into tail position sometimes
            compile_expression_inner(*datum, false)?
        } else {
            verbose_quotation(
                compile_quasiquotation_at_level(*datum, qq_state.unnest())?.instructions,
                kind,
            )
        },
        Datum::Abbreviation {
            kind: kind @ AbbreviationKind::CommaAt,
            datum,
        } => match qq_state {
            QuasiquoteState {
                level: 0,
                spreadable: true,
            } => {
                splice = true;
                compile_expression_inner(*datum, false)?
            }
            QuasiquoteState {
                level: 0,
                spreadable: false,
            } => Err(CompilerError::Illegal)?,
            _ => verbose_quotation(
                compile_quasiquotation_at_level(*datum, qq_state.unnest())?.instructions,
                kind,
            ),
        },
        Datum::Vector(datums) => {
            let compiled = datums
                .into_iter()
                .map(|d| compile_quasiquotation_at_level(d, qq_state.spreadable()));
            let (mut instructions, lists) = unroll_qquoted_list_elements(compiled)?;
            instructions.push(Instruction::DynVector(lists));
            instructions
        }
        d => compile_expression_inner(d, false)?,
    };

    Ok(QuasiquoteResult {
        instructions,
        splice,
    })
}

struct ReversedIterator<I: DoubleEndedIterator> {
    original: I,
}

impl<I> Iterator for ReversedIterator<I>
where
    I: DoubleEndedIterator,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.original.next_back()
    }
}

trait Reverse
where
    Self: Sized + DoubleEndedIterator,
{
    fn reverse(self) -> ReversedIterator<Self>;
}

impl<I> Reverse for I
where
    I: DoubleEndedIterator,
{
    fn reverse(self) -> ReversedIterator<Self> {
        ReversedIterator { original: self }
    }
}

#[inline]
fn verbose_quotation(
    mut instructions: Vec<Instruction>,
    kind: AbbreviationKind,
) -> Vec<Instruction> {
    let keyword: &str = kind.into();
    instructions.insert(0, Instruction::Symbol(keyword.into()));
    instructions.push(Instruction::List(2, false));
    instructions
}

#[inline]
fn unroll_qquoted_list_elements<I: Iterator<Item = Result<QuasiquoteResult, CompilerError>>>(
    compiled: I,
) -> Result<(Vec<Instruction>, usize), CompilerError> {
    let mut instructions = vec![];
    let mut accumulated_scalars = 0;
    let mut lists = 0;
    for result in compiled {
        let result = result?;
        if result.splice {
            if accumulated_scalars > 0 {
                lists += 1;
                instructions.push(Instruction::Integer(accumulated_scalars as i32));
                accumulated_scalars = 0;
            }
            lists += 1;
            instructions.extend(result.instructions);
            instructions.push(Instruction::Flatten);
        } else {
            accumulated_scalars += 1;
            instructions.extend(result.instructions);
        }
    }
    if accumulated_scalars > 0 {
        lists += 1;
        instructions.push(Instruction::Integer(accumulated_scalars as i32));
    }
    Ok((instructions, lists))
}

//
// Helpers
//
#[derive(PartialEq)]
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
    fn compiled(self) -> Result<Vec<Instruction>, CompilerError>;
    fn into_variables(self) -> Result<Vec<String>, CompilerError>;
}

impl CompilerHelper for VecDeque<Datum> {
    fn compiled(mut self) -> Result<Vec<Instruction>, CompilerError> {
        let mut acc = if let Some(d) = self.pop_front() {
            compile_expression_inner(d, false)?
        } else {
            return Ok(vec![]);
        };

        for d in self {
            acc.extend(compile_expression_inner(d, false)?);
        }

        Ok(acc)
    }

    fn into_variables(self) -> Result<Vec<String>, CompilerError> {
        self.into_iter()
            .map(|d| match (symbol_type(&d), d) {
                (Symbol::Variable, Datum::Symbol(s)) => Ok(s),
                _ => Err(CompilerError::Illegal),
            })
            .collect()
    }
}

fn find_unused_vars(instructions: &[Instruction]) -> (String, String) {
    let mut vars = ("a".to_owned(), "b".to_owned());

    loop {
        let mut overlap = false;
        for instruction in instructions {
            match *instruction {
                Instruction::LoadVar(ref v)
                | Instruction::DefineVar(ref v)
                | Instruction::SetVar(ref v) => {
                    let vstring: &String = &*v;
                    if vstring == &vars.0 || vstring == &vars.1 {
                        overlap = true;
                        break;
                    }
                }
                _ => continue,
            }
        }

        if overlap {
            vars.0.push('a');
            vars.1.push('b');
        } else {
            break vars;
        }
    }
}

impl From<AbbreviationKind> for &'static str {
    fn from(kind: AbbreviationKind) -> &'static str {
        match kind {
            AbbreviationKind::Comma => keywords::UNQUOTE,
            AbbreviationKind::CommaAt => keywords::UNQUOTE_SPLICING,
            AbbreviationKind::Quote => keywords::QUOTE,
            AbbreviationKind::Quasiquote => keywords::QUASIQUOTE,
        }
    }
}
