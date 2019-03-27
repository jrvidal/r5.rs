use std::collections::VecDeque;
use std::rc::Rc;

type LambdaFormals = (Vec<String>, Option<String>);

use super::{compile_expression_inner, keywords, CompilerError, CompilerHelper, Datum, DatumKind, Instruction};

// Order of formals: DefineVar(arg_n), ... DefineVar(arg_1)
pub(super) fn compile_lambda_exp(
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
    Ok(vec![Instruction::Lambda {
        code: Rc::new(instructions),
        arity,
    }])
}

pub(super) fn compile_body(
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

pub(super) fn compile_sequence(
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

fn compile_definition(datum: Datum, tail: bool) -> Result<Vec<Instruction>, CompilerError> {
    let mut list = datum.list().ok_or(CompilerError::Illegal)?;

    check![!list.is_empty(), CompilerError::Illegal];

    let symbol = super::keyword_name(list.pop_front().unwrap()).ok_or(CompilerError::Illegal)?;

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

pub(super) fn parse_lambda_formals_exp(datum: Datum) -> Result<LambdaFormals, CompilerError> {
    use super::Symbol;
    match (super::symbol_type(&datum), datum.tree) {
        (_, DatumKind::List(l)) => Ok((l.into_variables()?, None)),
        (_, DatumKind::Pair { car, cdr }) => {
            let vars = car.into_variables()?;
            let rest = super::parse_variable(*cdr)?;
            Ok((vars, Some(rest)))
        }
        (Symbol::Variable, DatumKind::Symbol(s)) => Ok((vec![], Some(s))),
        _ => Err(CompilerError::Illegal),
    }
}
