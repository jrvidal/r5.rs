use super::{compile_expression_inner, AbbreviationKind, CompilerError, Datum, Instruction};

pub(super) fn compile_quotation(d: Datum) -> Result<Vec<Instruction>, CompilerError> {
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

pub(super) fn compile_quasiquotation(datum: Datum) -> Result<Vec<Instruction>, CompilerError> {
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
