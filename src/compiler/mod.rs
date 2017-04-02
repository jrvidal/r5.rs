
use std::collections::HashMap;
use std::iter::once;

use parser::{Body, Expression, LambdaFormals};
use helpers::{CowString, ImmutableString};

#[derive(Debug, Clone)]
pub enum Instruction {
    Integer(isize),
    String(CowString),
    Character(char),
    Boolean(bool),
    Ret,
    Nil,
    Call(usize),
    Branch(usize),
    BranchUnless(usize),
    Lambda(usize),
    Pair,
    LoadVar(ImmutableString),
    SetVar(ImmutableString),
}

struct ByProduct {
    instructions: Vec<Instruction>,
    lambdas: Option<HashMap<usize, Vec<Instruction>>>,
}

pub fn compile(expression: Expression) -> (Vec<Instruction>, usize) {
    let mut counter = 0;
    let objects = compile_inner(expression, &mut counter);
    assemble(objects)
}

fn assemble(objects: ByProduct) -> (Vec<Instruction>, usize) {
    let mut lambda_indices = HashMap::new();
    let ByProduct {
        lambdas,
        instructions,
    } = objects;

    let mut final_instructions = vec![];

    if let Some(l) = lambdas {
        for (lambda_idx, code) in l.into_iter() {
            let offset = final_instructions.len();
            final_instructions.extend(code);
            lambda_indices.insert(lambda_idx, offset);
        }
    }

    let entry = final_instructions.len();
    final_instructions.extend(instructions);

    for ins in final_instructions.iter_mut() {
        if let Instruction::Lambda(ref mut lambda_idx) = *ins {
            *lambda_idx = *lambda_indices.get(lambda_idx).expect("unexpected lambda");
        }
    }

    (final_instructions, entry)
}

impl ByProduct {
    fn from_instructions(instructions: Vec<Instruction>) -> ByProduct {
        ByProduct {
            instructions,
            lambdas: None,
        }
    }
}

fn compile_inner(expression: Expression, mut counter: &mut usize) -> ByProduct {
    let from_instructions = ByProduct::from_instructions;

    match expression {
        Expression::String(s) => from_instructions(vec![Instruction::String(CowString::from(s))]),
        Expression::Character(c) => from_instructions(vec![Instruction::Character(c)]),

        Expression::Variable(s) => {
            from_instructions(vec![Instruction::LoadVar(ImmutableString::from(s))])
        }

        Expression::Assignment {
            variable,
            expression,
        } => {
            let mut byproduct = compile_inner(*expression, counter);
            byproduct
                .instructions
                .push(Instruction::SetVar(ImmutableString::from(variable)));
            byproduct
        }

        Expression::Boolean(b) => from_instructions(vec![Instruction::Boolean(b)]),

        Expression::Call {
            operator,
            mut operands,
        } => {
            let mut ins = vec![];
            let operands_n = operands.len();
            operands.reverse();
            operands.push(*operator);

            let mut lambdas = HashMap::new();

            for op in operands.into_iter() {
                let compiled = compile_inner(op, counter);
                ins.extend(compiled.instructions);
                merge_lambdas(&mut lambdas, compiled.lambdas);
            }
            ins.push(Instruction::Call(operands_n));

            ByProduct {
                instructions: ins,
                lambdas: if lambdas.len() > 0 {
                    Some(lambdas)
                } else {
                    None
                },
            }
        }
        Expression::Lambda { formals, body } => {
            let mut ins = vec![];
            let mut lambdas = HashMap::new();

            match formals {
                LambdaFormals::List(vars) => for var in vars {
                    ins.push(Instruction::SetVar(ImmutableString::from(var)));
                },
                _ => panic!("unsupported formals"),
            }

            let Body {
                definitions,
                commands,
                expression: body_expression,
            } = body;

            // TODO: handle definitions
            for (c, is_command) in commands
                .into_iter()
                .map(|c| (c, true))
                .chain(once((*body_expression, false)))
            {

                let ByProduct {
                    lambdas: c_lambdas,
                    instructions,
                } = compile_inner(c, counter);
                merge_lambdas(&mut lambdas, c_lambdas);
                ins.extend(instructions);
                ins.push(Instruction::Ret);
                if is_command {
                    // ins.push(Instruction::Pop);
                }
            }

            let lambda_id = *counter;
            *counter += 1;
            lambdas.insert(lambda_id, ins);

            ByProduct {
                instructions: vec![Instruction::Lambda(lambda_id)],
                lambdas: Some(lambdas),
            }
        }

        Expression::Conditional {
            test,
            consequent,
            alternate,
        } => {
            let mut ins = vec![];
            let mut lambdas = HashMap::new();

            let byproduct = compile_inner(*test, counter);
            merge_lambdas(&mut lambdas, byproduct.lambdas);
            ins.extend(byproduct.instructions);

            let byproduct = compile_inner(*consequent, counter);
            merge_lambdas(&mut lambdas, byproduct.lambdas);
            ins.push(Instruction::BranchUnless(byproduct.instructions.len() + 2));
            ins.extend(byproduct.instructions);

            if let Some(alternate) = alternate {
                let byproduct = compile_inner(*alternate, counter);
                ins.push(Instruction::Branch(byproduct.instructions.len() + 1));
                ins.extend(byproduct.instructions);
                merge_lambdas(&mut lambdas, byproduct.lambdas);
            } else {
                ins.push(Instruction::Nil);
            }

            ByProduct {
                instructions: ins,
                lambdas: if lambdas.len() > 0 {
                    Some(lambdas)
                } else {
                    None
                },
            }

        }

        _ => panic!("unsupported expression"),
    }
}


fn merge_lambdas(
    lambdas: &mut HashMap<usize, Vec<Instruction>>,
    other: Option<HashMap<usize, Vec<Instruction>>>,
) {
    if let Some(other_l) = other {
        lambdas.extend(other_l)
    }
}
