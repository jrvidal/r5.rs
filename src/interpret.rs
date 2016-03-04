use parser::{Expression, LambdaFormals, Definition, Derived, Binding};
use helpers::*;
use values::*;

const NO_VALUE : Object = Object::ValueObject(ValueObject::NoValue);
const TRUE : Object = Object::ValueObject(ValueObject::Boolean(true));
const FALSE : Object = Object::ValueObject(ValueObject::Boolean(false));

#[derive(Debug)]
pub enum EvalError {
    NonProcedure,
    UnboundVar(String),
    ArgumentCount,
}

pub fn eval(expression: &Expression, mut environment: &mut Environment, heap: &mut Heap) -> Result<Object, EvalError> {
    match *expression {
        Expression::Boolean(b) => Ok(Object::ValueObject(ValueObject::Boolean(b))),
        Expression::Character(c) => Ok(Object::ValueObject(ValueObject::Character(c))),
        // FIXME: remove clone
        Expression::String(ref s) => Ok(heap.insert_ref(RefObject::String(s.clone()))),
        Expression::Number(_) => Ok(Object::ValueObject(ValueObject::Number)),

        Expression::Variable(ref s) => environment.get(s).ok_or_else(|| {
            EvalError::UnboundVar(s.clone())
        }),

        Expression::Assignment {
            variable: ref s,
            expression: ref exp
        } => {
            eval(exp, environment, heap).and_then(|val| {
                let assigned = environment.set_mut(s, val, false);
                if assigned {
                    Ok(NO_VALUE)
                } else {
                    Err(EvalError::UnboundVar(s.clone()))
                }
            })
        },

        Expression::Quotation(ref datum) => Ok(from_datum(datum, heap)),

        Expression::Conditional {ref test, ref consequent, ref alternate} => {
            let test_value = try![ eval(test, environment, heap) ].to_bool();

            if test_value {
                eval(consequent, environment, heap)
            } else {
                alternate.as_ref().map(|alt_exp| {
                    eval(alt_exp, environment, heap)
                }).unwrap_or(Ok(NO_VALUE))
            }
        },
        Expression::Derived(Derived::And(ref expressions)) => {
            if expressions.len() == 0 {
                return Ok(TRUE);
            }

            for exp in expressions.iter() {
                if !try![ eval(exp, environment, heap) ].to_bool() {
                    return Ok(FALSE);
                }
            }

            Ok(TRUE)
        },
        Expression::Derived(Derived::Or(ref expressions)) => {
            if expressions.len() == 0 {
                return Ok(FALSE);
            }

            for exp in expressions.iter() {
                if try![ eval(exp, environment, heap) ].to_bool() {
                    return Ok(TRUE);
                }
            }

            Ok(FALSE)
        },
        Expression::Derived(Derived::Let {
            ref bindings,
            ref body
        }) => {
            let mut let_env = Environment::new(Some(environment.clone()));

            let bounded : Vec<(&String, Object)> = try![ bindings.iter().map(|&Binding {
                variable: ref var_name,
                init: ref exp
            }| {
                (Ok(var_name), eval(exp, environment, heap)).result()
            }).collect() ];

            for (var_name, obj) in bounded.into_iter() {
                let_env.set(var_name, obj);
            }

            eval(&body.expression, &mut let_env, heap)
        },
        Expression::Lambda {
            ref formals,
            ref body
        } => {
            Ok(heap.insert_ref(RefObject::Procedure {
                environment: environment.clone(),
                body: Box::new(body.clone()),
                formals: formals.clone()
            }))
        },
        Expression::Call {
            operator: ref fun,
            operands: ref args
        } => {
            let evald_proc = try![ eval(fun, environment, heap) ];

            let proc_ref = match evald_proc {
                Object::RefObject(shared) => shared,
                _ => return Err(EvalError::NonProcedure)
            };

            let borrowed_proc = proc_ref.borrow();

            let (body, formals, env) = match *borrowed_proc {
                RefObject::Procedure {
                    ref body,
                    ref formals,
                    environment: ref env
                } => (body, formals, env),
                _ => return Err(EvalError::NonProcedure)
            };


            // TO DO: evaluation order...
            let evald_args : Vec<Object> = try![ args.into_iter()
                .map(|arg| {
                    eval(&arg, environment, heap)
                }).collect() ];

            let mut call_env = try![ build_call_env(formals, evald_args, env, heap) ];

            for def in body.definitions.iter() {
                try![ eval_definition(def, &mut call_env, heap) ];
            }

            for exp in body.commands.iter() {
               try![ eval(exp, &mut call_env, heap) ];
            }

            eval(&body.expression, &mut call_env, heap)
        },
        _ => panic!("unimplemented expression type for interpreter!")
    }
}

fn eval_definition(definition: &Definition, environment: &mut Environment, heap: &mut Heap) -> Result<(), EvalError> {
    match *definition {
        Definition::Define {
            variable: ref var_name,
            ref expression
        } => {
            eval(expression, environment, heap).map(|object| {
                environment.set(var_name, object);
            })
        },
        Definition::DefineLambda {
            variable: ref var_name,
            ref formals,
            ref body
        } => {
            let env = environment.clone();
            environment.set(var_name, heap.insert_ref(RefObject::Procedure {
                body: Box::new(body.clone()),
                formals: formals.clone(),
                environment: env
            }));
            Ok(())
        },
        Definition::Begin(ref definitions) => {
            for def in definitions.iter() {
                try![eval_definition(def, environment, heap)];
            }
            Ok(())
        }
    }
}

fn build_call_env(formals: &LambdaFormals, mut arguments: Vec<Object>, local_env: &Environment, heap: &mut Heap) -> Result<Environment, EvalError> {
    let mut call_env = Environment::new(Some(local_env.clone()));

    match *formals {
        LambdaFormals::List(ref formal_list) => {
            if formal_list.len() != arguments.len() {
                return Err(EvalError::ArgumentCount);
            }
            for (name, arg_value) in formal_list.iter().zip(arguments.into_iter()) {
                call_env.set(name, arg_value);
            }
        },
        LambdaFormals::Rest(ref formal_list, ref rest_param) => {
            if formal_list.len() > arguments.len() {
                return Err(EvalError::ArgumentCount);
            }

            let rest_args = arguments.split_off(formal_list.len());

            for (name, arg_value) in formal_list.iter().zip(arguments.into_iter()) {
                call_env.set(name, arg_value);
            }

            call_env.set(rest_param, make_list(rest_args, heap));
        },
        LambdaFormals::VarArgs(ref rest_param) => {
            call_env.set(rest_param, make_list(arguments, heap));
        },
    }

    Ok(call_env)
}
