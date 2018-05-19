extern crate env_logger;
#[cfg(not(target_os = "emscripten"))]
#[macro_use]
extern crate log;
extern crate r5rs;
#[cfg(not(target_os = "emscripten"))]
extern crate rustyline;

#[cfg(target_os = "emscripten")]
fn main() {}

#[cfg(not(target_os = "emscripten"))]
fn main() {
    not_web::main();
}

#[cfg(not(target_os = "emscripten"))]
mod not_web {

    use env_logger;
    use r5rs::compiler::*;
    use r5rs::lexer::*;
    use r5rs::reader::*;
    use r5rs::vm::*;
    use std::env::{args, var};

    use rustyline;
    use rustyline::error::ReadlineError;

    pub fn main() {
        env_logger::init();

        let file = args().nth(1);
        let with_profiler = var("PROFILE").map(|s| !s.is_empty()).unwrap_or(false);

        match file {
            Some(file) => run_file(file, with_profiler),
            None => run_repl(),
        }
    }

    fn run_file(file_path: String, with_profiler: bool) {
        use std::fs::File;
        use std::io::Read;

        let source = {
            let mut bf = String::new();
            let mut file = File::open(file_path).expect("Unable to find file");
            file.read_to_string(&mut bf).expect("Error reading");
            bf
        };
        let environment = default_env();
        let mut last_value = None;
        let mut time_profiler = None;

        {
            let callback = |result: Result<_, _>| {
                let should_continue = result.is_ok();
                last_value = Some(result);
                should_continue
            };

            if with_profiler {
                let mut profiler = TimeProfiler::new();
                interpret(&source[..], &environment, &mut profiler, callback);
                time_profiler = Some(profiler);
            } else {
                interpret(&source[..], &environment, &mut NoopProfiler, callback);
            }
        }

        if let Some(Err(err)) = last_value {
            println!("{:?}", err);
        }

        let profile_report = time_profiler.map(|p| p.report()).unwrap_or(None);

        if let Some(report) = profile_report {
            eprintln!("{}", report);
        }
    }

    fn run_repl() {
        let mut rl = rustyline::Editor::<()>::new();
        let environment = default_env();

        loop {
            let readline = rl.readline("> ");
            let line = match readline {
                Ok(line) => line,
                Err(ReadlineError::Eof) => break,
                Err(_) => continue,
            };

            rl.add_history_entry(&line);

            interpret(
                &line[..],
                &environment,
                &mut NoopProfiler,
                |result: Result<Value, _>| {
                    let need_more = result.is_ok();
                    match result {
                        Ok(v) => println!("{}", v.to_repl()),
                        Err(e) => {
                            println!("Error: {:?}", e);
                        }
                    }
                    need_more
                },
            )
        }
    }

    fn interpret<F, P>(
        source: &str,
        environment: &GcShared<Environment>,
        profiler: &mut P,
        mut cb: F,
    ) where
        F: FnMut(Result<Value, ExecutionError>) -> bool,
        P: Profiler,
    {
        let mut tokens = match Tokens::new(source.chars()).collect() {
            Ok(tokens) => tokens,
            Err(e) => {
                println!("Invalid input: {:?}", e);
                return;
            }
        };

        loop {
            let datum = match parse_datum(&mut tokens) {
                Ok(Some(datum)) => datum,
                Err(e) => {
                    println!("Invalid datum: {:?}", e);
                    break;
                }
                Ok(None) => {
                    break;
                }
            };

            let bytecode = match compile_expression(datum) {
                Some(bytecode) => bytecode,
                None => {
                    println!("Invalid expression");
                    break;
                }
            };

            debug!("Code:\n{:?}", bytecode);
            let should_continue = cb(exec(&bytecode, environment.clone(), profiler));

            if !should_continue {
                break;
            }
        }
    }

}
