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

    use std::collections::HashMap;
    use std::collections::hash_map::Entry;
    use std::time::{Instant, Duration};
    use std::env::args;
    use env_logger;
    use r5rs::reader::*;
    use r5rs::lexer::*;
    use r5rs::vm::*;
    use r5rs::compiler::*;

    struct TimeProfiler {
        instructions: HashMap<InstructionRef, (usize, u64)>,
        next: Option<InstructionRef>,
        start: Instant
    }

    impl Profiler for TimeProfiler {
        fn on_instruction_start(&mut self, instruction: InstructionRef) {
            if self.next.is_some() {
                unreachable!()
            }
            self.next = Some(instruction.clone());
            self.start = Instant::now();
        }
        fn on_instruction_end(&mut self, instruction: InstructionRef) {
            if self.next.is_none() || *self.next.as_ref().unwrap() != instruction {
                unreachable!()
            }
            self.next = None;

            let elapsed = self.start.elapsed();
            let mut nanos = 0;
            nanos += (elapsed.as_secs() as u64 * 1000000000);
            nanos += elapsed.subsec_nanos() as u64;
            match self.instructions.entry(instruction) {
                Entry::Occupied(mut entry) => {
                    entry.get_mut().0 += 1;
                    entry.get_mut().1 += nanos;
                },
                Entry::Vacant(vacant) => {
                    vacant.insert((1, nanos));
                }
            }
        }
    }

    use rustyline;
    use rustyline::error::ReadlineError;

    pub fn main() {
        env_logger::init().expect("logger");

        let file = args().nth(1);

        match file {
            Some(file) => run_file(file),
            None => run_repl(),
        }
    }

    fn run_file(file_path: String) {
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

        interpret(&source[..], &environment, |result| {
            let should_continue = result.is_ok();
            last_value = Some(result);
            should_continue
        });

        if let Some(Err(err)) = last_value {
            println!("{:?}", err);
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

            interpret(&line[..], &environment, |result: Result<Value, _>| {
                let need_more = result.is_ok();
                match result {
                    Ok(v) => println!("{}", v.to_repl()),
                    Err(e) => {
                        println!("Error: {:?}", e);
                    }
                }
                need_more
            })
        }
    }

    fn interpret<F>(source: &str, environment: &GcShared<Environment>, mut cb: F)
    where
        F: FnMut(Result<Value, ExecutionError>) -> bool,
    {
        let mut tokens = match Tokens::new(source.chars()).collect() {
            Ok(tokens) => tokens,
            Err(e) => {
                println!("Invalid input: {:?}", e);
                return;
            }
        };
        
        let mut profiler = TimeProfiler {
            instructions: HashMap::new(),
            next: None,
            start: Instant::now()
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
            let should_continue = cb(exec(&bytecode, environment.clone(), &mut profiler));

            if !should_continue {
                break;
            }
        }

        for (instruction, (count, nanos)) in profiler.instructions {
            println!("{}: {} times, {} avg", instruction, count, (nanos as f64) / (count as f64));
        }
    }

}
