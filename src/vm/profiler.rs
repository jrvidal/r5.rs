use compiler::InstructionRef;
use std::collections::{HashMap, hash_map::Entry};
use std::time::Instant;

pub trait Profiler {
    fn on_instruction_start(&mut self, instruction: InstructionRef);
    fn on_instruction_end(&mut self, instruction: InstructionRef);
    fn report(&self) -> Option<String> {
        None
    }
}

pub struct TimeProfiler {
    instructions: HashMap<InstructionRef, (usize, u64)>,
    next: Option<InstructionRef>,
    start: Instant,
}

impl TimeProfiler {
    pub fn new() -> TimeProfiler {
        TimeProfiler {
            instructions: HashMap::new(),
            next: None,
            start: Instant::now(),
        }
    }
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
        nanos += elapsed.as_secs() as u64 * 1000000000;
        nanos += elapsed.subsec_nanos() as u64;
        match self.instructions.entry(instruction) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().0 += 1;
                entry.get_mut().1 += nanos;
            }
            Entry::Vacant(vacant) => {
                vacant.insert((1, nanos));
            }
        }
    }

    fn report(&self) -> Option<String> {
        let (total_time, total_instructions) = self.instructions
            .iter()
            .fold((0, 0), |(acc_time, acc_ins), (_, (count, nanos))| {
                (acc_time + nanos, acc_ins + count)
            });

        let mut report = String::new();

        let mut lines = self.instructions
            .iter()
            .map(|(instruction, &(count, nanos))| {
                let instruction_duration = (nanos as f64) / (count as f64);
                let line = format!(
                    "{:15}: {:6.0} ns on avg,  {:2.0}% of total time,  {:2.0}% of total ins.\n",
                    instruction,
                    instruction_duration,
                    100.0 * (nanos as f64) / (total_time as f64),
                    100.0 * (count as f64) / (total_instructions as f64)
                );

                (instruction_duration, line)
            })
            .collect::<Vec<_>>();

        lines.sort_by(|(dur1, _), (dur2, _)| dur1.partial_cmp(dur2).unwrap().reverse());

        lines.into_iter().for_each(|(_, line)| report += &line);

        Some(report)
    }
}

pub struct NoopProfiler;

impl Profiler for NoopProfiler {
    fn on_instruction_start(&mut self, _: InstructionRef) {}
    fn on_instruction_end(&mut self, _: InstructionRef) {}
}
