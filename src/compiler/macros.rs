use std::collections::VecDeque;

use helpers::*;
use super::{Datum};

const ELLIPSIS : &str = "...";

struct TransformerSpec {
    literals: Vec<ImmutableString>,
    syntax_rules: Vec<SyntaxRule>
}

struct SyntaxRule {
    pattern: Datum
}


fn validate_pattern(datum: &Datum) -> bool {
    let mut stack = VecDeque::new();
    stack.push_front((datum, false));

    while let Some((datum, can_be_ellipsis)) = stack.pop_back() {
      match datum {
        Datum::Boolean(..) | Datum::String(..) | Datum::Character(..) | Datum::Number(..) => {},
        Datum::Symbol(s) => match (&s[..], super::symbol_type(datum), can_be_ellipsis) {
            (ELLIPSIS, _, true) => {},
            (_, super::Symbol::Variable, _) => {},
            _ => return false
        }
        Datum::Abbreviation { .. } => return false,
        Datum::List(datums) | Datum::Vector(datums) => {
          let len = datums.len();
          let it = datums.iter()
            .enumerate()
            .map(|(i, d)| (d, len > 1 && i == len - 1));
          stack.extend(it);
        }
        Datum::Pair { car, cdr } => {
          stack.extend(car.iter().map(|d| (d, false)));
          stack.push_back((cdr, false));
        }
      }
    }

    true
}

fn validate_template(datum: &Datum) -> bool {
    unimplemented!()
}