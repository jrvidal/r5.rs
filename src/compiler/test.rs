use super::*;

macro_rules! list_datum! {
  ($tokens:tt) => {
    Datum::List(vec![$tokens].into_iter().collect())
  };
}

#[test]
fn quasiquotation() {
    compile_expression(d)
}