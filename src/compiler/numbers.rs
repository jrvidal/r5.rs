use lexer::NumberToken;

enum Num {
    Integer(i32),
    Float(f32),
}

fn convert(token: NumberToken) -> Option<Num> {
    unimplemented!()
}
