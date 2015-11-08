pub trait Tuple2Helper<A, B, E> {
    fn result(self) -> Result<(A, B), E>;
}

impl<A, B, E> Tuple2Helper<A, B, E> for (Result<A, E>, Result<B, E>) {
    fn result(self) -> Result<(A, B), E> {
        match (self.0, self.1) {
            (Ok(a), Ok(b)) => Ok((a, b)),
            (Err(e), _) | (_, Err(e)) => Err(e)
        }
    }
}

pub trait Tuple3Helper<A, B, C, E> {
    fn result(self) -> Result<(A, B, C), E>;
}

impl<A, B, C, E> Tuple3Helper<A, B, C, E> for (Result<A, E>, Result<B, E>, Result<C, E>) {
    fn result(self) -> Result<(A, B, C), E> {
        match (self.0, self.1, self.2) {
            (Ok(a), Ok(b), Ok(c)) => Ok((a, b, c)),
            (Err(e), _, _) | (_, Err(e), _) | (_, _, Err(e)) => Err(e)
        }
    }
}

pub trait ResultHelper {
    fn result(self) -> Result<(), ()>;
}

impl ResultHelper for bool {
    fn result(self) -> Result<(), ()> {
        if self {
            Ok(())
        } else {
            Err(())
        }
    }
}