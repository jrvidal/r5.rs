use std::fmt::{Debug, Formatter, Error};

/**
    A simple buffered reader to peek a chars stream
*/

const BUFFER_SIZE : u8 = 12;

#[derive(Debug)]
pub struct Peek {
    buffer: [char; BUFFER_SIZE as usize],
    end: u8,
    index: u8
}

impl Iterator for Peek {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        if self.index >= self.end {
            None
        } else {
            self.index += 1;
            Some(self.buffer[self.index as usize - 1])
        }
    }
}

pub trait Peekable : Iterator<Item=char> {
    fn peek(&mut self) -> Peek;
    fn small_peek(&mut self) -> [Option<char>; 3];
    fn advance(&mut self, n: usize);
}

/**
    An implementation of Peekable
*/
pub struct PeekableChars<T: Iterator<Item=char>> {
    buffer: [char; BUFFER_SIZE as usize],
    index: u8,
    end: u8,
    stream: T,
    exhausted: bool,
}

impl<T> PeekableChars<T> where T: Iterator<Item=char> {
    fn _load(&mut self) {
        for i in self.index as usize..self.end as usize {
            self.buffer[i - self.index as usize] = self.buffer[i];
        }
        self.end -= self.index;
        self.index = 0;

        while let Some(c) = self.stream.next() {
            self.buffer[self.end as usize] = c;
            self.end += 1;

            if self.end == BUFFER_SIZE {
                break;
            }
        }

        self.exhausted = self.end == 0;
    }

    pub fn from_iter(iterator: T) -> Self {
        let mut p : PeekableChars<T>= PeekableChars {
            index: 0,
            end: 0,
            stream: iterator,
            buffer: ['\x00'; BUFFER_SIZE as usize],
            exhausted: false
        };
        p._load();
        p
    }
}

impl<T: Iterator<Item=char>> Debug for PeekableChars<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        return write!(f, "buffer: {:?}\nindex: {}, end: {}", self.buffer, self.index, self.end);
    }
}

impl<T> Peekable for PeekableChars<T> where T: Iterator<Item=char> {
    fn peek(&mut self) -> Peek {
        self._load();

        Peek {
            buffer: self.buffer.clone(),
            index: 0,
            end: self.end
        }
    }

    fn small_peek(&mut self) -> [Option<char>; 3] {
        if self.end - self.index < 3 {
            self._load();
        }

        let diff = self.end - self.index;

        let mut result = [None; 3];

        if diff > 0 {
            result[0] = Some(self.buffer[self.index as usize]);
        }

        if diff > 1 {
            result[1] = Some(self.buffer[self.index as usize + 1]);
        }

        if diff > 2 {
            result[2] = Some(self.buffer[self.index as usize + 2]);
        }

        result
    }

    fn advance(&mut self, n: usize) {
        if (self.end - self.index) as usize >= n {
            self.index += n as u8;
        } else {
            for _ in 0..n {
                self.next();
            }
        }
    }

}

impl<T> Iterator for PeekableChars<T> where T:Iterator<Item=char> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        if self.exhausted {
            None
        } else {
            let c = self.buffer[self.index as usize];
            self.index += 1;
            if self.index == self.end {
                self._load();
            }
            Some(c)
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use super::BUFFER_SIZE;

    const S : &'static str = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";

    #[test]
    fn iterate_short() {
        let p = PeekableChars::from_iter(S.chars().take(4));
        assert!(p.collect::<String>() == S[..4]);
    }

    #[test]
    fn iterate_long() {
        let p = PeekableChars::from_iter(S.chars());
        assert!(p.collect::<String>() == S);
    }

    #[test]
    fn peek_short() {
        let mut p = PeekableChars::from_iter(S.chars().take(10));

        for _ in 0..5 {
            p.next();
        }

        {
            let few = p.peek();

            assert!(few.collect::<String>() == S[5..10]);
        }


        assert!(p.collect::<String>() == S[5..10]);
    }

    #[test]
    fn peek_long() {
        let mut p = PeekableChars::from_iter(S.chars());

        for _ in 0..30 {
            p.next();
        }

        {
            let few = p.peek();

            assert!(few.collect::<String>() == S[30..(30 + BUFFER_SIZE as usize)]);
        }

        assert!(p.collect::<String>() == S[30..]);
    }
}
