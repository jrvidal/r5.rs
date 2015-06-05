use std::fmt::{Debug, Formatter, Error};

/**
    A simple buffered reader to peek a chars stream
*/

const BUFFER_SIZE : u8 = 12;

pub struct Peekable<T: Iterator<Item=char>> {
    buffer: [char; BUFFER_SIZE as usize],
    index: u8,
    end: u8,
    stream: T,
    exhausted: bool,
}

#[derive(Debug)]
pub struct Peek {
    buffer: [char; BUFFER_SIZE as usize],
    end: u8,
    index: u8
}

impl<T: Iterator<Item=char>> Debug for Peekable<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        return write!(f, "buffer: {:?}\nindex: {}, end: {}", self.buffer, self.index, self.end);
    }
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

impl<T> Peekable<T> where T:Iterator<Item=char> {
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

    pub fn peek(&mut self) -> Peek {
        self._load();

        Peek {
            buffer: self.buffer.clone(),
            index: 0,
            end: self.end
        }
    }

    pub fn small_peek(&mut self) -> [Option<char>; 3] {
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

    pub fn advance(&mut self, n: usize) {
        if (self.end - self.index) as usize >= n {
            self.index += n as u8;
        } else {
            for _ in 0..n {
                self.next();
            }
        }
    }

    pub fn from_iter(iterator: T) -> Self {
        let mut p = Peekable {
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

impl<T> Iterator for Peekable<T> where T:Iterator<Item=char> {
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


#[test]
fn iterate() {
    let s = "abcd";
    let p = Peekable::from_iter(s.chars());
    assert!(p.collect::<String>() == s);
}

#[test]
fn iterate2() {
    let s = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";
    let p = Peekable::from_iter(s.chars());
    assert!(p.collect::<String>() == s);
}

#[test]
fn peek() {
    let s = "0123456789";
    let mut p = Peekable::from_iter(s.chars());

    for _ in 0..5 {
        p.next();
    }

    {
        let few = p.peek();

        assert!(few.collect::<String>() == "56789");
    }


    assert!(p.collect::<String>() == "56789");
}

#[test]
fn peek2() {
    let s = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";
    let mut p = Peekable::from_iter(s.chars());

    for _ in 0..30 {
        p.next();
    }

    {
        let few = p.peek();

        assert!(few.collect::<String>() == s[30..(30 + BUFFER_SIZE as usize)]);
    }

    assert!(p.collect::<String>() == &s[30..]);
}