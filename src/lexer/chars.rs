use std::ascii::AsciiExt;

#[derive(Debug)]
pub struct Chars {
    vec: Vec<char>,
    pub case_sensitive: bool,
    index: usize,
}

impl Iterator for Chars {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        let r = self.peek(0);
        self.index += 1;
        r
    }
}

impl Chars {
    pub fn peek(&self, i: usize) -> Option<char> {
        let c = self.vec.get(self.index + i);

        if self.case_sensitive {
            c.cloned()
        } else {
            c.map(|&d| d.to_ascii_lowercase())
        }
    }

    pub fn peek_sensitive(&self, i: usize) -> Option<char> {
        self.vec.get(self.index + i).cloned()
    }

    pub fn advance(&mut self, n: usize) {
        self.index += n;
    }

    pub fn from_vec(v: Vec<char>) -> Chars {
        Chars {
            vec: v,
            case_sensitive: false,
            index: 0,
        }
    }

    // REVISIT
    #[cfg(test)]
    pub fn from_str(s: &str) -> Chars {
        Chars::from_vec(s.chars().collect())
    }
}
