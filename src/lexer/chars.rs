use std::collections::VecDeque;
use std::vec;

#[derive(Debug)]
pub struct Chars<I>
where
    I: Iterator<Item = char>,
{
    it: I,
    case_sensitive: bool,
    buf: VecDeque<I::Item>,
}

impl<I: Iterator<Item = char>> Chars<I> {
    fn fill_buf(&mut self) {
        loop {
            if self.buf.len() >= 10 {
                break;
            }

            if let Some(item) = self.it.next() {
                self.buf.push_back(item);
            } else {
                break;
            }
        }
    }
}

impl Chars<vec::IntoIter<char>> {
    #[cfg(test)]
    pub fn from_str(s: &str) -> Chars<vec::IntoIter<char>> {
        s.chars().collect::<Vec<_>>().into_iter().into()
    }
}

impl<I: Iterator<Item = char>> Iterator for Chars<I> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        self.fill_buf();
        self.buf.pop_front().map(|c| {
            if self.case_sensitive {
                c
            } else {
                c.to_ascii_lowercase()
            }
        })
    }
}

pub trait LexerIterator: Iterator<Item = char> {
    fn case_sensitive(&mut self, sensitive: bool);
    fn is_case_sensitive(&self) -> bool;
    // index is supposed to be <= 10
    fn peek(&mut self, index: usize) -> Option<char>;
    fn advance(&mut self, n: usize) {
        for _ in 0..n {
            let _ = self.next();
        }
    }

    fn peek_sensitive(&mut self, index: usize) -> Option<char> {
        let current = self.is_case_sensitive();
        self.case_sensitive(true);
        let ret = self.peek(index);
        self.case_sensitive(current);
        ret
    }
}

impl<I: Iterator<Item = char>> LexerIterator for Chars<I> {
    fn case_sensitive(&mut self, sensitive: bool) {
        self.case_sensitive = sensitive;
    }

    fn is_case_sensitive(&self) -> bool {
        self.case_sensitive
    }

    fn peek(&mut self, index: usize) -> Option<char> {
        self.fill_buf();
        self.buf.get(index).cloned().map(|c| {
            if self.case_sensitive {
                c
            } else {
                c.to_ascii_lowercase()
            }
        })
    }
}

impl<T: IntoIterator<Item = char>> From<T> for Chars<T::IntoIter> {
    fn from(into_iter: T) -> Chars<T::IntoIter> {
        Chars {
            it: into_iter.into_iter(),
            buf: VecDeque::new(),
            case_sensitive: false,
        }
    }
}
