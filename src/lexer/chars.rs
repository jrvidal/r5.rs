use std::ascii::AsciiExt;
use std::collections::VecDeque;

#[derive(Debug)]
pub struct Chars2<I>
where
    I: Iterator<Item = char>,
{
    it: I,
    case_sensitive: bool,
    buf: VecDeque<I::Item>,
}

impl<I: Iterator<Item = char>> Chars2<I> {
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

impl<I: Iterator<Item = char>> Iterator for Chars2<I> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        self.fill_buf();
        self.buf.pop_front().map(|c| if self.case_sensitive {
            c
        } else {
            c.to_ascii_lowercase()
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

impl<I: Iterator<Item = char>> LexerIterator for Chars2<I> {
    fn case_sensitive(&mut self, sensitive: bool) {
        self.case_sensitive = sensitive;
    }

    fn is_case_sensitive(&self) -> bool {
        self.case_sensitive
    }

    fn peek(&mut self, index: usize) -> Option<char> {
        self.fill_buf();
        self.buf
            .get(index)
            .cloned()
            .map(|c| if self.case_sensitive {
                c
            } else {
                c.to_ascii_lowercase()
            })
    }
}

impl<T: IntoIterator<Item = char>> From<T> for Chars2<T::IntoIter> {
    fn from(into_iter: T) -> Chars2<T::IntoIter> {
        Chars2 {
            it: into_iter.into_iter(),
            buf: VecDeque::new(),
            case_sensitive: true,
        }
    }
}

// use std::vec::IntoIter;
// impl From<&'static str> for Chars2<IntoIter<char>> {
//     #[test]
//     fn from(s: &'static str) -> Chars2<IntoIter<char>> {
//         Chars2 {
//             it: s.iter().collect::<Vec<_>>().into_iter(),
//             buf: VecDeque::new(),
//             case_sensitive: true,
//         }
//     }
// }

// #[derive(Debug)]
// pub struct Chars {
//     vec: Vec<char>,
//     pub case_sensitive: bool,
//     index: usize,
// }

// impl Iterator for Chars {
//     type Item = char;

//     fn next(&mut self) -> Option<char> {
//         let r = self.peek(0);
//         self.index += 1;
//         r
//     }
// }

// impl Chars {
//     pub fn peek(&self, i: usize) -> Option<char> {
//         let c = self.vec.get(self.index + i);

//         if self.case_sensitive {
//             c.cloned()
//         } else {
//             c.map(|&d| d.to_ascii_lowercase())
//         }
//     }

//     pub fn peek_sensitive(&self, i: usize) -> Option<char> {
//         self.vec.get(self.index + i).cloned()
//     }

//     pub fn advance(&mut self, n: usize) {
//         self.index += n;
//     }

//     pub fn from_vec(v: Vec<char>) -> Chars {
//         Chars {
//             vec: v,
//             case_sensitive: false,
//             index: 0,
//         }
//     }

//     // REVISIT
//     #[cfg(test)]
//     pub fn from_str(s: &str) -> Chars {
//         Chars::from_vec(s.chars().collect())
//     }
// }
