use neoncode::expr::LexValue;
use std::fmt::{write, Debug};

use crate::{Error, Result};

const TWO_CHAR_OPS: [&'static str; 7] = ["==", ">=", "=>", "<=", "=<", "||", "&&"];
const WHITESPACE: [char; 3] = [' ', '\n', '\t'];

#[derive(Debug, Clone)]

pub struct Lexer {
    pub cont: String,
    pub filename: String,
    pub index: usize,
}

pub trait LexVal {
    fn new_last_one(l: &Lexer) -> Self;
    fn new_last(l: &Lexer, count: usize) -> Self;
    fn get(&self) -> String;
}

impl LexVal for LexValue {
    fn new_last_one(l: &Lexer) -> LexValue {
        LexValue {
            file: l.filename.clone(),
            start: l.index - 1,
            end: l.index,
        }
    }
    fn new_last(l: &Lexer, count: usize) -> LexValue {
        LexValue {
            file: l.filename.clone(),
            start: l.index - count,
            end: l.index,
        }
    }
    fn get(&self) -> String {
        let lock = crate::FILE_CONTENTS.lock().unwrap();
        let cont = lock.get(&self.file).unwrap();
        return String::from_utf8_lossy(cont[self.start..self.end].as_bytes()).into_owned();
    }
}

#[derive(Debug, Clone)]

pub(crate) enum LexToken {
    Punc(LexValue),
    Id(LexValue),
    Op(LexValue),
    Number(LexValue),
    EOF,
}

impl LexToken {
    pub(crate) fn get_val(&self) -> LexValue {
        match self {
            LexToken::Punc(v) => v.clone(),
            LexToken::Id(v) => v.clone(),
            LexToken::Op(v) => v.clone(),
            LexToken::Number(v) => v.clone(),
            LexToken::EOF => panic!(),
        }
    }
}

impl Lexer {
    pub(crate) fn new(name: String, cont: String) -> Lexer {
        // let cont = fs::read_to_string(&name).unwrap();
        return Lexer {
            filename: name,
            cont: cont,
            index: 0,
        };
    }
    /*
    pub(crate) fn new_explicit(name: String, cont: String) -> Lexer {
        return Lexer {filename: name, cont: cont, index: 0};
    }*/

    pub(crate) fn lex(&mut self) -> Result<LexToken> {
        if self.index >= self.cont.as_bytes().len() {
            return Ok(LexToken::EOF);
        }
        while WHITESPACE.contains(&(self.cont.as_bytes()[self.index] as char)) {
            self.index += 1;
            if self.index >= self.cont.as_bytes().len() {
                return Ok(LexToken::EOF);
            }
        }
        match self.cont.as_bytes()[self.index] as char {
            '*' | '/' | '+' | '-' | '=' | '<' | '>' | '.' => {
                self.index += 1;
                if !(self.index >= self.cont.as_bytes().len()) {
                    if TWO_CHAR_OPS.contains(
                        &std::str::from_utf8(&self.cont.as_bytes()[self.index - 1..self.index + 1])
                            .unwrap(),
                    ) {
                        self.index += 1;
                        return Ok(LexToken::Op(LexValue::new_last(self, 2)));
                    }
                }
                Ok(LexToken::Op(LexValue::new_last_one(self)))
            }
            '0'..='9' => {
                let mut count = 0;
                loop {
                    let c = self.cont.as_bytes()[self.index] as char;
                    if !matches!(c, '0'..='9') || self.index >= self.cont.len() {
                        break;
                    }
                    count += 1;
                    self.index += 1;
                }
                Ok(LexToken::Number(LexValue::new_last(self, count)))
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut count = 0;
                loop {
                    if self.index >= self.cont.len() {
                        break;
                    }
                    let c = self.cont.as_bytes()[self.index] as char;
                    if !matches!(c, 'a'..='z'|'A'..='Z'|'1'..='9'|'_') {
                        break;
                    }
                    count += 1;
                    self.index += 1;
                }
                Ok(LexToken::Id(LexValue::new_last(self, count)))
            }
            '{' | '}' | '\'' | '"' | '(' | ')' | '[' | ']' | '!' | '&' => {
                self.index += 1;
                Ok(LexToken::Punc(LexValue::new_last_one(self)))
            }
            _ => {
                self.index += 1;
                Err(Error {
                    index: self.index - 1,
                    msg: format!(
                        "I don't know how to lex {}",
                        self.cont.as_bytes()[self.index - 1] as char
                    ),
                })
            }
        }
    }
}
