#![feature(once_cell)] // 1.53.0-nightly (2021-04-01 d474075a8f28ae9a410e)
use std::{lazy::SyncLazy, sync::Mutex};

use std::collections::HashMap;

use lexer::Lexer;
use parser::Parser;

mod lexer;
mod parser;
mod eval;



extern crate neoncode;

#[derive(Debug, Clone)]
pub(crate) struct Error {
    msg: String,
    index: usize,
}

pub(crate) type Result<T> = std::result::Result<T, Error>;

static FILE_CONTENTS: SyncLazy<Mutex<HashMap<String, String>>> = SyncLazy::new(|| Mutex::new(HashMap::new()));

fn main() {
    let mut types = HashMap::new();
    types.insert(String::from("u8"), parser::Type::U8);
    let file_cont = std::fs::read_to_string("ex.neon").unwrap();
    FILE_CONTENTS.lock().unwrap().insert(String::from("ex.neon"), file_cont.clone());
    let lexer = Lexer::new("ex.neon".to_string(), file_cont);
    let mut parser = Parser::new(lexer, types);
    println!("{:?}", parser.parse());
    
}

