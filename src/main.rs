#![feature(once_cell)] // 1.53.0-nightly (2021-04-01 d474075a8f28ae9a410e)
use std::{lazy::SyncLazy, sync::Mutex};

use std::collections::HashMap;

use inkwell::context::Context;
use inkwell::passes::{PassManager, PassManagerSubType};
use lexer::Lexer;
use parser::Parser;

use crate::compiler::Compiler;

mod lexer;
mod parser;
// mod eval;
mod compiler;

extern crate neoncode;
extern crate inkwell;

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
    types.insert(String::from("void"), parser::Type::Void);
    types.insert(String::from("i64"), parser::Type::I64);
    types.insert(String::from("struct"), parser::Type::Struct);
    let file_cont = std::fs::read_to_string("ex.neon").unwrap();
    FILE_CONTENTS.lock().unwrap().insert(String::from("ex.neon"), file_cont.clone());
    let lexer = Lexer::new("ex.neon".to_string(), file_cont);
    let mut parser = Parser::new(lexer, types);
    let context = Context::create();
    let module = context.create_module("ex.neon");
    let fpm = PassManager::create(&module);
    let mut compiler = Compiler::new("ex.neon".to_string(), &context, module, fpm);
    let mut f = parser.parse_toplevel();
    loop {
        println!("f: {:?}", f);
        if (&f).is_err() { break; }
        match f.unwrap() {
            either::Either::Left(f) => println!("Compiled function: {:?}", compiler.compile_fn(f).unwrap()),
            either::Either::Right(s) => println!("Compiled struct type: {:?}", compiler.compile_struct_type(s).unwrap()),
        }
        f = parser.parse_toplevel();
    }
    println!("{:?}", compiler.dump_module());
    std::fs::write("neon.ll", compiler.dump_module()).unwrap();
    
}

