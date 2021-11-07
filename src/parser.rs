use crate::lexer::LexVal;
use either::Either;
use neoncode::expr::{BinaryOp, FunctionType, Struct};
pub use neoncode::expr::{LexValue, Type};
use std::collections::HashMap;
use Either::{Left, Right};

use crate::{
    lexer::{LexToken, Lexer},
    Error,
};

#[derive(Debug, Clone)]
pub(crate) struct Function {
    pub(crate) name: LexValue,
    pub(crate) ty: Type,
    pub(crate) args_names: Vec<LexValue>,
    pub(crate) body: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr {
    Variable(LexValue),
    Declaration(Type, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Number(f64),
    Block(Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Option<Expr>>),
    Negated(Box<Expr>),
    /// name then the stuff in ( )
    Call(LexValue, Vec<Expr>),
    Null,
}

pub(crate) struct Parser {
    lexer: Lexer,
    types: HashMap<String, Type>,
    op_prec: HashMap<String, usize>,
}

#[derive(Debug)]
pub(crate) enum ParseError {
    LexError(Error),
    CannotParseNumber(LexValue),
    ThisIsNotAString,
    NotAValidParameterType,
    EOF,
}

impl From<Error> for ParseError {
    fn from(e: Error) -> Self {
        ParseError::LexError(e)
    }
}

impl Expr {
    pub(crate) fn into_variable(&self) -> LexValue {
        if let Expr::Variable(v) = self {
            v.clone()
        } else {
            panic!()
        }
    }
}

pub(crate) type ParseResult = Result<Expr, ParseError>;
impl Parser {
    pub(crate) fn new(lexer: Lexer, types: HashMap<String, Type>) -> Parser {
        let mut op_prec: HashMap<String, usize> = HashMap::new();
        op_prec.insert(".".to_string(), 30);
        op_prec.insert("*".to_string(), 20);
        op_prec.insert("/".to_string(), 20);
        op_prec.insert("%".to_string(), 20);
        op_prec.insert("+".to_string(), 10);
        op_prec.insert("-".to_string(), 10);
        op_prec.insert("<".to_string(), 7);
        op_prec.insert(">".to_string(), 7);
        op_prec.insert("<=".to_string(), 7);
        op_prec.insert(">=".to_string(), 7);
        op_prec.insert("==".to_string(), 7);
        op_prec.insert("!=".to_string(), 7);
        op_prec.insert("&&".to_string(), 3);
        op_prec.insert("||".to_string(), 3); // todo: should && bind tighter than ||
        op_prec.insert("=".to_string(), 1);
        Parser {
            lexer,
            types,
            op_prec,
        }
    }

    fn parse_atom(&mut self) -> ParseResult {
        let l = self.lexer.lex()?;
        match l {
            LexToken::Punc(p) => self.parse_punc(p),
            LexToken::Id(i) => self.parse_id(i, true),
            LexToken::Op(o) => {
                if o.get() == "!" {
                    Ok(Expr::Negated(Box::new(self.parse_atom()?)))
                } else {
                    todo!()
                }
            } // todo: boolean negation
            LexToken::Number(n) => self.parse_number(n),
            LexToken::EOF => todo!(),
        }
    }

    fn parse_punc(&mut self, p: LexValue) -> ParseResult {
        match p.get().as_str() {
            "{" => {
                let mut exprs = vec![];
                loop {
                    if let Ok(LexToken::Punc(o)) = self.lexer.clone().lex() {
                        if o.get().as_str() == "}" {
                            break;
                        }
                    }
                    exprs.push(self.parse()?);
                }
                self.lexer.lex()?;
                return Ok(Expr::Block(exprs));
            } // todo: implement (_)
            "!" => {
                let negated = self.parse_atom()?; // todo: check
                Ok(Expr::Negated(Box::new(negated)))
            }
            p => panic!("Unknown punctuation {}", p),
        }
    }

    fn parse_if(&mut self) -> ParseResult {
        // self.lexer.lex(); // skip the if
        let cond = self.parse()?;
        let then = self.parse()?;
        let els = if let Ok(LexToken::Id(l)) = self.lexer.clone().lex() {
            if l.get() == "else" {
                self.lexer.lex()?;
                Some(self.parse()?)
            } else {
                None
            }
        } else {
            None
        };
        Ok(Expr::If(Box::new(cond), Box::new(then), Box::new(els)))
    }

    fn parse_call(&mut self, name: LexValue) -> ParseResult {
        self.lexer.lex()?; // skip the '('
        let mut exprs = vec![];
        loop {
            if let Ok(LexToken::Punc(o)) = self.lexer.clone().lex() {
                if o.get().as_str() == ")" {
                    break;
                }
            }
            exprs.push(self.parse()?); // todo: should we do commas
        }
        self.lexer.lex()?;
        return Ok(Expr::Call(name, exprs));
    }

    fn parse_type(&mut self, i: LexValue) -> Result<(Type, Option<Vec<LexValue>>), ParseError> {
        let peeked = self.lexer.clone().lex()?;
        match peeked {
            LexToken::Punc(p) => match p.get().as_str() {
                "(" => {
                    self.lexer.lex()?;
                    let mut args = vec![];
                    let mut names = vec![];
                    loop {
                        if let Ok(LexToken::Punc(o)) = self.lexer.clone().lex() {
                            if o.get().as_str() == ")" {
                                break;
                            }
                        }
                        match self.parse()? {
                            Expr::Declaration(ty, n) => {
                                if let Expr::Variable(n) = *n {
                                    names.push(n);
                                } else {
                                    panic!();
                                }
                                args.push(ty);
                            }
                            _ => return Err(ParseError::NotAValidParameterType),
                        };
                    }
                    self.lexer.lex()?;
                    let fun = FunctionType {
                        ret_type: self.types.get(&i.get()).unwrap().clone(),
                        args,
                    };
                    let ty = Type::Function(Box::new(fun));
                    return Ok((ty, Some(names)));
                }
                _ => {}
            },
            _ => {}
        }
        return Ok((self.types.get(&i.get()).unwrap().clone(), None));
    }

    /// oh god please no
    fn parse_id(&mut self, i: LexValue, parse_def: bool) -> ParseResult {
        match i.get().as_str() {
            "if" => return self.parse_if(),
            _ => {}
        }
        let peeked = self.lexer.clone().lex()?;
        match peeked {
            LexToken::Punc(p) => match p.get().as_str() {
                "(" => return self.parse_call(i),
                _ => return Ok(Expr::Variable(i)),
            },
            LexToken::Id(id) => {
                if self.types.contains_key(&id.get()) && parse_def {
                    self.lexer.lex()?;
                    let (e, _) = self.parse_type(id)?;
                    let ret = Expr::Declaration(e, Box::new(Expr::Variable(i)));
                    return Ok(ret);
                }
                return Ok(Expr::Variable(i));
            }
            _ => return Ok(Expr::Variable(i)),
        }
    }

    fn parse_number(&mut self, n: LexValue) -> ParseResult {
        let s = n.get();
        let num = s.parse::<f64>();
        let num = match num {
            Ok(num) => num,
            Err(_) => return Err(ParseError::CannotParseNumber(n)),
        };
        return Ok(Expr::Number(num));
    }

    fn maybe_binary(&mut self, e: Expr, my_prec: usize) -> ParseResult {
        let peeked = self.lexer.clone().lex()?;
        if let LexToken::Op(o) = peeked {
            if self.op_prec.contains_key(&o.get()) {
                if *self.op_prec.get(&o.get()).unwrap_or(&0) > my_prec {
                    self.lexer.lex()?;
                    let atom = self.parse_atom()?;
                    let rhs = self.maybe_binary(atom, *self.op_prec.get(&o.get()).unwrap_or(&0))?;
                    let binary =
                        Expr::Binary(Box::new(e), BinaryOp::from(o.get().as_str()), Box::new(rhs));
                    return self.maybe_binary(binary, my_prec);
                }
            }
        }
        return Ok(e);
    }

    pub(crate) fn parse_struct(&mut self) -> Result<Struct, ParseError> {
        match self.parse()? {
            Expr::Binary(name, BinaryOp::Assign, val) => {
                let name = match *name {
                    Expr::Declaration(_, name) => name.into_variable(),
                    _ => panic!(),
                };
                let vals = match *val {
                    Expr::Declaration(t, v) => {
                        let mut h = HashMap::new();
                        h.insert(v.into_variable().get(), (0, t));
                        h
                    }
                    Expr::Block(b) => {
                        let mut h = HashMap::new();
                        let mut count = 0;
                        for i in b {
                            if let Expr::Declaration(t, name) = i {
                                h.insert(name.into_variable().get(), (count, t));
                            } else {
                                panic!()
                            }
                            count += 1;
                        }
                        h
                    }
                    _ => panic!(),
                };
                let s = Struct {
                    name: name.clone(),
                    vals,
                };
                self.types.insert(name.get(), Type::StructType(s.clone()));
                return Ok(s);
            }
            x => {
                println!("aa: {:?}", x);
                panic!()
            }
        }
    }

    pub(crate) fn parse_toplevel(&mut self) -> Result<Either<Function, Struct>, ParseError> {
        let mut l = self.lexer.clone();
        match l.lex() {
            // stops must_use yeling
            _ => {}
        }
        if let Ok(v) = l.lex() {
            if matches!(v, LexToken::EOF) {
                return Err(ParseError::EOF);
            }
            println!("l: {:?}", v);
            if v.get_val().get().as_str() == "struct" {
                return Ok(Right(self.parse_struct()?));
            }
        }

        let name = match self.lexer.lex()? {
            LexToken::Id(i) => i,
            LexToken::EOF => return Err(ParseError::EOF),
            _ => return Err(ParseError::ThisIsNotAString),
        };

        if name.get().as_str() == "struct" {
            panic!();
            // return Ok(Either::Right(self.parse_struct()?));
        }

        let ret_type = match self.lexer.lex()? {
            LexToken::Id(i) => i,
            _ => return Err(ParseError::ThisIsNotAString),
        };

        let (fn_type, args_names) = self.parse_type(ret_type)?;

        let body: Option<Expr>;

        let e = Expr::Null;
        let e = self.maybe_binary(e, 0)?;
        if e == Expr::Null {
            body = None;
        } else if let Expr::Binary(_, op, rhs) = e {
            if op != BinaryOp::Assign {
                panic!();
            }
            body = Some(*rhs);
        } else {
            panic!();
        }
        let f = Function {
            name: name,
            ty: fn_type,
            args_names: args_names.unwrap(),
            body,
        };
        return Ok(Left(f));
    }

    pub(crate) fn parse(&mut self) -> ParseResult {
        let res = self.parse_atom()?;
        let res = self.maybe_binary(res, 0);
        return res;
    }
}
