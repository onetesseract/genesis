use std::collections::HashMap;

use crate::{Error, lexer::{LexToken, LexValue, Lexer}};

#[derive(Debug)]
pub(crate) struct Function {
    pub(crate) name: LexValue,
    pub(crate) ty: Type,
    pub(crate) args_names: Vec<LexValue>,
    pub(crate) body: Option<Expr>
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionType {
    pub(crate) ret_type: Type,
    pub(crate) args: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr {
    Variable(LexValue),
    Declaration(Type, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Number(f64),
    Block(Vec<Expr>),
    /// name then the stuff in ( )
    Call(LexValue, Vec<Expr>),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BinaryOp {
    Add,
    Sub,
    Mul,

    Assign,
}

impl From<&str> for BinaryOp {
    fn from(s: &str) -> Self {
        match s {
            "=" => BinaryOp::Assign,
            "+" => BinaryOp::Add,
            "-" => BinaryOp::Sub,
            "*" => BinaryOp::Mul,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Type {
    I64,
    U8,
    Void,
    Function(Box<FunctionType>),
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
}

impl From<Error> for ParseError {
    fn from(e: Error) -> Self {
        ParseError::LexError(e)
    }
}

pub(crate) type ParseResult = Result<Expr, ParseError>;
impl Parser {
    pub(crate) fn new(lexer: Lexer, types: HashMap<String, Type>) -> Parser {
        let mut op_prec: HashMap<String, usize> = HashMap::new();
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
        Parser { lexer, types, op_prec }
    }

    fn parse_atom(&mut self) -> ParseResult {
        let l =  self.lexer.lex()?;
        match l {
            LexToken::Punc(p) => self.parse_punc(p),
            LexToken::Id(i) => self.parse_id(i, true),
            LexToken::Op(_o) => todo!(), // todo: boolean negation
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
                        if o.get().as_str() == "}" { break; }
                    }
                    exprs.push(self.parse()?);
                }
                self.lexer.lex()?;
                return Ok(Expr::Block(exprs));
            }
            _ => panic!("Unknown punctuation"),
        }
    }

    fn parse_call(&mut self, name: LexValue) -> ParseResult {
        self.lexer.lex()?; // skip the '('
        let mut exprs = vec![];
        loop {
            if let Ok(LexToken::Punc(o)) = self.lexer.clone().lex() {
                if o.get().as_str() == ")" { break; }
            }
            exprs.push(self.parse()?); // todo: should we do commas
        }
        self.lexer.lex()?;
        return Ok(Expr::Call(name, exprs))
    }

    fn parse_type(&mut self, i: LexValue) -> Result<(Type, Option<Vec<LexValue>>), ParseError> {
        let peeked = self.lexer.clone().lex()?;
        match peeked {
            LexToken::Punc(p) => {
                match p.get().as_str() {
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
                                    } else { panic!(); }
                                    args.push(ty);
                                },
                                _ => return Err(ParseError::NotAValidParameterType),
                            };
                        }
                        self.lexer.lex()?;
                        let fun = FunctionType { ret_type: self.types.get(&i.get()).unwrap().clone(), args };
                        let ty = Type::Function(Box::new(fun));
                        return Ok((ty, Some(names)));
                    }
                    _ => {},
                }
            }
            _ => {},
        }
        return Ok((self.types.get(&i.get()).unwrap().clone(), None));
    }

    /// oh god please no
    fn parse_id(&mut self, i: LexValue, parse_def: bool) -> ParseResult {
        let peeked = self.lexer.clone().lex()?;
        match peeked {
            LexToken::Punc(p) => {
                match p.get().as_str() {
                    "(" => return self.parse_call(i),
                    _ => return Ok(Expr::Variable(i)),
                }
            },
            LexToken::Id(id) => {
                if self.types.contains_key(&id.get()) && parse_def {
                    self.lexer.lex()?;
                    let (e, _) = self.parse_type(id)?;
                    let ret = Expr::Declaration(e, Box::new(Expr::Variable(i)));
                    return Ok(ret);
                }
                return Ok(Expr::Variable(i))
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
                    let binary = Expr::Binary(Box::new(e), BinaryOp::from(o.get().as_str()), Box::new(rhs));
                    return self.maybe_binary(binary, my_prec);
                }
            }
        }
        return Ok(e);
    }

    pub(crate) fn parse_toplevel(&mut self) -> Result<Function, ParseError> {
        let name = match self.lexer.lex()? {
            LexToken::Id(i) => i,
            _ => return Err(ParseError::ThisIsNotAString),
        };

        let ret_type = match self.lexer.lex()? {
            LexToken::Id(i) => i,
            _ => return Err(ParseError::ThisIsNotAString),
        };

        let (fn_type, args_names) = self.parse_type(ret_type)?;

        let mut body: Option<Expr>;

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
        let f = Function { name: name, ty: fn_type, args_names: args_names.unwrap(), body };
        return Ok(f);
    }

    pub(crate) fn parse(&mut self) -> ParseResult {
        let res = self.parse_atom()?;
        let res = self.maybe_binary(res, 0);
        return res;
    }
}