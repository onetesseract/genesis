use std::collections::HashMap;

use crate::parser::Expr;

#[derive(Clone)]
pub struct Variable {
    data: Vec<u8>,
}

pub(crate) fn eval(e: Expr, vars: &mut HashMap<String, Variable>) -> Variable {
    match e {
        Expr::Variable(v) => vars.get(&v.get()).unwrap().clone(),
        Expr::Declaration(_, _) => todo!(),
        Expr::Binary(lhs, op, rhs) => {
            let l = eval(*lhs, vars);
            let r = eval(*rhs, vars);
            match op {
                crate::parser::BinaryOp::Add => {
                    let mut data: [u8; 8] = [0, 0, 0, 0, 0, 0, 0, 0];
                    let mut data_count = 0;
                    for i in l.data {
                        data[data_count] = i;
                        data_count += 1;
                    }
                    let l_num = usize::from_be_bytes(data);
                    data = [0, 0, 0, 0, 0, 0, 0, 0];
                    data_count = 0;
                    for i in r.data {
                        data[data_count] = i;
                        data_count += 1;
                    }
                    let r_num = usize::from_be_bytes(data);
                    panic!();
                }
                crate::parser::BinaryOp::Sub => todo!(),
                crate::parser::BinaryOp::Mul => todo!(),
                crate::parser::BinaryOp::Assign => todo!(),
            }
        }
        Expr::Number(_) => todo!(),
        Expr::Block(b) => {
            let mut v = Variable { data: vec![] };
            for i in b {
                v = eval(i, vars);
            }
            v
        }
        Expr::Call(_, _) => todo!(),
    }
}
