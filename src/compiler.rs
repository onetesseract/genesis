use std::{collections::HashMap};
pub(crate) type Result<T> = std::result::Result<T, String>;
use inkwell::{self, builder::Builder, context::Context, module::Module, passes::PassManager, types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, VoidType}, values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue}};
use crate::parser::{self, Expr, Function, Type};

macro_rules! gimme {
    ($x:expr) => {
        match $x {
            Ok(e) => e,
            Err(e) => return Err(e),
        };
    };
}

macro_rules! gimme_opt {
    ($x:expr, $y:expr) => {
        match $x {
            Some(x) => x,
            None => return Err($y),
        }
    };
}

pub(crate) struct Compiler<'ctx> {
    builder: Builder<'ctx>,
    context: &'ctx Context,
    module: Module<'ctx>,
    fpm: PassManager<FunctionValue<'ctx>>,
    variables: HashMap<String, PointerValue<'ctx>>,
    fun: Option<Function>,
    fn_val: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    pub(crate) fn dump_module(&self) -> String{
        self.module.print_to_string().to_string()
    }

    pub(crate) fn new(name: String, c: &'ctx Context, module: Module<'ctx>, fpm: PassManager<FunctionValue<'ctx>>) -> Compiler<'ctx> {
        let builder = c.create_builder();
        let c = Compiler { builder, context: c, module, variables: HashMap::new(), fun: None, fn_val: None, fpm };
        return c;
    }

    fn entry_block_alloca(&self, name: String, ty: BasicTypeEnum<'ctx>) -> Result<PointerValue<'ctx>> {
        let b = self.context.create_builder();
        let entry_block = self.fn_val.unwrap().get_first_basic_block().unwrap();
        match entry_block.get_first_instruction() {
            Some(i) => b.position_before(&i),
            None => b.position_at_end(entry_block),
        }
        Ok(b.build_alloca(ty, &name))
    }

    fn build_add(&self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> Result<Option<BasicValueEnum<'ctx>>> {
        match &lhs.get_type() {
            BasicTypeEnum::ArrayType(_) => todo!(),
            BasicTypeEnum::FloatType(_) => Ok(Some(self.builder.build_float_add(lhs.into_float_value(), rhs.into_float_value(), "tmpfloatadd").as_basic_value_enum())),
            BasicTypeEnum::IntType(_) => Ok(Some(self.builder.build_int_add(lhs.into_int_value(), rhs.into_int_value(), "tmpintadd").as_basic_value_enum())),
            BasicTypeEnum::PointerType(p) => {
                let l = self.builder.build_load(lhs.into_pointer_value(), "tmpaddloadlhs");
                let r = self.builder.build_load(rhs.into_pointer_value(), "tmpaddloadrhs");
                self.build_add(l, r)
            },
            BasicTypeEnum::StructType(_) => todo!(),
            BasicTypeEnum::VectorType(_) => todo!(),
        }
    }

    fn build_mul(&self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> Result<Option<BasicValueEnum<'ctx>>> {
        match &lhs.get_type() {
            BasicTypeEnum::ArrayType(_) => todo!(),
            BasicTypeEnum::FloatType(_) => Ok(Some(self.builder.build_float_mul(lhs.into_float_value(), rhs.into_float_value(), "tmpfloatmul").as_basic_value_enum())),
            BasicTypeEnum::IntType(_) => Ok(Some(self.builder.build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "tmpintmul").as_basic_value_enum())),
            BasicTypeEnum::PointerType(_) => todo!(),
            BasicTypeEnum::StructType(_) => todo!(),
            BasicTypeEnum::VectorType(_) => todo!(),
        }
    }

    fn build_sub(&self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> Result<Option<BasicValueEnum<'ctx>>> {
        match &lhs.get_type() {
            BasicTypeEnum::ArrayType(_) => todo!(),
            BasicTypeEnum::FloatType(_) => Ok(Some(self.builder.build_float_sub(lhs.into_float_value(), rhs.into_float_value(), "tmpfloatsub").as_basic_value_enum())),
            BasicTypeEnum::IntType(_) => Ok(Some(self.builder.build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "tmpintsub").as_basic_value_enum())),
            BasicTypeEnum::PointerType(_) => todo!(),
            BasicTypeEnum::StructType(_) => todo!(),
            BasicTypeEnum::VectorType(_) => todo!(),
        }
    }

    fn build_div(&self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> Result<Option<BasicValueEnum<'ctx>>> {
        match &lhs.get_type() {
            BasicTypeEnum::ArrayType(_) => todo!(),
            BasicTypeEnum::FloatType(_) => Ok(Some(self.builder.build_float_div(lhs.into_float_value(), rhs.into_float_value(), "tmpfloatdiv").as_basic_value_enum())),
            BasicTypeEnum::IntType(_) => Ok(Some(self.builder.build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), "tmpintdiv").as_basic_value_enum())),
            BasicTypeEnum::PointerType(_) => todo!(),
            BasicTypeEnum::StructType(_) => todo!(),
            BasicTypeEnum::VectorType(_) => todo!(),
        }
    }

    fn build_eq_comp(&self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> Result<Option<BasicValueEnum<'ctx>>> {
        match &lhs.get_type() {
            BasicTypeEnum::ArrayType(_) => todo!(),
            BasicTypeEnum::FloatType(_) => Ok(Some(self.builder.build_float_compare(inkwell::FloatPredicate::OEQ, lhs.into_float_value(), rhs.into_float_value(), "tmpfloatcmp").as_basic_value_enum())),
            BasicTypeEnum::IntType(_) => Ok(Some(self.builder.build_int_compare(inkwell::IntPredicate::EQ, lhs.into_int_value(), rhs.into_int_value(), "tmpintdiv").as_basic_value_enum())),
            BasicTypeEnum::PointerType(_) => todo!(),
            BasicTypeEnum::StructType(_) => todo!(),
            BasicTypeEnum::VectorType(_) => todo!(),
        }
    }


    fn build_assign(&self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> Result<Option<BasicValueEnum<'ctx>>> {
        self.builder.build_store(lhs.into_pointer_value(), rhs);
        Ok(None)
    }



    fn compile(&mut self, e: Expr) -> Result<Option<BasicValueEnum<'ctx>>> {
        match e {
            Expr::Binary(l, op, r) => {
                let lhs = gimme_opt!(gimme!(self.compile(*l)), String::from("void"));
                let rhs = gimme_opt!(gimme!(self.compile(*r)), String::from("void"));
                match op {
                    parser::BinaryOp::Assign => self.build_assign(lhs, rhs),
                    parser::BinaryOp::Add => self.build_add(lhs, rhs),
                    parser::BinaryOp::Sub => self.build_sub(lhs, rhs),
                    parser::BinaryOp::Mul => self.build_mul(lhs, rhs),
                    /* crate::parser::BinaryOp::Div => self.build_div(lhs, rhs),
                    crate::parser::BinaryOp::Equal => self.build_eq_comp(lhs, rhs),
                    crate::parser::BinaryOp::NEqual => todo!(),
                    crate::parser::BinaryOp::Less => todo!(),
                    crate::parser::BinaryOp::More => todo!(),
                    crate::parser::BinaryOp::LessEqual => todo!(),
                    crate::parser::BinaryOp::MoreEqual => todo!(), */
                }
            },
            Expr::Variable(v) => {
                let val = self.variables.get(&v.get());
                match val {
                    Some(x) => {Ok(Some(x.as_basic_value_enum()))},
                    None => Err(format!("Cannot find variable {}", v.get())),
                }
            },
            /*
            Expr::If(i) => { // TODO: optimise so bools actually exist
                let cond = gimme_opt!(gimme!(self.compile(i.cond)), format!("Cannot use {:?} as boolean", i.cond));
                let comp: IntValue = match cond {
                    BasicValueEnum::ArrayValue(_) => todo!(),
                    BasicValueEnum::IntValue(i) => { self.builder.build_int_compare(inkwell::IntPredicate::NE, i, self.context.i8_type().const_zero(), "tmpintcmp")},
                    BasicValueEnum::FloatValue(_) => todo!(),
                    BasicValueEnum::PointerValue(_) => todo!(),
                    BasicValueEnum::StructValue(_) => todo!(),
                    BasicValueEnum::VectorValue(_) => todo!(),
                };
                let parent = gimme_opt!(self.fn_val, String::from("this shouldnt happen"));
                let then_bb = self.context.append_basic_block(parent, "then_branch");
                let else_bb = self.context.append_basic_block(parent, "else_branch");
                let cont_bb = self.context.append_basic_block(parent, "cont_branch");
                let branch = self.builder.build_conditional_branch(comp, then_bb, else_bb);

                self.builder.position_at_end(then_bb);
                let then_val = gimme!(self.compile(i.then));
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(else_bb);
                let else_val: Option<BasicValueEnum<'ctx>> = if let Some(x) = i.els {
                    gimme!(self.compile(x))
                } else { None };
                self.builder.build_unconditional_branch(cont_bb);

                if let Some(t) = then_val {
                    if let Some(e) = else_val {
                        if t.get_type() == e.get_type() {
                            let phi = self.builder.build_phi(t.get_type(), "tmpifphi");
                            phi.add_incoming(&[
                                (&t, then_bb),
                                (&e, else_bb),
                            ]);
                            return Ok(Some(phi.as_basic_value()));
                        } else {
                            return Err(format!("Then-val not the same as else-val"));
                        }
                    }
                }

                Ok(None)
            },
            */
            Expr::Number(n) => {Ok(Some(self.context.i64_type().const_int(n as u64, false).as_basic_value_enum()))},
            Expr::Block(b) => {
                for i in b {
                    let c = self.compile(i);
                    match c {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    }
                }
                Ok(None)
            },
            Expr::Declaration(ty, e) => {
                let name = if let Expr::Variable(n) = *e { n } else { panic!(); };
                let ty = match ty {
                    Type::I64 => self.context.i64_type().as_basic_type_enum(),
                    Type::U8 => self.context.i8_type().as_basic_type_enum(),
                    _ => todo!(),
                    // Type::Struct(_) => todo!(),
                };
                let m = self.entry_block_alloca(name.get(), ty);
                let m = gimme!(m);
                self.variables.insert(name.get(), m);
                Ok(None)
            },
            Expr::Call(n, params) => {
                let fun = self.module.get_function(&n.get());
                let fun = gimme_opt!(fun, format!("Unknown function {}", n.get()));
                let mut args = vec![];
                for i in params {
                    args.push(gimme_opt!(gimme!(self.compile(i)), String::from("Cannot use as a non-void")));
                }
                let c = self.builder.build_call(fun, args.as_slice(), "tmpcall").try_as_basic_value().left();
                Ok(c)
            },
            Expr::Null => panic!(),
        }
    }

    fn ty(&self, ty: Type) -> Option<BasicTypeEnum> {
        match ty {
            Type::I64 => Some(self.context.i64_type().as_basic_type_enum()),
            Type::U8 => Some(self.context.i8_type().as_basic_type_enum()),
            Type::Void => None,
            _ => todo!(),
        }
    }

    pub(crate) fn compile_fn(&mut self, f: Function) -> Result<FunctionValue> {
        let fn_ty = if let Type::Function(t) = f.ty {
            let fn_ret_ty = match t.ret_type {
                Type::I64 => Some(self.context.i64_type().as_basic_type_enum()),
                Type::U8 => Some(self.context.i8_type().as_basic_type_enum()),
                Type::Void => None,
                _ => todo!(),
            };
            let mut arg_types = vec![];
            for i in t.args {
                arg_types.push( match i {
                    Type::I64 => self.context.i64_type().as_basic_type_enum(),
                    Type::U8 => self.context.i8_type().as_basic_type_enum(),
                    _ => todo!(),
                } );
            }
            if let Some(ret_ty) = fn_ret_ty {
                ret_ty.fn_type(arg_types.as_slice(), false)
            } else {
                self.context.void_type().fn_type(arg_types.as_slice(), false)
            }
        } else {
            ( match f.ty {
                Type::I64 => self.context.i64_type().as_basic_type_enum(),
                Type::U8 => self.context.i8_type().as_basic_type_enum(),
                _ => todo!(),
            } ).fn_type(&[], false)
        };

        let fn_val = self.module.add_function( &f.name.get(), fn_ty, None);

        if f.body.is_none() {
            return Ok(fn_val);
        }

        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        self.fn_val = Some(fn_val);
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            let arg_name = f.args_names[i].get();
            let alloca = self.entry_block_alloca(arg_name.clone(), arg.get_type())?;
            self.builder.build_store(alloca, arg);
            self.variables.insert(arg_name, alloca);
        }

        let body = self.compile(f.body.unwrap())?;
        if body.is_some() {
            self.builder.build_return(Some(&body.unwrap()));
        } else {
            self.builder.build_return(None);
        }

        if fn_val.verify(true) {
            self.fpm.run_on(&fn_val);
            return Ok(fn_val);
        }
        unsafe { fn_val.delete(); }
        return Err(format!("bad function lol"));
    }
}