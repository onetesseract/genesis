use std::{collections::HashMap, env::var};
pub(crate) type Result<T> = std::result::Result<T, String>;
use crate::lexer::LexVal;
use crate::parser::{Expr, Function, Type};
use inkwell::{
    self,
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    types::{AnyType, BasicType, BasicTypeEnum, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue},
    FloatPredicate, IntPredicate,
};
use neoncode::expr::{BinaryOp, Struct};

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
    is_assign: bool,
    fn_val: Option<FunctionValue<'ctx>>,

    structs: HashMap<String, (Struct, StructType<'ctx>)>,
}

impl<'ctx> Compiler<'ctx> {
    pub(crate) fn dump_module(&self) -> String {
        self.module.print_to_string().to_string()
    }

    pub(crate) fn new(
        name: String,
        c: &'ctx Context,
        module: Module<'ctx>,
        fpm: PassManager<FunctionValue<'ctx>>,
    ) -> Compiler<'ctx> {
        let builder = c.create_builder();
        let c = Compiler {
            builder,
            context: c,
            module,
            variables: HashMap::new(),
            fun: None,
            fn_val: None,
            is_assign: false,
            fpm,
            structs: HashMap::new(),
        };
        return c;
    }

    fn entry_block_alloca(
        &self,
        name: String,
        ty: BasicTypeEnum<'ctx>,
    ) -> Result<PointerValue<'ctx>> {
        let b = self.context.create_builder();
        let entry_block = self.fn_val.unwrap().get_first_basic_block().unwrap();
        match entry_block.get_first_instruction() {
            Some(i) => b.position_before(&i),
            None => b.position_at_end(entry_block),
        }
        Ok(b.build_alloca(ty, &name))
    }

    fn build_add(
        &self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        match &lhs.get_type() {
            BasicTypeEnum::ArrayType(_) => todo!(),
            BasicTypeEnum::FloatType(_) => Ok(Some(
                self.builder
                    .build_float_add(
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "tmpfloatadd",
                    )
                    .as_basic_value_enum(),
            )),
            BasicTypeEnum::IntType(_) => Ok(Some(
                self.builder
                    .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "tmpintadd")
                    .as_basic_value_enum(),
            )),
            BasicTypeEnum::PointerType(p) => {
                let l = self
                    .builder
                    .build_load(lhs.into_pointer_value(), "tmpaddloadlhs");
                let r = self
                    .builder
                    .build_load(rhs.into_pointer_value(), "tmpaddloadrhs");
                self.build_add(l, r)
            }
            BasicTypeEnum::StructType(_) => todo!(),
            BasicTypeEnum::VectorType(_) => todo!(),
        }
    }

    fn build_mul(
        &self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        match &lhs.get_type() {
            BasicTypeEnum::ArrayType(_) => todo!(),
            BasicTypeEnum::FloatType(_) => Ok(Some(
                self.builder
                    .build_float_mul(
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "tmpfloatmul",
                    )
                    .as_basic_value_enum(),
            )),
            BasicTypeEnum::IntType(_) => Ok(Some(
                self.builder
                    .build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "tmpintmul")
                    .as_basic_value_enum(),
            )),
            BasicTypeEnum::PointerType(_) => todo!(),
            BasicTypeEnum::StructType(_) => todo!(),
            BasicTypeEnum::VectorType(_) => todo!(),
        }
    }

    fn build_sub(
        &self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        match &lhs.get_type() {
            BasicTypeEnum::ArrayType(_) => todo!(),
            BasicTypeEnum::FloatType(_) => Ok(Some(
                self.builder
                    .build_float_sub(
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "tmpfloatsub",
                    )
                    .as_basic_value_enum(),
            )),
            BasicTypeEnum::IntType(_) => Ok(Some(
                self.builder
                    .build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "tmpintsub")
                    .as_basic_value_enum(),
            )),
            BasicTypeEnum::PointerType(_) => todo!(),
            BasicTypeEnum::StructType(_) => todo!(),
            BasicTypeEnum::VectorType(_) => todo!(),
        }
    }

    fn build_div(
        &self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        match &lhs.get_type() {
            BasicTypeEnum::ArrayType(_) => todo!(),
            BasicTypeEnum::FloatType(_) => Ok(Some(
                self.builder
                    .build_float_div(
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "tmpfloatdiv",
                    )
                    .as_basic_value_enum(),
            )),
            BasicTypeEnum::IntType(_) => Ok(Some(
                self.builder
                    .build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), "tmpintdiv")
                    .as_basic_value_enum(),
            )),
            BasicTypeEnum::PointerType(_) => todo!(),
            BasicTypeEnum::StructType(_) => todo!(),
            BasicTypeEnum::VectorType(_) => todo!(),
        }
    }

    fn build_eq_comp(
        &self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        match &lhs.get_type() {
            BasicTypeEnum::ArrayType(_) => todo!(),
            BasicTypeEnum::FloatType(_) => Ok(Some(
                self.builder
                    .build_float_compare(
                        inkwell::FloatPredicate::OEQ,
                        lhs.into_float_value(),
                        rhs.into_float_value(),
                        "tmpfloatcmp",
                    )
                    .as_basic_value_enum(),
            )),
            BasicTypeEnum::IntType(_) => Ok(Some(
                self.builder
                    .build_int_compare(
                        inkwell::IntPredicate::EQ,
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "tmpintdiv",
                    )
                    .as_basic_value_enum(),
            )),
            BasicTypeEnum::PointerType(_) => todo!(),
            BasicTypeEnum::StructType(_) => todo!(),
            BasicTypeEnum::VectorType(_) => todo!(),
        }
    }

    fn build_assign(
        &self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        self.builder.build_store(lhs.into_pointer_value(), rhs);
        Ok(None)
    }

    fn build_struct_access(
        &mut self,
        lhs: Expr,
        rhs: Expr,
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        let mut lhs = self.compile(lhs)?.unwrap();
        match rhs {
            Expr::Variable(v) => {
                let mut struct_ty = None;
                let mut struct_name = None;
                let lhs = if !lhs.is_pointer_value() {
                    let alloca = self
                        .entry_block_alloca("tmpstructaccessstorage".to_string(), lhs.get_type())?;
                    self.builder.build_store(alloca, lhs);
                    alloca
                } else {
                    lhs.into_pointer_value()
                };
                for (name, (s, st)) in &self.structs {
                    println!("sts: {:?} {:?}", st, lhs.get_type());
                    if st.as_any_type_enum() == lhs.get_type().get_element_type() {
                        struct_ty = Some(s);
                        struct_name = Some(name);
                    }
                }
                println!("Struct ty: {:?}", struct_ty);
                let struct_ty = struct_ty.unwrap();
                let mut var_index = None;
                for (name, (index, ty)) in &struct_ty.vals {
                    if *name == v.get() {
                        var_index = Some(*index);
                    }
                }
                let var_index = match var_index {
                    Some(v) => v,
                    None => {
                        return Err(format!(
                            "I can't find member {} in struct {}",
                            v.get(),
                            struct_name.unwrap()
                        ))
                    }
                };
                return Ok(Some(
                    self.builder
                        .build_struct_gep(
                            lhs,
                            std::convert::TryInto::try_into(var_index).unwrap(),
                            "tmpstructgep",
                        )
                        .unwrap()
                        .as_basic_value_enum(),
                ));
            }
            Expr::Call(_, _) => todo!(),
            _ => panic!(),
        }
    }

    fn compile(&mut self, e: Expr) -> Result<Option<BasicValueEnum<'ctx>>> {
        match e {
            Expr::Binary(l, op, r) => {
                if let BinaryOp::StructAccess = op {
                    return self.build_struct_access(*l, *r);
                }
                if let BinaryOp::Assign = op {
                    self.is_assign = true;
                }
                let lhs = gimme_opt!(gimme!(self.compile(*l)), String::from("void"));
                self.is_assign = false;
                let rhs = gimme_opt!(gimme!(self.compile(*r)), String::from("void"));
                match op {
                    BinaryOp::Assign => self.build_assign(lhs, rhs),
                    BinaryOp::Add => self.build_add(lhs, rhs),
                    BinaryOp::Sub => self.build_sub(lhs, rhs),
                    BinaryOp::Mul => self.build_mul(lhs, rhs),
                    BinaryOp::Eq => Ok(Some(
                        self.build_cmp_eq(&lhs, &Some(rhs)).as_basic_value_enum(),
                    )),
                    BinaryOp::StructAccess => panic!(),
                    /* crate::parser::BinaryOp::Div => self.build_div(lhs, rhs),
                    crate::parser::BinaryOp::Equal => self.build_eq_comp(lhs, rhs),
                    crate::parser::BinaryOp::NEqual => todo!(),
                    crate::parser::BinaryOp::Less => todo!(),
                    crate::parser::BinaryOp::More => todo!(),
                    crate::parser::BinaryOp::LessEqual => todo!(),
                    crate::parser::BinaryOp::MoreEqual => todo!(), */
                }
            }
            Expr::Variable(v) => {
                let val = self.variables.get(&v.get());
                match val {
                    Some(x) => {
                        if self.is_assign {
                            Ok(Some(x.as_basic_value_enum()))
                        } else {
                            Ok(Some(self.builder.build_load(*x, "tmpvariableload")))
                        }
                    }
                    None => Err(format!("Cannot find variable {}", v.get())),
                }
            }
            Expr::If(cond, then, els) => {
                // TODO: optimise so bools actually exist
                let cond = self.compile(*cond)?.unwrap(); // format!("Cannot use {:?} as boolean", i.cond));
                let comp = cond.into_int_value(); //cond.into_int_value();
                                                  // let cmp = self.build_eq_comp(comp.as_basic_value_enum(), self.context.i8_type().const_int(1, false).as_basic_value_enum())?.unwrap();
                let parent = self.fn_val.unwrap();
                let then_bb = self.context.append_basic_block(parent, "then_branch");
                let else_bb = self.context.append_basic_block(parent, "else_branch");
                let cont_bb = self.context.append_basic_block(parent, "cont_branch");
                let _branch = self
                    .builder
                    .build_conditional_branch(comp, then_bb, else_bb);

                self.builder.position_at_end(then_bb);
                let then_val = self.compile(*then)?;
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(else_bb);
                let else_val: Option<BasicValueEnum<'ctx>> = if let Some(x) = *els {
                    self.compile(x)?
                } else {
                    None
                };
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(cont_bb);

                if let Some(t) = then_val {
                    if let Some(e) = else_val {
                        if t.get_type() == e.get_type() {
                            let phi = self.builder.build_phi(t.get_type(), "tmpifphi");
                            phi.add_incoming(&[(&t, then_bb), (&e, else_bb)]);
                            return Ok(Some(phi.as_basic_value()));
                        } else {
                            return Err(format!("Then-val not the same as else-val"));
                        }
                    }
                }

                Ok(None)
            }
            Expr::Number(n) => Ok(Some(
                self.context
                    .i64_type()
                    .const_int(n as u64, false)
                    .as_basic_value_enum(),
            )),
            Expr::Block(b) => {
                let mut c = None;
                let mut last_expr = None;
                for i in b {
                    let t = self.compile(i.clone());
                    c = match t {
                        Ok(t) => t,
                        Err(e) => return Err(e),
                    };
                    last_expr = Some(i)
                }
                if let Some(BasicValueEnum::PointerValue(p)) = c {
                    if let Some(Expr::Variable(_)) = last_expr {
                        c = Some(self.builder.build_load(p, "tmpblockendload"))
                    }
                }
                Ok(c)
            }
            Expr::Declaration(ty, e) => {
                let name = if let Expr::Variable(n) = *e {
                    n
                } else {
                    panic!();
                };
                let ty = match ty {
                    Type::I64 => self.context.i64_type().as_basic_type_enum(),
                    Type::U8 => self.context.i8_type().as_basic_type_enum(),
                    Type::StructType(s) => self
                        .structs
                        .get(&s.name.get())
                        .unwrap()
                        .1
                        .as_basic_type_enum(),
                    _ => todo!(),
                    // Type::Struct(_) => todo!(),
                };
                let m = self.entry_block_alloca(name.get(), ty);
                let m = gimme!(m);
                self.variables.insert(name.get(), m);
                Ok(None)
            }
            Expr::Call(n, params) => {
                let fun = self.module.get_function(&n.get());
                let fun = gimme_opt!(fun, format!("Unknown function {}", n.get()));
                let mut args = vec![];
                for i in params {
                    // args.push(gimme_opt!(gimme!(self.compile(i)), String::from("Cannot use as a non-void")));
                    let mut s = self.compile(i.clone())?.unwrap();
                    // if s.is_pointer_value() && (matches!(i, Expr::Variable(_)) || matches!(i, Expr::Call(..)) || matches!(i, Expr::Binary(_, BinaryOp::StructAccess, _))) {
                    //     s = self.builder.build_load(s.into_pointer_value(), "tmpcallparamderef");
                    // }
                    args.push(s)
                }
                let c = self
                    .builder
                    .build_call(fun, args.as_slice(), "tmpcall")
                    .try_as_basic_value()
                    .left();
                Ok(c)
            }
            Expr::Null => panic!(),
            Expr::Negated(e) => {
                let e = self.compile(*e)?.unwrap().into_int_value();
                let cmp = self.builder.build_int_compare(IntPredicate::NE, e, e.get_type().const_zero(), "tmpnegationcmp");
                let xor = self.builder.build_xor(cmp, cmp.get_type().const_all_ones(), "tmpnegationxor");
                Ok(Some(xor.as_basic_value_enum()))
            },
            /*
            Expr::If(cond, then, els) => {
                let parent = self.fn_val.unwrap();
                let cond = self.compile(*cond)?.unwrap().into_int_value();

                let then_bb = self.context.append_basic_block(parent, "then");
                let cont_bb = self.context.append_basic_block(parent, "ifcont");
                todo!();
            },
            */
        }
    }

    /// Comparses LHS to RHS. If RHS not present, copmpares LHS with (not?) zero.
    #[inline]
    fn build_cmp_eq(
        &self,
        lhs: &BasicValueEnum<'ctx>,
        rhs: &Option<BasicValueEnum<'ctx>>,
    ) -> IntValue<'ctx> {
        match lhs {
            BasicValueEnum::ArrayValue(_) => todo!(),
            BasicValueEnum::IntValue(lhs) => {
                if let Some(rhs) = rhs {
                    self.builder.build_int_compare(
                        IntPredicate::EQ,
                        *lhs,
                        rhs.into_int_value(),
                        "tmpintcmp",
                    )
                } else {
                    self.builder.build_int_compare(
                        IntPredicate::NE,
                        *lhs,
                        self.context.i64_type().const_zero(),
                        "tmpintzerocmp",
                    )
                }
            }
            BasicValueEnum::FloatValue(lhs) => {
                if let Some(rhs) = rhs {
                    self.builder.build_float_compare(
                        FloatPredicate::OEQ,
                        *lhs,
                        rhs.into_float_value(),
                        "tmpfloatcmp",
                    )
                } else {
                    self.builder.build_float_compare(
                        FloatPredicate::ONE,
                        *lhs,
                        self.context.f64_type().const_zero(),
                        "tmpfloatzerocmp",
                    )
                }
            }
            BasicValueEnum::PointerValue(p) => {
                self.build_cmp_eq(&self.builder.build_load(*p, "tmpcmpload"), rhs)
            }
            BasicValueEnum::StructValue(_) => todo!(),
            BasicValueEnum::VectorValue(_) => todo!(),
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

    pub(crate) fn compile_struct_type(&mut self, s: Struct) -> Result<StructType> {
        let mut fields: Vec<BasicTypeEnum> = vec![]; //.reserve(s.vals.len());
        for _ in &s.vals {
            fields.push(self.context.i8_type().as_basic_type_enum()); // todo: ther has to be a better way than this etc etc
        }
        for (_, (c, t)) in &s.vals {
            fields[*c] = match t {
                Type::I64 => self.context.i64_type().as_basic_type_enum(),
                Type::U8 => self.context.i8_type().as_basic_type_enum(),
                _ => todo!(),
            };
        }
        let st = self.context.struct_type(fields.as_slice(), false); // todo: implement packing of structs
        self.structs.insert(s.name.get(), (s, st));
        Ok(st)
    }

    pub(crate) fn compile_fn(&mut self, f: Function) -> Result<FunctionValue> {
        let fn_ty = if let Type::Function(t) = f.ty {
            let fn_ret_ty = match t.ret_type {
                Type::I64 => Some(self.context.i64_type().as_basic_type_enum()),
                Type::U8 => Some(self.context.i8_type().as_basic_type_enum()),
                Type::Void => None,
                Type::StructType(s) => {
                    println!("s: {:?}, sn, {}, ss: {:?}", s, s.name.get(), self.structs);
                    Some(
                        self.structs
                            .get(&s.name.get())
                            .unwrap()
                            .1
                            .as_basic_type_enum(),
                    )
                }
                _ => todo!(),
            };
            let mut arg_types = vec![];
            for i in t.args {
                arg_types.push(match i {
                    Type::I64 => self.context.i64_type().as_basic_type_enum(),
                    Type::U8 => self.context.i8_type().as_basic_type_enum(),
                    _ => todo!(),
                });
            }
            if let Some(ret_ty) = fn_ret_ty {
                ret_ty.fn_type(arg_types.as_slice(), false)
            } else {
                self.context
                    .void_type()
                    .fn_type(arg_types.as_slice(), false)
            }
        } else {
            (match f.ty {
                Type::I64 => self.context.i64_type().as_basic_type_enum(),
                Type::U8 => self.context.i8_type().as_basic_type_enum(),
                _ => todo!(),
            })
            .fn_type(&[], false)
        };

        let fn_val = self.module.add_function(&f.name.get(), fn_ty, None);

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

        println!("Pre-checks: {}", self.dump_module());

        if fn_val.verify(true) {
            self.fpm.run_on(&fn_val);
            return Ok(fn_val);
        }
        unsafe {
            fn_val.delete();
        }
        return Err(format!("bad function lol"));
    }
}
