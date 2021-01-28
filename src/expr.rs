pub use num_rational::Rational64;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type C = Rational64;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Var {
    id: usize,
}

impl Var {
    pub fn new(id: usize) -> Self {
        Var { id }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

// あとで
// #[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
// pub enum Func {
//     Sin,
//     Cos,
//     Tan,
//     Log,
//     Exp,
//     Neg
// }

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expr {
    BinOp {
        op: Op,
        exp1: Rc<Expr>,
        exp2: Rc<Expr>,
    },
    Sin(Rc<Expr>),
    Cos(Rc<Expr>),
    Tan(Rc<Expr>),
    Log(Rc<Expr>),
    Exp(Rc<Expr>),
    Neg(Rc<Expr>),
    Var(Var),
    Num(C),
}

impl Expr {
    fn new_binop(one: Rc<Expr>, other: Rc<Expr>, op: Op) -> Rc<Expr> {
        Rc::new(Expr::BinOp {
            op,
            exp1: one,
            exp2: other,
        })
    }
    fn new_num(n: i64) -> Rc<Expr> {
        Rc::new(Expr::Num(C::new(n, 1)))
    }
    pub fn diff(&self, v: &str, e: &Env) -> Rc<Expr> {
        match e.borrow().search_var(&String::from(v)) {
            Some(v) => self.diff_internal(v, e),
            None => {
                // unreachable!();
                Rc::new(Expr::Num(C::new(0, 1)))
            }
        }
    }
    fn diff_internal(&self, v: Var, e: &Env) -> Rc<Expr> {
        match self {
            Expr::BinOp { op, exp1, exp2 } => {
                let factor_left;
                let factor_right;
                match op {
                    Op::Add => {
                        factor_left = Expr::new_num(1);
                        factor_right = Expr::new_num(1);
                    }
                    Op::Sub => {
                        factor_left = Expr::new_num(1);
                        factor_right = Expr::new_num(-1);
                    }
                    Op::Mul => {
                        factor_left = exp2.clone();
                        factor_right = exp1.clone();
                    }
                    Op::Div => {
                        factor_left = Expr::new_binop(Expr::new_num(1), exp2.clone(), Op::Div);
                        let deno = Expr::new_binop(exp2.clone(), Expr::new_num(2), Op::Pow);
                        factor_right =
                            Rc::new(Expr::Neg(Expr::new_binop(exp1.clone(), deno, Op::Div)));
                    }
                    Op::Pow => {
                        let factor1 = Expr::new_binop(exp2.clone(), exp1.clone(), Op::Div);
                        let factor2 = Rc::new(Expr::Log(exp1.clone()));
                        factor_left = Expr::new_binop(factor1, Rc::new(self.clone()), Op::Mul);
                        factor_right = Expr::new_binop(factor2, Rc::new(self.clone()), Op::Mul);
                    }
                }
                let left = Expr::new_binop(factor_left, exp1.diff_internal(v, e), Op::Mul);
                let right = Expr::new_binop(factor_right, exp2.diff_internal(v, e), Op::Mul);
                Expr::new_binop(left, right, Op::Add)
            }
            Expr::Sin(inexp) => Expr::new_binop(
                Rc::new(Expr::Cos(inexp.clone())),
                inexp.diff_internal(v, e),
                Op::Mul,
            ),
            Expr::Cos(inexp) => {
                let inner = Expr::new_binop(
                    Rc::new(Expr::Sin(inexp.clone())),
                    inexp.diff_internal(v, e),
                    Op::Mul,
                );
                Rc::new(Expr::Neg(Rc::new(Expr::Neg(inner))))
            }
            Expr::Tan(inexp) => {
                let factor =
                    Expr::new_binop(Rc::new(Expr::Cos(inexp.clone())), Expr::new_num(2), Op::Pow);
                Expr::new_binop(factor, inexp.diff_internal(v, e), Op::Mul)
            }
            Expr::Log(inexp) => {
                let factor = Expr::new_binop(Expr::new_num(1), inexp.clone(), Op::Div);
                Expr::new_binop(factor, inexp.diff_internal(v, e), Op::Mul)
            }
            Expr::Exp(inexp) => {
                Expr::new_binop(Rc::new(self.clone()), inexp.diff_internal(v, e), Op::Mul)
            }
            Expr::Neg(inexp) => Rc::new(Expr::Neg(inexp.diff_internal(v, e))),
            Expr::Var(vt) => {
                if *vt == v {
                    Expr::new_num(1)
                } else {
                    Expr::new_num(0)
                }
            }
            Expr::Num(_n) => Expr::new_num(0),
        }
    }
    pub fn print(&self, e: &Env) {
        self.print_internal(e);
    }

    fn print_func(&self, name: &str, e: &Env) {
        print!("{}(", name);
        self.print_internal(e);
        print!(")");
    }

    fn print_internal(&self, e: &Env) {
        match self {
            Expr::BinOp { op, exp1, exp2 } => {
                let ops;
                match op {
                    Op::Add => {
                        ops = String::from("+");
                    }
                    Op::Sub => {
                        ops = String::from("-");
                    }
                    Op::Mul => {
                        ops = String::from("*");
                    }
                    Op::Div => {
                        ops = String::from("/");
                    }
                    Op::Pow => {
                        ops = String::from("^");
                    }
                }
                exp1.print_internal(e);
                print!("{}", ops);
                exp2.print_internal(e);
            }
            Expr::Sin(inexp) => {
                inexp.print_func("sin", e);
            }
            Expr::Cos(inexp) => {
                inexp.print_func("cos", e);
            }
            Expr::Tan(inexp) => {
                inexp.print_func("tan", e);
            }
            Expr::Log(inexp) => {
                inexp.print_func("log", e);
            }
            Expr::Exp(inexp) => {
                inexp.print_func("exp", e);
            }
            Expr::Neg(inexp) => {
                print!("-");
                inexp.print_internal(e);
            }
            Expr::Var(vt) => {
                print!("{}", e.borrow().vars[vt]);
            }
            Expr::Num(n) => {
                print!("{}", n);
            }
        }
    }
}

// impl std::fmt::Display for Expr {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let res = String::new();
//         write!(f, "{}", res)
//     }
// }

// 文字列からの検索, 変数からの検索を両方早くしたいんだけど, Mapにすると重そう
// かといってそうじゃなければ変数の数だけはかかる？
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Environment {
    pub vars: HashMap<Var, String>,
    pub rev_vars: HashMap<String, Var>,
    pub exprs: HashMap<Expr, Rc<Expr>>,
}

pub type Env = RefCell<Environment>;

impl Environment {
    pub fn new() -> Env {
        RefCell::new(Environment {
            vars: HashMap::new(),
            rev_vars: HashMap::new(),
            exprs: HashMap::new(),
        })
    }

    pub fn extend_var(&mut self, var_str: String) -> Var {
        let v = Var::new(self.vars.len());
        self.vars.insert(v, var_str.clone());
        self.rev_vars.insert(var_str, v);
        v
    }

    pub fn search_var(&self, var_str: &String) -> Option<Var> {
        match self.rev_vars.get(var_str) {
            Some(v) => Some(*v),
            None => None,
        }
    }

    pub fn extend_expr(&mut self, e: Expr) -> Rc<Expr> {
        match self.search_expr(&e) {
            Some(ptr_e) => ptr_e,
            None => {
                let ptr_e = Rc::new(e.clone());
                self.exprs.insert(e, ptr_e.clone());
                ptr_e
            }
        }
    }

    fn search_expr(&self, e: &Expr) -> Option<Rc<Expr>> {
        match self.exprs.get(&e) {
            Some(ptr) => Some(ptr.clone()),
            None => None,
        }
    }
}
