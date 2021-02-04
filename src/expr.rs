pub use num_rational::Rational64;
pub use num_traits::identities::{One, Zero};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type C = Rational64;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Var {
    id: usize,
}

impl Var {
    pub fn new(id: usize) -> Self {
        Var { id }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Bop {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Uop {
    Sin,
    Cos,
    Tan,
    Log,
    Exp,
    Neg,
}

// reduceの都合でBinOpを最後に
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
    Var(Var),
    Num(C),
    UnOp {
        op: Uop,
        exp: Rc<Expr>,
    },
    BinOp {
        op: Bop,
        exp1: Rc<Expr>,
        exp2: Rc<Expr>,
    },
}

impl Expr {
    pub fn new_unop(op: Uop, one: Rc<Expr>, env: &Env) -> Rc<Expr> {
        let e = Expr::UnOp { op, exp: one };
        env.borrow_mut().extend_expr(e)
    }

    pub fn new_binop(op: Bop, one: Rc<Expr>, other: Rc<Expr>, env: &Env) -> Rc<Expr> {
        let e = Expr::BinOp {
            op,
            exp1: one,
            exp2: other,
        };
        env.borrow_mut().extend_expr(e)
    }

    pub fn new_num(n: i64, env: &Env) -> Rc<Expr> {
        let e = Expr::Num(C::new(n, 1));
        let p = env.borrow_mut().extend_expr(e);
        p
    }

    pub fn new_var(n: usize, env: &Env) -> Rc<Expr> {
        let e = Expr::Var(Var::new(n));
        env.borrow_mut().extend_expr(e)
    }

    fn new_num_from_op(op: Bop, left: Rc<Expr>, right: Rc<Expr>, env: &Env) -> Rc<Expr> {
        match *left {
            Expr::Num(n) => match *right {
                Expr::Num(m) => match op {
                    Bop::Add => Rc::new(Expr::Num(n + m)),
                    Bop::Sub => Rc::new(Expr::Num(n - m)),
                    Bop::Mul => Rc::new(Expr::Num(n * m)),
                    Bop::Div => Rc::new(Expr::Num(n / m)),
                    // Powは無理(無理数)
                    Bop::Pow => unimplemented!(),
                },
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn new_num_from_rat(c: C, env: &Env) -> Rc<Expr> {
        let e = Expr::Num(c);
        env.borrow_mut().extend_expr(e)
    }

    fn is_const(&self) -> bool {
        match self {
            Expr::Num(_n) => true,
            _ => false,
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            Expr::Num(n) => n.is_zero(),
            _ => false,
        }
    }

    fn is_one(&self) -> bool {
        match self {
            Expr::Num(n) => *n == C::one(),
            _ => false,
        }
    }

    fn is_minus_one(&self) -> bool {
        match self {
            Expr::Num(n) => *n == -C::one(),
            _ => false,
        }
    }

    pub fn diff(&self, v: &str, e: &Env) -> Rc<Expr> {
        let var = e.borrow().search_var(&String::from(v));
        match var {
            Some(v) => self.diff_internal(v, e),
            None => {
                // unreachable!();
                Rc::new(Expr::Num(C::new(0, 1)))
            }
        }
    }
    fn diff_internal(&self, v: Var, e: &Env) -> Rc<Expr> {
        match self {
            Expr::UnOp { op, exp: inexp } => match op {
                Uop::Sin => Expr::new_binop(
                    Bop::Mul,
                    Expr::new_unop(Uop::Cos, inexp.clone(), e),
                    inexp.diff_internal(v, e),
                    e,
                ),
                Uop::Cos => {
                    let inner = Expr::new_binop(
                        Bop::Mul,
                        Expr::new_unop(Uop::Sin, inexp.clone(), e),
                        inexp.diff_internal(v, e),
                        e,
                    );
                    Expr::new_unop(Uop::Neg, inner, e)
                }
                Uop::Tan => {
                    let factor = Expr::new_binop(
                        Bop::Pow,
                        Expr::new_unop(Uop::Cos, inexp.clone(), e),
                        Expr::new_num(2, e),
                        e,
                    );
                    Expr::new_binop(Bop::Div, inexp.diff_internal(v, e), factor, e)
                }
                Uop::Log => {
                    let factor = Expr::new_binop(Bop::Div, Expr::new_num(1, e), inexp.clone(), e);
                    Expr::new_binop(Bop::Mul, factor, inexp.diff_internal(v, e), e)
                }
                Uop::Exp => Expr::new_binop(
                    Bop::Mul,
                    e.borrow_mut().extend_expr(self.clone()),
                    inexp.diff_internal(v, e),
                    e,
                ),
                Uop::Neg => Expr::new_unop(Uop::Neg, inexp.diff_internal(v, e), e),
            },
            Expr::BinOp { op, exp1, exp2 } => {
                let factor_left;
                let factor_right;
                match op {
                    Bop::Add => {
                        factor_left = Expr::new_num(1, e);
                        factor_right = Expr::new_num(1, e);
                    }
                    Bop::Sub => {
                        factor_left = Expr::new_num(1, e);
                        factor_right = Expr::new_num(-1, e);
                    }
                    Bop::Mul => {
                        factor_left = exp2.clone();
                        factor_right = exp1.clone();
                    }
                    Bop::Div => {
                        factor_left =
                            Expr::new_binop(Bop::Div, Expr::new_num(1, e), exp2.clone(), e);
                        let deno = Expr::new_binop(Bop::Pow, exp2.clone(), Expr::new_num(2, e), e);
                        factor_right = Expr::new_unop(
                            Uop::Neg,
                            Expr::new_binop(Bop::Div, exp1.clone(), deno, e),
                            e,
                        );
                    }
                    Bop::Pow => {
                        let factor1 = Expr::new_binop(Bop::Div, exp2.clone(), exp1.clone(), e);
                        let factor2 = Expr::new_unop(Uop::Log, exp1.clone(), e);
                        factor_left = Expr::new_binop(
                            Bop::Mul,
                            factor1,
                            e.borrow_mut().extend_expr(self.clone()),
                            e,
                        );
                        factor_right = Expr::new_binop(
                            Bop::Mul,
                            factor2,
                            e.borrow_mut().extend_expr(self.clone()),
                            e,
                        );
                    }
                }
                let left = Expr::new_binop(Bop::Mul, factor_left, exp1.diff_internal(v, e), e);
                let right = Expr::new_binop(Bop::Mul, factor_right, exp2.diff_internal(v, e), e);
                Expr::new_binop(Bop::Add, left, right, e)
            }
            Expr::Var(vt) => {
                if *vt == v {
                    Expr::new_num(1, e)
                } else {
                    Expr::new_num(0, e)
                }
            }
            Expr::Num(_n) => Expr::new_num(0, e),
        }
    }

    pub fn reduce(&self, e: &Env) -> Rc<Expr> {
        self.reduce_internal(e)
    }
    fn reduce_internal(&self, e: &Env) -> Rc<Expr> {
        match self {
            Expr::UnOp { op, exp: inexp } => match op {
                Uop::Sin => {
                    let inexp = inexp.reduce(e);
                    // rationalなので, 完全な定数化は無理
                    if inexp.is_zero() {
                        Expr::new_num(0, e)
                    } else {
                        Expr::new_unop(Uop::Sin, inexp, e)
                    }
                }
                Uop::Cos => {
                    let inexp = inexp.reduce(e);
                    if inexp.is_zero() {
                        Expr::new_num(1, e)
                    } else {
                        Expr::new_unop(Uop::Cos, inexp, e)
                    }
                }
                Uop::Tan => {
                    let inexp = inexp.reduce(e);
                    if inexp.is_zero() {
                        Expr::new_num(0, e)
                    } else {
                        Expr::new_unop(Uop::Tan, inexp, e)
                    }
                }
                Uop::Log => {
                    // TODO: log x ^ e = e * log x ??
                    // TODO: log e = 1 ??
                    let inexp = inexp.reduce(e);
                    if inexp.is_one() {
                        Expr::new_num(0, e)
                    } else {
                        Expr::new_unop(Uop::Log, inexp, e)
                    }
                }
                Uop::Exp => {
                    // TODO: e log x = x
                    let inexp = inexp.reduce(e);
                    if inexp.is_zero() {
                        Expr::new_num(1, e)
                    } else {
                        Expr::new_unop(Uop::Exp, inexp, e)
                    }
                }
                Uop::Neg => {
                    let inexp = inexp.reduce(e);
                    match *inexp {
                        Expr::Num(n) => Rc::new(Expr::Num(-n)),
                        _ => Expr::new_unop(Uop::Neg, inexp, e),
                    }
                }
            },
            Expr::BinOp { op, exp1, exp2 } => {
                let (left, right) = (exp1.reduce(e), exp2.reduce(e));
                match op {
                    Bop::Add => {
                        // TODO: plus + plus以外をsubにする？
                        if left.is_zero() {
                            right
                        } else if right.is_zero() {
                            left
                        } else if left.is_const() && right.is_const() {
                            Expr::new_num_from_op(Bop::Add, left, right, e)
                        } else if Rc::ptr_eq(&left, &right) {
                            Expr::new_binop(Bop::Mul, Expr::new_num(2, e), left, e)
                        } else {
                            Expr::new_binop(Bop::Add, left, right, e)
                        }
                    }
                    Bop::Sub => {
                        if left.is_zero() {
                            right
                        } else if right.is_zero() {
                            left
                        } else if left.is_const() && right.is_const() {
                            Expr::new_num_from_op(Bop::Sub, left, right, e)
                        } else if Rc::ptr_eq(&left, &right) {
                            Expr::new_num(0, e)
                        } else {
                            Expr::new_binop(Bop::Sub, left, right, e)
                        }
                    }
                    Bop::Mul => {
                        if left.is_zero() || right.is_zero() {
                            Expr::new_num(0, e)
                        } else if left.is_one() {
                            right
                        } else if left.is_minus_one() {
                            Expr::new_unop(Uop::Neg, right, e)
                        } else if right.is_one() {
                            left
                        } else if right.is_minus_one() {
                            Expr::new_unop(Uop::Neg, left, e)
                        } else if left.is_const() && right.is_const() {
                            Expr::new_num_from_op(Bop::Mul, left, right, e)
                        } else if Rc::ptr_eq(&left, &right) {
                            Expr::new_binop(Bop::Pow, Expr::new_num(2, e), left, e)
                        } else {
                            Expr::new_binop(Bop::Mul, left, right, e)
                        }
                    }
                    Bop::Div => {
                        if left.is_zero() || right.is_zero() {
                            panic!("zero div")
                        } else if right.is_one() {
                            left
                        } else if right.is_minus_one() {
                            Expr::new_unop(Uop::Neg, left, e)
                        } else if left.is_const() && right.is_const() {
                            Expr::new_num_from_op(Bop::Div, left, right, e)
                        } else if Rc::ptr_eq(&left, &right) {
                            Expr::new_num(1, e)
                        } else {
                            Expr::new_binop(Bop::Div, left, right, e)
                        }
                    }
                    Bop::Pow => {
                        // TODO: e log x = x
                        if left.is_zero() {
                            Expr::new_num(0, e)
                        } else if right.is_zero() {
                            Expr::new_num(1, e)
                        } else if left.is_one() {
                            Expr::new_num(1, e)
                        } else if right.is_one() {
                            left
                        } else {
                            Expr::new_binop(Bop::Pow, left, right, e)
                        }
                    }
                }
            }
            Expr::Var(vt) => Expr::new_var(vt.id, e),
            Expr::Num(n) => Expr::new_num_from_rat(*n, e),
        }
    }

    pub fn print(&self, e: &Env) {
        self.print_internal(e);
        println!("");
    }

    fn print_func(&self, name: &str, e: &Env) {
        print!("{}(", name);
        self.print_internal(e);
        print!(")");
    }

    fn print_internal(&self, e: &Env) {
        match self {
            Expr::UnOp { op, exp: inexp } => match op {
                Uop::Sin => {
                    inexp.print_func("sin", e);
                }
                Uop::Cos => {
                    inexp.print_func("cos", e);
                }
                Uop::Tan => {
                    inexp.print_func("tan", e);
                }
                Uop::Log => {
                    inexp.print_func("log", e);
                }
                Uop::Exp => {
                    inexp.print_func("exp", e);
                }
                Uop::Neg => {
                    print!("-");
                    inexp.print_internal(e);
                }
            },
            Expr::BinOp { op, exp1, exp2 } => {
                let ops;
                match op {
                    Bop::Add => {
                        ops = String::from("+");
                    }
                    Bop::Sub => {
                        ops = String::from("-");
                    }
                    Bop::Mul => {
                        ops = String::from("*");
                    }
                    Bop::Div => {
                        ops = String::from("/");
                    }
                    Bop::Pow => {
                        ops = String::from("^");
                    }
                }
                exp1.print_internal(e);
                print!("{}", ops);
                exp2.print_internal(e);
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

    pub fn remove_expr(&mut self, e: &Expr) {
        match self.exprs.remove(e) {
            Some(_) => (),
            None => unreachable!(),
        }
    }

    pub fn clean(&mut self) {
        let mut remove_list = std::collections::HashSet::new();
        let mut change = true;
        while change {
            change = false;
            {
                for (exp, expp) in &self.exprs {
                    if Rc::strong_count(&expp) == 1 {
                        remove_list.insert(exp.clone());
                    }
                }
            }
            for rexpr in &remove_list {
                self.exprs.remove(rexpr);
                change = true;
            }
            remove_list = std::collections::HashSet::new();
        }
    }
}
