use super::expr::{Bop, Env, Environment, Expr, Uop, Var, C};
use super::parser_combinator::*;
use std::rc::Rc;

fn unsigned_number<'a>() -> impl Parser<'a, (Rc<Expr>, &'a Env)> {
    one_or_more(any_char.pred(|c| c.0.is_numeric())).map(|chars| {
        let expr = Expr::Num(chars.iter().fold(C::new(0, 1), |s, c| {
            s * C::new(10, 1) + C::new(c.0.to_digit(10).expect("") as i64, 1)
        }));
        let env = chars.last().expect("").1;
        let p = env.borrow_mut().extend_expr(expr);
        return (p, env);
    })
}
#[test]
fn number_parser() {
    let e = Environment::new();
    assert_eq!(
        Ok(("", &e, (Rc::new(Expr::Num(C::new(64, 1))), &e))),
        unsigned_number().parse("64", &e)
    );
    assert_eq!(
        Ok(("", &e, (Rc::new(Expr::Num(C::new(12333, 1))), &e))),
        unsigned_number().parse("12333", &e)
    );
    assert_eq!(
        Ok(("", &e, (Rc::new(Expr::Num(C::new(64, 1))), &e))),
        unsigned_number().parse("64", &e)
    );
    println!("{:?}", e);
    // assert_eq!(Ok(("", &e, Expr::Num(0))), unsigned_number().parse("0", &e));
    assert_eq!(Err(""), unsigned_number().parse("", &e));
    assert_eq!(Err("-123"), unsigned_number().parse("-123", &e));
}

fn variable<'a>() -> impl Parser<'a, (Rc<Expr>, &'a Env)> {
    identifier.map(|(s, env)| {
        let option_var = env.borrow_mut().search_var(&s);
        if let Some(var) = option_var {
            let var_expr = env.borrow().exprs[&Expr::Var(var)].clone();
            (var_expr, env)
        } else {
            let v = env.borrow_mut().extend_var(s);
            let var_expr = env.borrow_mut().extend_expr(Expr::Var(v));
            (var_expr, env)
        }
    })
}

#[test]
fn variable_parser() {
    let e = Environment::new();
    // let (_inp, new_e, res1) = variable().parse("x1", &e).unwrap();
    // println!("{:?}", new_e);
    assert_eq!(
        Ok(("", &e, (Rc::new(Expr::Var(Var::new(0))), &e))),
        variable().parse("x1", &e)
    );
    assert_eq!(
        Ok(("", &e, (Rc::new(Expr::Var(Var::new(0))), &e))),
        variable().parse("x1", &e)
    );
    println!("{:?}", e);
}

fn primary<'a>() -> impl Parser<'a, (Rc<Expr>, &'a Env)> {
    either(unsigned_number(), either(variable(), parenthesized_expr()))
}

fn func<'a>() -> impl Parser<'a, (Rc<Expr>, &'a Env)> {
    pair(
        one_of(vec!["sin", "cos", "tan", "log", "exp"]),
        parenthesized_expr(),
    )
    .map(|(name, (ex, env))| {
        let expr;
        match name {
            "sin" => expr = Expr::Sin(ex),
            "cos" => expr = Expr::Cos(ex),
            "tan" => expr = Expr::Tan(ex),
            "log" => expr = Expr::Log(ex),
            "exp" => expr = Expr::Exp(ex),
            _ => unimplemented!(),
        }
        // let optoin_expr = env.borrow().search_expr(&expr);
        // if optoin_expr.is_some() {
        //     (optoin_expr.unwrap(), env)
        // } else {
        (env.borrow_mut().extend_expr(expr), env)
        // }
    })
}

fn unary<'a>() -> impl Parser<'a, (Rc<Expr>, &'a Env)> {
    zero_or_more(whitespace_wrap(
        any_char.pred(|(c, _e)| *c == '+' || *c == '-'),
    ))
    .and_then(|vec_c_r| {
        either(func(), primary()).map(move |(p, env)| {
            let expr;
            if vec_c_r.iter().filter(|(c, _e)| *c == '-').count() % 2 != 0 {
                expr = Expr::Neg(p);
                let p = env.borrow_mut().extend_expr(expr);
                return (p, env);
            } else {
                return (p, env);
            }
        })
    })
}

#[test]
fn unary_parser() {
    let e = Environment::new();
    let exptcted_expr = Rc::new(Expr::Log(Rc::new(Expr::Var(Var::new(0)))));
    // Expr::new_unop(Expr::new_var_test(0), Uop::Log);
    assert_eq!(
        Ok(("", &e, (exptcted_expr, &e))),
        unary().parse("  - + - + log(x)", &e)
    );
}

fn factor<'a>() -> impl Parser<'a, (Rc<Expr>, &'a Env)> {
    unary().and_then(|(one, env)| {
        zero_or_more(right(whitespace_wrap(match_literal("^")), unary())).map(move |mut unaries| {
            if unaries.len() == 0 {
                (one.clone(), env)
            } else {
                let env = unaries.last().unwrap().1;
                let mut pow: Rc<Expr> = unaries.pop().unwrap().0;
                let expr = Expr::BinOp {
                    op: Bop::Pow,
                    exp1: one.clone(),
                    exp2: pow,
                };
                let mut res = Expr::new_num(std::i64::MAX);

                res = env.borrow_mut().extend_expr(expr);
                // 毎更新ごとに登録
                while let Some((una, _env)) = unaries.pop() {
                    let mut cur_expr = (*res).clone();
                    match &mut cur_expr {
                        Expr::BinOp { exp2, .. } => {
                            pow = Expr::new_binop(una, exp2.clone(), Bop::Pow);
                            *exp2 = pow;
                        }
                        _ => unreachable!(),
                    }
                    res = env.borrow_mut().extend_expr(cur_expr);
                }
                (res, env)
            }
        })
    })
}
#[test]
fn factor_parser() {
    let e = Environment::new();
    let expected_factor1 = Expr::new_binop(
        Expr::new_var_test(0),
        Expr::new_binop(Expr::new_num(3), Expr::new_num(2), Bop::Pow),
        Bop::Pow,
    );
    assert_eq!(
        Ok(("", &e, (expected_factor1, &e))),
        factor().parse("x1 ^ 3 ^ 2", &e)
    );
}

fn term<'a>() -> impl Parser<'a, (Rc<Expr>, &'a Env)> {
    factor().and_then(|(one, env)| {
        zero_or_more(whitespace_wrap(pair(
            whitespace_wrap(any_char.pred(|(c, _e)| *c == '*' || *c == '/')),
            factor(),
        )))
        .map(move |mut factors| {
            if factors.len() == 0 {
                (one.clone(), env)
            } else {
                let env = factors.last().unwrap().1 .1;
                let mut res = env.borrow_mut().extend_expr((*one).clone());
                factors.reverse();
                while let Some(((c, _e1), (f, _e2))) = factors.pop() {
                    let cur_expr;
                    match c {
                        '*' => {
                            cur_expr = Expr::BinOp {
                                op: Bop::Mul,
                                exp1: res,
                                exp2: f,
                            };
                        }
                        '/' => {
                            cur_expr = Expr::BinOp {
                                op: Bop::Div,
                                exp1: res,
                                exp2: f,
                            };
                        }
                        _ => unreachable!(),
                    }
                    res = env.borrow_mut().extend_expr(cur_expr);
                }
                (res, env)
            }
        })
    })
}

#[test]
fn term_parser() {
    let e = Environment::new();
    let expected_term = Expr::new_binop(
        Expr::new_binop(
            Expr::new_binop(Expr::new_var_test(0), Expr::new_num(3), Bop::Pow),
            Expr::new_binop(Expr::new_var_test(1), Expr::new_num(2), Bop::Pow),
            Bop::Mul,
        ),
        Expr::new_binop(Expr::new_var_test(0), Expr::new_num(4), Bop::Pow),
        Bop::Mul,
    );
    assert_eq!(
        Ok(("", &e, (expected_term, &e))),
        term().parse("x1 ^ 3 * y1 ^ 2 * x1 ^ 4", &e)
    );
}

fn expr<'a>() -> impl Parser<'a, (Rc<Expr>, &'a Env)> {
    whitespace_wrap(term()).and_then(|(one, env)| {
        zero_or_more(whitespace_wrap(pair(
            whitespace_wrap(any_char.pred(|(c, _e)| *c == '+' || *c == '-')),
            term(),
        )))
        .map(move |mut terms| {
            if terms.len() == 0 {
                (one.clone(), env)
            } else {
                let env = terms.last().unwrap().1 .1;
                let mut res = env.borrow_mut().extend_expr((*one).clone());
                terms.reverse();
                while let Some(((c, _e1), (t, _e2))) = terms.pop() {
                    let cur_expr;
                    match c {
                        '+' => {
                            cur_expr = Expr::BinOp {
                                op: Bop::Add,
                                exp1: res,
                                exp2: t,
                            };
                        }
                        '-' => {
                            cur_expr = Expr::BinOp {
                                op: Bop::Sub,
                                exp1: res,
                                exp2: t,
                            };
                        }
                        _ => unreachable!(),
                    }
                    res = env.borrow_mut().extend_expr(cur_expr);
                }
                (res, env)
            }
        })
    })
}

fn parenthesized_expr<'a>() -> impl Parser<'a, (Rc<Expr>, &'a Env)> {
    right(
        match_literal("("),
        left(whitespace_wrap(expr()), match_literal(")")),
    )
}

#[test]
fn expr_parser() {
    let e = Environment::new();

    let num = Rc::new(Expr::BinOp {
        op: Bop::Add,
        exp1: Rc::new(Expr::Num(C::new(2, 1))),
        exp2: Rc::new(Expr::Log(Rc::new(Expr::Var(Var::new(0))))),
    });
    // Expr::new_binop(Expr::new_num(2), Expr::new_unop(Expr::new_var_test(0), Uop::Log) , Bop::Add)
    let deno = Rc::new(Expr::Tan(Rc::new(Expr::Var(Var::new(0)))));
    let expr1 = Expr::new_binop(
        Expr::new_binop(num, deno, Bop::Div),
        Expr::new_var_test(1),
        Bop::Pow,
    );
    let exptcted_expr = Rc::new(Expr::BinOp {
        op: Bop::Add,
        exp1: expr1,
        exp2: Rc::new(Expr::Sin(Rc::new(Expr::Var(Var::new(1))))),
    });
    assert_eq!(
        Ok(("", &e, (exptcted_expr, &e))),
        expr().parse("( ( 2 + log(x) ) / tan(x) ) ^ y  + sin(y) ", &e)
    );
    let res = expr().parse("( ( 2 + log(x) ) / tan(x) ) ^ y  + sin(y)", &e);
    match res {
        Ok((_, _, (expr, env))) => {
            expr.diff("y", env).reduce(&e).print(&e);
        }
        Err(_) => panic!(""),
    }
}
