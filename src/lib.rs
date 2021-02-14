mod diff;
mod expr;
mod parse;
mod parser_combinator;

use chrono::Duration;
use diff::*;
use expr::*;
use parse::*;
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn large_example_chain() {
        let e = &Environment::new();
        let size = 15;
        let sec_max = 5;
        let v = match variables().parse(&"x", e) {
            Ok((_, _, mut vars)) => {
                assert!(vars.len() == 1);
                vars.pop().unwrap()
            }
            Err(_) => panic!(""),
        };
        let mut target_expr = v.clone();
        for _ in 0..size {
            let cos = Expr::new_unop(Uop::Cos, target_expr.clone(), e);
            let sin = Expr::new_unop(Uop::Sin, target_expr, e);
            target_expr = Expr::new_binop(Bop::Add, cos, sin, e);
        }
        // target_expr.print(e);
        let var: String = String::from("x");
        let naive_d = target_expr.diff(&var, e).reduce(e);
        let mut d = Deriv::new(target_expr, e, &var);
        let d_for_dp = d.clone();
        // optimization
        let now = time::Instant::now();
        d.reduce(e);
        println!("optimized in {} mill sec", now.elapsed().as_millis());
        let xs = String::from("x");
        let v = e.borrow().rev_vars[&xs];
        use std::time;
        let now = time::Instant::now();
        let mut cnt = 0;
        let x = 1.;
        println!("differentiation w.r.t single variable in {} sec", sec_max);
        while now.elapsed().as_secs() < sec_max {
            naive_d.eval(&var, &vec![x], e);
            cnt += 1;
        }
        println!("naive expression tree walk: {} times", cnt);

        cnt = 0;
        let now = time::Instant::now();
        while now.elapsed().as_secs() < sec_max {
            d_for_dp.forward_eval_dp(v, &var, &vec![x], e);
            cnt += 1;
        }
        println!("derivative graph: {} times", cnt);
        cnt = 0;
        let now = time::Instant::now();
        while now.elapsed().as_secs() < sec_max {
            d.forward_eval(v, &var, &vec![x], e);
            cnt += 1;
        }
    }

    #[test]
    fn large_example_circle() {
        let sec_max = 5;
        let e = &Environment::new();
        // 大きすぎるとstack overflow
        let size = 100;
        let v = match variables().parse(&"x", e) {
            Ok((_, _, mut vars)) => {
                assert!(vars.len() == 1);
                vars.pop().unwrap()
            }
            Err(_) => panic!(""),
        };
        let mut target_expr = v.clone();
        let mut cos = Expr::new_unop(Uop::Cos, target_expr.clone(), e);
        let mut sin = Expr::new_unop(Uop::Cos, target_expr, e);
        for _ in 0..size {
            cos = Expr::new_unop(Uop::Cos, sin, e);
            sin = Expr::new_unop(Uop::Sin, cos.clone(), e);
        }
        target_expr = Expr::new_binop(Bop::Add, cos, sin, e);
        let var: String = String::from("x");
        let naive_d = target_expr.diff(&var, e).reduce(e);
        let mut d = Deriv::new(target_expr, e, &var);
        let d_for_dp = d.clone();

        // optimization
        let now = time::Instant::now();
        d.reduce(e);
        println!("optimized in {} mill sec", now.elapsed().as_millis());
        let xs = String::from("x");
        let v = e.borrow().rev_vars[&xs];
        use std::time;
        let now = time::Instant::now();
        let mut cnt = 0;
        let x = 1.;
        println!("differentiation w.r.t single variable in {} sec", sec_max);
        while now.elapsed().as_secs() < sec_max {
            naive_d.eval(&var, &vec![x], e);
            cnt += 1;
        }
        println!("naive expression tree walk: {} times", cnt);

        cnt = 0;
        let now = time::Instant::now();
        while now.elapsed().as_secs() < sec_max {
            d_for_dp.forward_eval_dp(v, &var, &vec![x], e);
            cnt += 1;
        }
        println!("derivative graph: {} times", cnt);
        cnt = 0;
        let now = time::Instant::now();
        while now.elapsed().as_secs() < sec_max {
            d.forward_eval(v, &var, &vec![x], e);
            cnt += 1;
        }
        println!("derivative graph optimize: {} times", cnt);
    }

    // fn p(l: i64, m: i64, z: Rc<Expr>, env: &Env) -> Rc<Expr> {
    //     if l == 0 && m == 0 {
    //         Expr::new_num(1, env)
    //     } else if l == m {
    //         let a = Expr::new_binop(Bop::Mul, Expr::new_num(2, env), Expr::new_num(m, env), env);
    //         let b = Expr::new_binop(Bop::Sub, Expr::new_num(1, env), a, env);
    //         Expr::new_binop(Bop::Mul, b, p(m - 1, m - 1, z, env), env).reduce(env)
    //     } else if l == (m + 1) {
    //         let a = Expr::new_binop(Bop::Mul, Expr::new_num(2, env), Expr::new_num(m, env), env);
    //         let b = Expr::new_binop(Bop::Sub, a, Expr::new_num(1, env), env);
    //         let c = Expr::new_binop(Bop::Mul, b, z.clone(), env);
    //         Expr::new_binop(Bop::Mul, c, p(m, m, z, env), env).reduce(env)
    //     } else {
    //         let a1 = Expr::new_num((2 * l - 1) / l - m, env);
    //         let b1 = Expr::new_binop(Bop::Mul, a1, z.clone(), env);
    //         let c = Expr::new_binop(Bop::Mul, b1, p(l - 1, m, z.clone(), env), env);
    //         let a2 = Expr::new_num((l + m - 1) / l - m, env);
    //         let b2 = Expr::new_binop(Bop::Mul, a2, p(l - 2, m, z, env), env);
    //         Expr::new_binop(Bop::Sub, c, b2, env).reduce(env)
    //     }
    // }
    // fn s(m: i64, env: &Env) -> Rc<Expr> {
    //     if m == 0 {
    //         Expr::new_num(0, env)
    //     } else {
    //         let x = Expr::new_var("x".to_owned(), env);
    //         let y = Expr::new_var("y".to_owned(), env);
    //         let a = Expr::new_binop(Bop::Mul, x, c(m - 1, env), env);
    //         let b = Expr::new_binop(Bop::Mul, y, s(m - 1, env), env);
    //         Expr::new_binop(Bop::Sub, a, b, env).reduce(env)
    //     }
    // }
    // fn c(m: i64, env: &Env) -> Rc<Expr> {
    //     if m == 0 {
    //         Expr::new_num(1, env).reduce(env)
    //     } else {
    //         let x = Expr::new_var("x".to_owned(), env);
    //         let y = Expr::new_var("y".to_owned(), env);
    //         let a = Expr::new_binop(Bop::Mul, x, s(m - 1, env), env);
    //         let b = Expr::new_binop(Bop::Mul, y, c(m - 1, env), env);
    //         Expr::new_binop(Bop::Add, a, b, env).reduce(env)
    //     }
    // }
    // fn fact(n: i64) -> i64 {
    //     if n == 0 {
    //         1
    //     } else {
    //         n * fact(n - 1)
    //     }
    // }
    // fn n(m: i64, l: i64, env: &Env) -> Rc<Expr> {
    //     let p = Expr::pi(env);
    //     if m == 0 {
    //         let c = Expr::new_binop(
    //             Bop::Div,
    //             Expr::new_num(2 * l + 1, env),
    //             Expr::new_binop(Bop::Mul, Expr::new_num(4, env), p, env),
    //             env,
    //         );
    //         Expr::sqrt(c, env)
    //     } else {
    //         let c = Expr::new_binop(
    //             Bop::Div,
    //             Expr::new_num(2 * l + 1, env),
    //             Expr::new_binop(Bop::Mul, Expr::new_num(2, env), p, env),
    //             env,
    //         );
    //         let c1 = Expr::new_num(fact(l - m) / fact(l + m), env);
    //         Expr::sqrt(c1, env)
    //     }
    // }

    // fn y(l: i64, mut m: i64, env: &Env) -> Rc<Expr> {
    //     println!("{:?}", (l, m));
    //     let t;
    //     if m < 0 {
    //         m = -m;
    //         t = Expr::new_binop(
    //             Bop::Mul,
    //             p(l, m, Expr::new_var("z".to_owned(), env), env),
    //             s(m, env),
    //             env,
    //         );
    //     } else {
    //         t = Expr::new_binop(
    //             Bop::Mul,
    //             p(l, m, Expr::new_var("z".to_owned(), env), env),
    //             c(m, env),
    //             env,
    //         );
    //     }
    //     println!("{:?}", "yeaw");
    //     Expr::new_binop(Bop::Mul, n(l, m, env), t, env)
    // }

    // #[test]
    // fn spherical_harmonics() {
    //     let env = &Environment::new();
    //     let max_l = 5;
    //     let mut target_exprs = vec![];
    //     for l in 0..max_l {
    //         for m in -l..l {
    //             target_exprs.push(y(l, m, env));
    //         }
    //     }
    //     let (x, y, z) = match variables().parse(&"x y z", env) {
    //         Ok((_, _, mut vars)) => {
    //             assert!(vars.len() == 3);
    //             (
    //                 vars.pop().unwrap(),
    //                 vars.pop().unwrap(),
    //                 vars.pop().unwrap(),
    //             )
    //         }
    //         Err(_) => panic!(""),
    //     };
    //     let (mut deriv_tree_xs, mut deriv_tree_ys, mut deriv_tree_zs) = (vec![], vec![], vec![]);
    //     for var in ["x", "y", "z"].iter().map(|s| s.to_owned()) {
    //         for t in &target_exprs {
    //             if var == String::from("x") {
    //                 deriv_tree_xs.push(t.diff(&var, env).reduce(env));
    //             } else if var == String::from("y") {
    //                 deriv_tree_ys.push(t.diff(&var, env).reduce(env));
    //             } else {
    //                 deriv_tree_zs.push(t.diff(&var, env).reduce(env));
    //             }
    //         }
    //     }
    //     let var: String = String::from("hoge");
    //     let mut derivative_graphs = vec![];
    //     let mut derivative_graphs_for_dp = vec![];
    //     for t in target_exprs {
    //         let d = Deriv::new(t, env, &var);
    //         derivative_graphs.push(d.clone());
    //         derivative_graphs_for_dp.push(d);
    //     }
    //     let values = vec![1., 1., 1.];
    //     let var = String::from("x y z");
    //     use std::time;
    //     // let now = time::Instant::now();
    //     // while now.elapsed().as_secs() < 2 {
    //     //     for derives in [deriv_tree_xs, deriv_tree_ys, deriv_tree_zs].iter() {
    //     //         for d in derives {
    //     //             d.eval(&var, &values, env);
    //     //         }
    //     //     }
    //     // }
    // let now = time::Instant::now();
    // while now.elapsed().as_secs() < 2 {
    //     for d in &derivative_graphs_for_dp {
    //         d.backward_grad(&var, &values, env);
    //     }
    // }
    //     let now = time::Instant::now();
    //     while now.elapsed().as_secs() < 2 {
    //         for d in &derivative_graphs {
    //             d.backward_grad(&var, &values, env);
    //         }
    //     }
    // }
}
fn read_line() -> String {
    use std::io::Read;
    let stdin = std::io::stdin();
    let stdin = stdin.lock();
    let token: String = stdin
        .bytes()
        .map(|c| c.expect("failed to read char") as char)
        .take_while(|c| !c.is_whitespace())
        .collect();
    token.parse().ok().expect("failed to parse token")
}

fn read<T: std::str::FromStr>() -> T {
    use std::io::Read;
    let stdin = std::io::stdin();
    let stdin = stdin.lock();
    let token: String = stdin
        .bytes()
        .map(|c| c.expect("failed to read char") as char)
        .skip_while(|c| c.is_whitespace())
        .take_while(|c| !c.is_whitespace())
        .collect();
    token.parse().ok().expect("failed to parse token")
}
