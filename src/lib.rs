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
    fn read_expression_once_diff_once_eval_many() {
        let e = &Environment::new();
        let s: String = read_line();
        let target_expr = match expr().parse(&s, e) {
            Ok((_, _, (expr, env))) => expr,
            Err(_) => panic!(""),
        };
        println!("parsed");
        let var: String = read_line();
        let v = match variables().parse(&var, e) {
            Ok((_, _, mut vars)) => {
                assert!(vars.len() == 1);
                vars.pop().unwrap()
            }
            Err(_) => panic!(""),
        };
        print!("derivative is ");
        let naive_d = target_expr.diff(&var, e).reduce(e);
        naive_d.print(e);
        let mut d = Deriv::new(target_expr, e, &var);
        println!("root{:?}", d.root);
        for (i, l) in d.graph.iter().enumerate() {
            for edge in l {
                println!("{} -> {}", i, edge.to);
                edge.exp.print(e);
            }
        }
        let d_for_dp = d.clone();
        d.reduce(e);
        loop {
            for (i, l) in d.graph.iter().enumerate() {
                for edge in l {
                    println!("{} -> {}", i, edge.to);
                    edge.exp.print(e);
                }
            }
            let x: f64 = read();
            let time = Duration::span(|| println!("{}", naive_d.eval(&var, &vec![x], e)));
            println!("naive:{} micro sec takes", time.num_microseconds().unwrap());
            println!("naive:{} nano sec takes", time.num_nanoseconds().unwrap());
            let time = Duration::span(|| println!("{}", d_for_dp.evaldp(&var, &vec![x], e)));
            println!("dp:{} micro sec takes", time.num_microseconds().unwrap());
            println!("dp:{} nano sec takes", time.num_nanoseconds().unwrap());
            let time = Duration::span(|| println!("{}", d.eval(&var, &vec![x], e)));
            println!("dom:{} micro sec takes", time.num_microseconds().unwrap());
            println!("dom:{} nano sec takes", time.num_nanoseconds().unwrap());
        }
    }

    #[test]
    fn large_example_chain() {
        let e = &Environment::new();
        let size = 13;
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
        target_expr.print(e);
        let var: String = String::from("x");
        print!("derivative is ");
        let naive_d = target_expr.diff(&var, e).reduce(e);
        let mut d = Deriv::new(target_expr, e, &var);
        let d_for_dp = d.clone();
        d.reduce(e);
        loop {
            let x: f64 = read();
            let time = Duration::span(|| println!("{}", naive_d.eval(&var, &vec![x], e)));
            println!("naive:{} micro sec takes", time.num_microseconds().unwrap());
            println!("naive:{} nano sec takes", time.num_nanoseconds().unwrap());
            let time = Duration::span(|| println!("{}", d_for_dp.evaldp(&var, &vec![x], e)));
            println!("dp:{} micro sec takes", time.num_microseconds().unwrap());
            println!("dp:{} nano sec takes", time.num_nanoseconds().unwrap());
            let time = Duration::span(|| println!("{}", d.eval(&var, &vec![x], e)));
            println!("dom:{} micro sec takes", time.num_microseconds().unwrap());
            println!("dom:{} nano sec takes", time.num_nanoseconds().unwrap());
        }
    }

    #[test]
    fn large_example_circle() {
        let e = &Environment::new();
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
        target_expr.print(e);
        let var: String = String::from("x");
        print!("derivative is ");
        let naive_d = target_expr.diff(&var, e).reduce(e);
        let mut d = Deriv::new(target_expr, e, &var);
        let d_for_dp = d.clone();
        d.reduce(e);
        loop {
            let x: f64 = read();
            let time = Duration::span(|| println!("{}", naive_d.eval(&var, &vec![x], e)));
            println!("naive:{} micro sec takes", time.num_microseconds().unwrap());
            println!("naive:{} nano sec takes", time.num_nanoseconds().unwrap());
            let time = Duration::span(|| println!("{}", d_for_dp.evaldp(&var, &vec![x], e)));
            println!("dp:{} micro sec takes", time.num_microseconds().unwrap());
            println!("dp:{} nano sec takes", time.num_nanoseconds().unwrap());
            let time = Duration::span(|| println!("{}", d.eval(&var, &vec![x], e)));
            println!("dom:{} micro sec takes", time.num_microseconds().unwrap());
            println!("dom:{} nano sec takes", time.num_nanoseconds().unwrap());
        }
    }
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
