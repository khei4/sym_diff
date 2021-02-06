use super::expr::{Env, Environment, Expr};
use super::parse::*;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Edge {
    to: usize,
    exp: Rc<Expr>,
}
/*
let graph: Vec<Vec<Edge>> = vec![vec![]; nodesize];
let reverse_graph: Vec<Vec<Edge>> = vec![vec![]; nodesize];
*/
#[derive(Debug, Clone)]
struct Deriv {
    size: usize,
    root: usize,
    leafs: HashSet<usize>,
    graph: Vec<Vec<Edge>>,
    reverse_graph: Vec<Vec<Edge>>,
}

impl Deriv {
    // まずは1変数の微分グラフを出す.
    // Exprから微分をする
    fn new(expr: Rc<Expr>, e: &Env, v: &str) -> Self {
        let mut m = HashMap::new();
        let expr = expr.reduce(e);
        expr.post_index(&mut 0, &mut m);
        println!("{:?}", m);
        let size = m.len();
        let (mut graph, mut reverse_graph) = (vec![vec![]; size], vec![vec![]; size]);
        let mut leafs = vec![];
        let mut memo = HashSet::new();
        Deriv::construct(
            &(*expr),
            e,
            v,
            &m,
            &mut graph,
            &mut reverse_graph,
            &mut leafs,
            &mut memo,
        );
        // ここでLeafも計算はできる.
        Deriv {
            size,
            root: size - 1,
            leafs: leafs.into_iter().collect(),
            graph,
            reverse_graph,
        }
    }
    fn construct(
        expr: &Expr,
        e: &Env,
        v: &str,
        postids: &HashMap<Expr, usize>,
        graph: &mut Vec<Vec<Edge>>,
        reverse_graph: &mut Vec<Vec<Edge>>,
        leafs: &mut Vec<usize>,
        memo: &mut HashSet<(usize, usize)>,
    ) {
        // 子のIndexをふる
        // 辺を追加する
        match expr {
            Expr::UnOp { exp, .. } => {
                let parent_id = postids[&expr];
                let child_id = postids[&(*exp)];
                if memo.contains(&(parent_id, child_id)) {
                    return;
                } else {
                    memo.insert((parent_id, child_id));
                }
                Deriv::construct(&(*exp), e, v, postids, graph, reverse_graph, leafs, memo);
                // diffじゃだめで, 一段だけやらなきゃ
                let mut v = expr.diff_comp(v, e);
                assert!(v.len() == 1);
                let d = v.pop().expect("");
                let edge = Edge {
                    to: child_id,
                    exp: d.clone(),
                };
                let redge = Edge {
                    to: parent_id,
                    exp: d,
                };
                graph[parent_id].push(edge);
                reverse_graph[child_id].push(redge);
            }
            Expr::BinOp { exp1, exp2, .. } => {
                let parent_id = postids[&expr];
                let child1_id = postids[&(*exp1)];
                let child2_id = postids[&(*exp2)];
                if memo.contains(&(parent_id, child1_id)) {
                    return;
                } else {
                    memo.insert((parent_id, child1_id));
                }
                Deriv::construct(&(*exp1), e, v, postids, graph, reverse_graph, leafs, memo);
                Deriv::construct(&(*exp2), e, v, postids, graph, reverse_graph, leafs, memo);
                let mut v = expr.diff_comp(v, e);
                assert!(v.len() == 2);
                let d2 = v.pop().expect("");
                let d1 = v.pop().expect("");
                let edge1 = Edge {
                    to: child1_id,
                    exp: d1.clone(),
                };
                let edge2 = Edge {
                    to: child2_id,
                    exp: d2.clone(),
                };
                let redge1 = Edge {
                    to: parent_id,
                    exp: d1,
                };
                let redge2 = Edge {
                    to: parent_id,
                    exp: d2,
                };
                graph[parent_id].push(edge1);
                graph[parent_id].push(edge2);
                reverse_graph[child1_id].push(redge1);
                reverse_graph[child2_id].push(redge2);
            }
            // 変数と定数でサイズが変わるの考えてなかったな...
            // まあでも実際こうするしかないような気もする.一回0ありでやって, 微分木から変換するのも作る
            _ => leafs.push(postids[expr]),
        }
    }

    fn intersect(mut b1: usize, mut b2: usize, doms: &Vec<Option<usize>>) -> usize {
        while b1 != b2 {
            while b1 < b2 {
                b1 = doms[b1].expect("dominator intersection failure");
            }
            while b2 < b1 {
                b2 = doms[b2].expect("dominator intersection failure");
            }
        }
        b1
    }

    // 支配関係を求める.
    fn dom_rel(&self) -> Vec<HashSet<usize>> {
        let mut doms = vec![None; self.size];
        doms[self.root] = Some(self.root);
        let mut changed = true;
        while changed {
            changed = false;
            for u in (0..self.size - 1).rev() {
                let mut new_idom = std::usize::MAX;
                for &Edge { to: v, .. } in &self.reverse_graph[u] {
                    if let Some(_i) = doms[v] {
                        if new_idom == std::usize::MAX {
                            new_idom = v;
                        } else {
                            new_idom = Deriv::intersect(v, new_idom, &doms);
                        }
                    }
                }
                if doms[u] != Some(new_idom) {
                    doms[u] = Some(new_idom);
                    changed = true;
                }
            }
        }
        let domtree = doms;
        let mut res: Vec<HashSet<usize>> = vec![HashSet::new(); self.size];
        for i in 0..self.size {
            res[i].insert(i);
            let mut cur = i;
            while cur != domtree[cur].unwrap() {
                let dom = domtree[cur].unwrap();
                res[i].insert(dom);
                cur = dom;
            }
        }
        res
    }

    // 逆支配関係を求める.(pdomされてるのが入ってる)
    // HashSetをやめるのはWIP
    fn pdom_rel(&self) -> Vec<HashSet<usize>> {
        use std::iter::FromIterator;
        let mut pdoms = vec![HashSet::new(); self.size];
        for n in 0..self.size {
            pdoms[n].insert(n);
        }
        let leafs: HashSet<usize> = self.leafs.clone().iter().map(|l| *l).collect();

        let mut changed = true;
        while changed {
            changed = false;
            for u in 0..self.size {
                if leafs.contains(&u) {
                    continue;
                }
                let mut new_set = HashSet::from_iter(0..self.size);
                for &Edge { to: v, .. } in &self.graph[u] {
                    new_set = new_set.intersection(&pdoms[v]).map(|e| *e).collect();
                }
                new_set.insert(u);
                if pdoms[u] != new_set {
                    pdoms[u] = new_set;
                    changed = true;
                }
            }
        }
        pdoms
    }

    fn factor_subgraphs(
        &self,
        doms: &Vec<HashSet<usize>>,
        pdoms: &Vec<HashSet<usize>>,
    ) -> Vec<(usize, usize)> {
        let mut factor_dom_nodes: HashSet<usize> = HashSet::new();
        let mut factor_pdom_nodes: HashSet<usize> = HashSet::new();
        // 支配木をたどってfactorを探す
        for i in 0..self.size {
            if 2 <= self.graph[i].len() {
                factor_dom_nodes.insert(i);
            }
            // factor_pdom
            if 2 <= self.reverse_graph[i].len() {
                factor_pdom_nodes.insert(i);
            }
        }
        let mut res = vec![];
        // domなら fd > n
        for fd in factor_dom_nodes {
            for n in 0..self.size {
                if n != fd && doms[n].contains(&fd) && 2 <= self.reverse_graph[n].len() {
                    res.push((fd, n));
                }
            }
        }
        // pdomなら fpd < n
        for fpd in factor_pdom_nodes {
            for n in 0..self.size {
                if n != fpd && pdoms[n].contains(&fpd) && 2 <= self.graph[n].len() {
                    res.push((fpd, n));
                }
            }
        }
        res.sort_by(|(x1, y1), (x2, y2)| {
            let absdiff = |x, y| {
                if x > y {
                    x - y
                } else {
                    y - x
                }
            };
            let diff1 = absdiff(x1, y1);
            let diff2 = absdiff(x2, y2);
            diff1
                .cmp(&diff2)
                .then_with(|| std::cmp::min(x1, y1).cmp(std::cmp::min(x2, y2)))
        });
        res
    }

    fn shrink(
        &mut self,
        fsub: (usize, usize),
        doms: &Vec<HashSet<usize>>,
        pdoms: &Vec<HashSet<usize>>,
        env: &Env,
    ) {
        let (start, goal) = (std::cmp::max(fsub.0, fsub.1), std::cmp::min(fsub.0, fsub.1));
        let mut que = vec![(start, vec![])];
        let mut paths = vec![];
        while 0 < que.len() {
            let (cur, path) = que.pop().unwrap();
            if cur == goal {
                paths.push(path);
                continue;
            } else if self.leafs.contains(&cur) {
                continue;
            } else {
                for Edge { to: next, .. } in &self.graph[cur] {
                    let mut p = path.clone();
                    p.push(*next);
                    que.push((*next, p));
                }
            }
        }
        // domなら 0 > 1,  pdomなら 0 < 1
        let mut res = Expr::new_num(0, env);
        let mut edges_will_be_removed: HashSet<(usize, usize)> = HashSet::new();
        use super::expr::Bop;
        for path in paths {
            let mut cur = start;
            let mut temp_expr = Expr::new_num(1, env);
            for next in path {
                // edgeをみつける
                for Edge { to: v, exp } in &self.graph[cur] {
                    if *v == next {
                        println!("edge{:?}", (cur, next));
                        temp_expr = Expr::new_binop(Bop::Mul, temp_expr, exp.clone(), env);
                        // v < cur
                        // fsub.0 is dominator
                        if fsub.1 < fsub.0 {
                            if pdoms[*v].contains(&fsub.1) {
                                edges_will_be_removed.insert((*v, cur));
                            }
                        } else {
                            if doms[cur].contains(&fsub.1) {
                                edges_will_be_removed.insert((*v, cur));
                            }
                        }
                        cur = next;
                        break;
                    }
                }
            }
            res = Expr::new_binop(Bop::Add, res, temp_expr, env);
        }
        // いらないのを消す
        for (to, from) in edges_will_be_removed {
            for i in 0..self.graph[from].len() {
                if self.graph[from][i].to == to {
                    self.graph[from].remove(i);
                    break;
                }
            }
            for i in 0..self.reverse_graph[to].len() {
                if self.reverse_graph[to][i].to == from {
                    self.reverse_graph[to].remove(i);
                    break;
                }
            }
        }
        res = res.reduce(env);
        let new_edge = Edge {
            to: goal,
            exp: res.clone(),
        };
        let new_redge = Edge {
            to: start,
            exp: res,
        };
        self.graph[start].push(new_edge);
        self.reverse_graph[goal].push(new_redge);
        // Edgeを消す, 足す
    }
}
#[test]
fn create_diff_graph() {
    let e = &Environment::new();
    // let res = expr().parse("log(x) * cos(x) * cos(cos(x))", e);
    let res = expr().parse("sin(cos(x)) * cos(cos(x))", e);
    match res {
        Ok((_, _, (expr, env))) => {
            expr.diff("x", env).reduce(env).print(env);
            let mut d = Deriv::new(expr, env, "x");
            env.borrow_mut().clean();
            for (i, l) in d.graph.iter().enumerate() {
                for e in l {
                    println!("{} -> {}", i, e.to);
                    e.exp.print(env);
                }
            }
            println!("rev");
            for (i, l) in d.reverse_graph.iter().enumerate() {
                for e in l {
                    println!("{} -> {}", i, e.to);
                    e.exp.print(env);
                }
            }
            let doms = d.dom_rel();
            let pdoms = d.pdom_rel();
            let factor_subgraphs = d.factor_subgraphs(&doms, &pdoms);
            for fsub in factor_subgraphs {
                d.shrink(fsub, &doms, &pdoms, env);
            }
            for (i, l) in d.graph.iter().enumerate() {
                for e in l {
                    println!("{} -> {}", i, e.to);
                    e.exp.print(env);
                }
            }
            for (i, l) in d.reverse_graph.iter().enumerate() {
                for e in l {
                    println!("{} -> {}", i, e.to);
                    e.exp.print(env);
                }
            }
        }
        Err(_) => panic!(""),
    }
}
