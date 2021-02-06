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
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Deriv {
    size: usize,
    root: usize,
    leafs: Vec<usize>,
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
            leafs,
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
    fn dom_rel(&self) -> Vec<Option<usize>> {
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
        doms
    }

    // pdomを求める.(複数のエントリーがある)
    // fn intersect_p(mut b1: usize, mut b2: usize, pdoms: &Vec<Option<usize>>) -> Option<usize> {
    //     while b1 != b2 {
    //         while b2 < b1 {
    //             if pdoms[b1].is_none() || b1 == pdoms[b1].unwrap() {
    //                 return None;
    //             } else {
    //                 b1 = pdoms[b1].expect("dominator intersection failure");
    //             }
    //         }
    //         while b1 < b2 {
    //             if pdoms[b2].is_none() || b2 == pdoms[b2].unwrap() {
    //                 return None;
    //             } else {
    //                 b2 = pdoms[b2].expect("dominator intersection failure");
    //             }
    //         }
    //     }
    //     b1
    // }
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

    fn factor_subgraphs(&self) -> Vec<(usize, usize)> {
        let domtree = self.dom_rel();
        let mut doms: Vec<HashSet<usize>> = vec![HashSet::new(); self.size];
        let pdoms = self.pdom_rel();
        let mut factor_dom_nodes: HashSet<usize> = HashSet::new();
        let mut factor_pdom_nodes: HashSet<usize> = HashSet::new();
        // 支配木をたどってfactorを探す
        for i in 0..self.size {
            // factor_dom, ついでにDomをHashSetに
            doms[i].insert(i);
            let mut cur = i;
            while cur != domtree[cur].unwrap() {
                let dom = domtree[cur].unwrap();
                doms[i].insert(dom);
                if 2 <= self.graph[dom].len() {
                    factor_dom_nodes.insert(dom);
                }
                cur = dom;
            }
            // factor_pdom
            if 2 <= self.reverse_graph[i].len() {
                factor_pdom_nodes.insert(i);
            }
        }
        let mut res = vec![];
        for fd in factor_dom_nodes {
            for n in 0..self.size {
                if n != fd && doms[n].contains(&fd) && 2 <= self.reverse_graph[n].len() {
                    res.push((fd, n));
                }
            }
        }
        for fpd in factor_pdom_nodes {
            for n in 0..self.size {
                if n != fpd && pdoms[n].contains(&fpd) && 2 <= self.graph[n].len() {
                    res.push((n, fpd));
                }
            }
        }
        res
    }
}
#[test]
fn create_diff_graph() {
    let e = &Environment::new();
    let res = expr().parse("log(x) * cos(x) * cos(cos(x))", e);
    match res {
        Ok((_, _, (expr, env))) => {
            expr.diff("x", env).reduce(env).print(env);
            let d = Deriv::new(expr, env, "x");
            env.borrow_mut().clean();
            println!("{:?}", env);
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
            println!("{:?}", doms);
            println!("{:?}", pdoms);
            let fsub = d.factor_subgraphs();
            println!("{:?}", fsub);
        }
        Err(_) => panic!(""),
    }
}
