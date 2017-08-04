use ::common::*;
use ::lib::unionfind::UnionFind;

#[derive(Serialize, Deserialize, Debug)]
pub struct AI {
}

pub fn setup(p: usize, my: usize, graph: Vec<Vec<usize>>, mines: Vec<usize>) -> AI {
	AI { }
}

fn score(p: usize, my: usize, g: &Vec<Vec<(usize, usize)>>, mines: &Vec<usize>, dist: &Vec<Vec<usize>>, uf: &UnionFind, u: usize, v: usize) -> i64 {
	let n = g.len();
	if uf.same(u, v) {
		return 0;
	}
	let mut uf = uf.clone();
	uf.unite(u, v);
	let mut score = 0;
	for (i, &x) in mines.iter().enumerate() {
		for y in 0..n {
			if uf.same(x, y) {
				score += dist[i][y] * dist[i][y];
			}
		}
	}
	score as i64
}

pub fn play(p: usize, my: usize, graph: Vec<Vec<usize>>, mines: Vec<usize>, moves: Vec<Option<(usize, usize)>>, ai: AI) -> (usize, usize, AI) {
	let n = graph.len();
	let dist: Vec<_> = mines.iter().map(|&v| ::lib::bfs(&graph, v)).collect();
	let g = ::lib::current_graph(p, &graph, &moves);
	let mut uf = UnionFind::new(n);
	for u in 0..n {
		for &(v, i) in &g[u] {
			if i == my {
				uf.unite(u, v);
			}
		}
	}
	let mut max = -1;
	let mut ret = (!0, !0);
	for u in 0..n {
		for &(v, i) in &g[u] {
			if i == !0 {
				if max.setmax(score(p, my, &g, &mines, &dist, &uf, u, v)) {
					ret = (u, v);
				}
			}
		}
	}
	debug!(max);
	(ret.0, ret.1, ai)
}
