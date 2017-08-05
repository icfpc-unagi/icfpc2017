use ::common::*;
use ::lib::unionfind::UnionFind;
use ::State;

#[derive(Serialize, Deserialize, Debug, Default)]
struct AI {
	dist: Vec<Vec<usize>>
}

pub fn setup(state: &mut State) {
	eprintln!("greedy");
	let g = state.graph.iter().map(|u| u.iter().map(|&(v, _)| v).collect()).collect();
	state.ai = ::serde_json::to_string(&AI { dist: state.mines.iter().map(|&v| ::lib::bfs(&g, v)).collect() }).unwrap();
}

fn score(state: &State, uf: &UnionFind, u: usize, v: usize) -> i64 {
	let ai: AI = ::serde_json::from_str(&state.ai).unwrap();
	let n = state.graph.len();
	if uf.same(u, v) {
		return 0;
	}
	let mut uf = uf.clone();
	uf.unite(u, v);
	let mut score = 0;
	for (i, &x) in state.mines.iter().enumerate() {
		for y in 0..n {
			if uf.same(x, y) {
				score += ai.dist[i][y] * ai.dist[i][y];
			}
		}
	}
	score as i64
}

pub fn play(state: &mut State) -> usize {
	let n = state.graph.len();
	let mut user = vec![!0; state.es.len()];
	for (i, &mov) in state.moves.iter().enumerate() {
		if let Some(e) = mov {
			user[e] = i % state.p;
		}
	}
	let mut uf = UnionFind::new(n);
	for u in 0..n {
		for &(v, e) in &state.graph[u] {
			if user[e] == state.my {
				uf.unite(u, v);
			}
		}
	}
	let mut max = -1;
	let mut ret = !0;
	for u in 0..n {
		for &(v, e) in &state.graph[u] {
			if user[e] == !0 {
				if max.setmax(score(&state, &uf, u, v)) {
					ret = e;
				}
			}
		}
	}
	debug!(max, user[ret]);
	ret
}
