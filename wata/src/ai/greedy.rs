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

pub fn play(state: &mut State) -> usize {
	let ai: AI = ::serde_json::from_str(&state.ai).unwrap();
	let n = state.graph.len();
	let mut user = vec![!0; state.es.len()];
	let mut opt = vec![!0; state.es.len()];
	for &(q, e) in &state.moves {
		if user[e] == !0 {
			user[e] = q;
		} else if opt[e] == !0 {
			opt[e] = q;
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
	let mut score = vec![0; state.es.len()];
	for e in 0..state.es.len() {
		if user[e] != !0 { score[e] = -1; }
	}
	for i in 0..state.mines.len() {
		let mut total = vec![0; n];
		for v in 0..n {
			total[uf.find(v)] += (ai.dist[i][v] * ai.dist[i][v]) as i64;
		}
		for e in 0..state.es.len() {
			let (u, v) = state.es[e];
			if user[e] != !0 || uf.same(u, v) { continue }
			if uf.same(state.mines[i], u) {
				score[e] += total[v];
			}
			if uf.same(state.mines[i], v) {
				score[e] += total[u];
			}
		}
	}
	let mut e = 0;
	for i in 0..state.es.len() {
		if score[e] < score[i] {
			e = i;
		}
	}
	debug!(score[e]);
	e
}
