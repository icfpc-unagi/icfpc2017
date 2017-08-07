use ::common::*;
use ::lib::unionfind::UnionFind;
use ::State;

#[derive(Serialize, Deserialize, Debug, Default)]
struct AI {
	dist: Vec<Vec<usize>>
}

pub fn setup(state: &mut State) {
	eprintln!("randw");
	let g = state.graph.iter().map(|u| u.iter().map(|&(v, _)| v).collect()).collect();
	state.ai = ::serde_json::to_string(&AI { dist: state.mines.iter().map(|&v| ::lib::bfs(&g, v)).collect() }).unwrap();
}

const R: usize = 10;

pub fn play(state: &mut State) -> usize {
	let ai: AI = ::serde_json::from_str(&state.ai).unwrap();
	let n = state.graph.len();
	let m = state.es.len();
	let mut user = vec![!0; state.es.len()];
	let mut opt = vec![!0; state.es.len()];
	for &(q, e) in &state.moves {
		if user[e] == !0 {
			user[e] = q;
		} else if opt[e] == !0 {
			opt[e] = q;
		}
	}
	use ::rand::Rng;
	let mut score: Vec<_> = user.iter().map(|&u| if u == !0 { 0.0 } else { -1.0 }).collect();
	let mut rng = ::rand::XorShiftRng::new_unseeded();
	let mut ws = vec![0.0; m];
	for i in 0..state.mines.len() {
		for _ in 0..R {
			use ::lib::dijkstra::Graph;
			let mut g: Graph<f64> = Graph::new(n);
			for u in 0..n {
				for &(v, e) in &state.graph[u] {
					if user[e] == !0 && u < v {
						let w = -(rng.next_f64().ln());
						// let w = rng.next_f64();
						g.add(u, v, w);
						g.add(v, u, w);
					} else if user[e] == state.my {
						g.add(u, v, 0.0);
					}
				}
			}
			let (list, dp) = g.solve(state.mines[i]);
			let mut sum = vec![0.0; n];
			for u in list.into_iter().rev() {
				if dp[u].0 > 0.0 {
					sum[u] += (ai.dist[i][u] * ai.dist[i][u]) as f64;// / (::std::f64::consts::E + dp[u].1 as f64).ln();
					let v = dp[u].1;
					sum[v] += sum[u];
					let e = state.graph[u][state.graph[u].binary_search_by(|&(w, _)| w.cmp(&v)).unwrap()].1;
					if user[e] == !0 {
						score[e] += sum[u] as f64 / R as f64;
					}
				}
			}
		}
	}
	let mut e = 0;
	for i in 0..m {
		if score[e] < score[i] {
			e = i;
		}
	}
	debug!(score[e]);
	e
}
