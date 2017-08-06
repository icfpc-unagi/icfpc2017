use ::common::*;
use ::lib::unionfind::UnionFind;
use ::State;

#[derive(Serialize, Deserialize, Debug, Default)]
struct AI {
}

pub fn setup(state: &mut State) {
	eprintln!("farthest");
	state.ai = "".to_owned();
}

fn get_graph(graph: &Vec<Vec<(usize, usize)>>, user: &Vec<usize>, i: usize) -> (Vec<Vec<(usize, usize)>>, Vec<usize>) {
	let n = graph.len();
	let mut id = vec![!0; n];
	let mut stack = vec![];
	let mut n2 = 0;
	for s in 0..n {
		if id[s] != !0 { continue }
		id[s] = n2;
		stack.push(s);
		while let Some(u) = stack.pop() {
			for &(v, e) in &graph[u] {
				if user[e] == i && id[v] == !0 {
					id[v] = n2;
					stack.push(v);
				}
			}
		}
		n2 += 1;
	}
	let mut g = vec![vec![]; n2];
	for u in 0..n {
		for &(v, e) in &graph[u] {
			if user[e] == !0 && id[u] != id[v] {
				g[id[u]].push((id[v], e));
			}
		}
	}
	(g, id)
}

pub fn play(state: &mut State) -> usize {
	let g = state.graph.iter().map(|u| u.iter().map(|&(v, _)| v).collect()).collect();
	let dist: Vec<_> = state.mines.iter().map(|&v| ::lib::bfs(&g, v)).collect();
	let n = state.graph.len();
	let m = state.es.len();
	let mut user = vec![!0; state.es.len()];
	for (i, &mov) in state.moves.iter().enumerate() {
		if let Some(e) = mov {
			user[e] = i % state.p;
		}
	}
	let mut score = vec![0.0; m];
	for e in 0..m {
		if user[e] != !0 { score[e] = -1.0 }
	}
	for q in 0..state.p {
		let (g, id) = get_graph(&state.graph, &user, q);
		let n = g.len();
		for (i, &s_) in state.mines.iter().enumerate() {
			let s = id[s_];
			let (mut qs, mut qt) = (0, 0);
			let mut que = vec![!0; n];
			let mut ds = vec![!0; n];
			ds[s] = 0;
			que[qt] = s;
			qt += 1;
			while qs < qt {
				let u = que[qs];
				qs += 1;
				for &(v, _) in &g[u] {
					if ds[v] == !0 {
						ds[v] = ds[u] + 1;
						que[qt] = v;
						qt += 1;
					}
				}
			}
			let mut sum = vec![0.0; n];
			for v in 0..id.len() {
				sum[id[v]] += (dist[i][v] * dist[i][v]) as f64;
			}
			for &u in que[..qt].iter().rev() {
				let mut max = 0.0;
				for &(v, e) in &g[u] {
					if ds[u] + 1 == ds[v] {
						max.setmax(sum[v]);
					}
				}
				sum[u] += max;
			}
			let mut ws = vec![0.0; n];
			ws[s] = 1.0;
			for &u in que[..qt].iter() {
				for &(v, e) in &g[u] {
					
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
