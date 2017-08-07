use ::common::*;
use ::lib::unionfind::UnionFind;
use ::State;

#[derive(Serialize, Deserialize, Debug, Default)]
struct AI {
}

pub fn setup(state: &mut State) {
	eprintln!("shortest2");
}

fn get_graph(graph: &Vec<Vec<(usize, usize)>>, user: &Vec<usize>, opt: &Vec<usize>, i: usize) -> (Vec<Vec<(usize, usize)>>, Vec<usize>) {
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
				if (user[e] == i || opt[e] == i) && id[v] == !0 {
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
	let mut opt = vec![!0; state.es.len()];
	let mut opt_n = vec![0; state.p];
	for &(q, e) in &state.moves {
		if user[e] == !0 {
			user[e] = q;
		} else if opt[e] == !0 {
			opt[e] = q;
			opt_n[q] += 1;
		}
	}
	let mut score = vec![0.0; m];
	for e in 0..m {
		if user[e] != !0 { score[e] = -1.0 }
	}
	let ex = 0.4 + 0.5 * (state.es.len() - state.turn) as f64 / state.es.len() as f64;
	for q in 0..state.p {
		let (g, id) = get_graph(&state.graph, &user, &opt, q);
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
				sum[id[v]] += (dist[i][v] * dist[i][v]) as f64 * ex.powf(ds[id[v]] as f64);
			}
			for &u in que[..qt].iter().rev() {
				let mut count: usize = 0;
				for &(v, _) in &g[u] {
					if ds[v] + 1 == ds[u] {
						count += 1;
					}
				}
				for &(v, e) in &g[u] {
					if ds[v] + 1 == ds[u] {
						let w = sum[u] as f64 / count as f64;
						if q == state.my {
							score[e] += w;
						} else {
							score[e] += w / (state.p - 1) as f64;
						}
						sum[v] += w;
					}
				}
			}
		}
	}
	let mut connected = vec![!0; n];
	let mut stack = vec![];
	for &s in &state.mines {
		if connected[s] != !0 { continue }
		connected[s] = s;
		stack.push(s);
		while let Some(u) = stack.pop() {
			for &(v, e) in &state.graph[u] {
				if (user[e] == state.my || opt[e] == state.my) && connected[v] == !0 {
					connected[v] = s;
					stack.push(v);
				}
			}
		}
	}
	let w = if state.p <= 4 { 1.0 } else if state.p <= 8 { 1.2 } else { 2.0 };
	for &u in &state.mines {
		for &(_, e) in &state.graph[u] {
			if connected[state.es[e].0] != connected[state.es[e].1] {
				score[e] *= w;
			}
		}
	}
	for u in 0..n {
		for &(_, e) in &state.graph[u] {
			if connected[state.es[e].0] != connected[state.es[e].1] {
				score[e] *= w;
			}
		}
	}
	if state.settings.options {
		let mut uf = UnionFind::new(n);
		for u in 0..n {
			for &(v, e) in &state.graph[u] {
				if user[e] == state.my {
					uf.unite(u, v);
				}
			}
		}
		let mut score_opt = vec![0; state.es.len()];
		for e in 0..state.es.len() {
			if user[e] == !0 || opt[e] != !0 { score_opt[e] = -1; }
		}
		for i in 0..state.mines.len() {
			let mut total = vec![0; n];
			for v in 0..n {
				total[uf.find(v)] += (dist[i][v] * dist[i][v]) as i64;
			}
			for e in 0..state.es.len() {
				let (u, v) = state.es[e];
				if user[e] == !0 || opt[e] != !0 || uf.same(u, v) { continue }
				if uf.same(state.mines[i], u) {
					score_opt[e] += total[v];
				}
				if uf.same(state.mines[i], v) {
					score_opt[e] += total[u];
				}
			}
		}
		let mut e = 0;
		for i in 0..m {
			if score[e] < score[i] {
				e = i;
			}
		}
		let mut e2 = 0;
		for i in 0..m {
			if score_opt[e2] < score_opt[i] {
				e2 = i;
			}
		}
		if opt_n[state.my] < state.mines.len() && score[e] < score_opt[e2] as f64 {
			e = e2;
		}
		e
	} else {
		let mut e = 0;
		for i in 0..m {
			if score[e] < score[i] {
				e = i;
			}
		}
		e
	}
}
