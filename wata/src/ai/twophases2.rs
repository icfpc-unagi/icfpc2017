use ::common::*;
use ::lib::unionfind::UnionFind;
use ::State;

#[derive(Serialize, Deserialize, Debug, Default)]
struct AI {
}

pub fn setup(state: &mut State) {
	eprintln!("twophases2");
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
	let mut opt = vec![!0; state.es.len()];
	for &(q, e) in &state.moves {
		if user[e] == !0 {
			user[e] = q;
		} else if opt[e] == !0 {
			opt[e] = q;
		}
	}
	let mut score = vec![0.0; m];
	for e in 0..m {
		if user[e] != !0 { score[e] = -1.0 }
	}
	{
		let (g, id) = get_graph(&state.graph, &user, state.my);
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
				// if ds[id[v]] <= dist[i][v] {
					// sum[id[v]] += (dist[i][v] * dist[i][v]) as f64;
					sum[id[v]] += (dist[i][v] * dist[i][v]) as f64 * 0.8f64.powf(ds[id[v]] as f64);
				// }
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
						score[e] += w;
						sum[v] += w;
					}
				}
			}
		}
	}
	let mut sorted: Vec<_> = score.iter().enumerate().map(|(e, &s)| (Rev(s), e)).collect();
	sorted.sort();
	let mut ret = sorted[0].1;
	let mut maxscore = -1e50;
	'lp: for r in 0..10 {
		if r >= sorted.len() || (sorted[r].0).0 < 0.0 { break }
		let e = sorted[r].1;
		let mut score = 0.0;
		user[e] = !1;
		for q in 0..state.p {
			let (g, id) = get_graph(&state.graph, &user, q);
			let g: Vec<Vec<_>> = g.into_iter().map(|v| v.into_iter().map(|(w, _)| w).collect()).collect();
			let n = g.len();
			for (i, &s_) in state.mines.iter().enumerate() {
				let d = ::STIME().elapsed().unwrap();
				let s = d.as_secs() as f64 + d.subsec_nanos() as f64 * 1e-9;
				if s > 0.8 {
					eprintln!("break: {}", s);
					break 'lp;
				}
				let s = id[s_];
				let ds = ::lib::bfs(&g, s);
				for v in 0..id.len() {
					if ds[id[v]] <= n {
						if q == state.my {
							score -= (dist[i][v] * dist[i][v]) as f64 * 0.8f64.powf(ds[id[v]] as f64);
						} else {
							score -= (dist[i][v] * dist[i][v]) as f64 * 0.8f64.powf(ds[id[v]] as f64) / (state.p - 1) as f64;
						}
					}
				}
			}
		}
		user[e] = !0;
		if maxscore.setmax(score) {
			ret = e;
		}
	}
	debug!(maxscore);
	ret
}
