pub mod unionfind;

pub fn current_graph(p: usize, graph: &Vec<Vec<usize>>, moves: &Vec<Option<(usize, usize)>>) -> Vec<Vec<(usize, usize)>> {
	let n = graph.len();
	let mut g = vec![vec![]; n];
	for i in 0..n {
		for &j in &graph[i] {
			g[i].push((j, !0));
		}
	}
	for i in 0..moves.len() {
		if let Some((u, v)) = moves[i] {
			let x = graph[u].binary_search(&v).unwrap();
			g[u][x].1 = i % p;
			let x = graph[v].binary_search(&u).unwrap();
			g[v][x].1 = i % p;
		}
	}
	g
}

pub fn bfs(g: &Vec<Vec<usize>>, s: usize) -> Vec<usize> {
	let n = g.len();
	let mut dist = vec![!0; n];
	let mut que = ::std::collections::VecDeque::new();
	dist[s] = 0;
	que.push_back(s);
	while let Some(u) = que.pop_front() {
		for &v in &g[u] {
			if dist[v] == !0 {
				dist[v] = dist[u] + 1;
				que.push_back(v);
			}
		}
	}
	dist
}
