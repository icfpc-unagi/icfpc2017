pub mod unionfind;
pub mod dijkstra;

pub fn bfs(g: &Vec<Vec<usize>>, s: usize) -> Vec<usize> {
	let n = g.len();
	let mut dist = vec![n + 1; n];
	let mut que = ::std::collections::VecDeque::new();
	dist[s] = 0;
	que.push_back(s);
	while let Some(u) = que.pop_front() {
		for &v in &g[u] {
			if dist[v] == n + 1 {
				dist[v] = dist[u] + 1;
				que.push_back(v);
			}
		}
	}
	dist
}
