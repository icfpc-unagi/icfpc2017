use std::ops::*;

type V = usize;

#[derive(Copy, Clone, Debug)]
pub struct E<W> {
	pub to: V,
	pub cost: W,
}

#[derive(Clone, Debug)]
pub struct Graph<W> {
	pub es: Vec<Vec<E<W>>>,
}

struct Entry<W>(V, W);
impl_cmp!(Entry<W>; |a, b| b.1.partial_cmp(&a.1).unwrap(); where W: PartialOrd);

impl<W> Graph<W> where W: Copy + Default + PartialOrd + Add<Output = W> {
	pub fn new(n: usize) -> Graph<W> {
		Graph { es: vec![vec![]; n] }
	}
	pub fn add(&mut self, v: V, to: V, cost: W) {
		self.es[v].push(E { to: to, cost: cost });
	}
	/// Compute [(dist, prev); n].
	/// dist[v] := dist(s, v). dist[v] = 0 when v is unreachable.
	/// prev[s] = !1. prev[v] = !0 when v is unreachable.
	pub fn solve(&self, s: V) -> (Vec<V>, Vec<(W, V)>) {
		let n = self.es.len();
		let mut fixed = vec![false; n]; // For avoiding negative loops due to floating point error
		let mut dp = vec![(W::default(), !0); n];
		let mut que = ::std::collections::BinaryHeap::new();
		dp[s] = (W::default(), !1);
		que.push(Entry(s, W::default()));
		let mut list = vec![];
		while let Some(Entry(u, d)) = que.pop() {
			if fixed[u] { continue }
			fixed[u] = true;
			list.push(u);
			for e in &self.es[u] {
				let v = e.to;
				let d2 = d + e.cost;
				if !fixed[v] && (dp[v].1 == !0 || dp[v].0 > d2) {
					dp[v] = (d2, u);
					que.push(Entry(v, d2));
				}
			}
		}
		(list, dp)
	}
	pub fn get_path(dp: &[(W, V)], mut t: V) -> Vec<V> {
		if dp[t].1 == !0 { return vec![] }
		let mut path = vec![];
		while t != !1 {
			path.push(t);
			t = dp[t].1;
		}
		path.into_iter().rev().collect()
	}
}
