use std::cell::Cell;

#[derive(Clone, Debug)]
pub struct UnionFind {
	/// size / parent
	ps: Vec<Cell<usize>>,
	pub is_root: Vec<bool>
}

impl UnionFind {
	pub fn new(n: usize) -> UnionFind {
		UnionFind { ps: vec![Cell::new(1); n], is_root: vec![true; n] }
	}
	pub fn find(&self, x: usize) -> usize {
		if self.is_root[x] { x }
		else {
			let p = self.find(self.ps[x].get());
			self.ps[x].set(p);
			p
		}
	}
	pub fn unite(&mut self, x: usize, y: usize) {
		let mut x = self.find(x);
		let mut y = self.find(y);
		if x == y { return }
		if self.ps[x].get() < self.ps[y].get() {
			::std::mem::swap(&mut x, &mut y);
		}
		*self.ps[x].get_mut() += self.ps[y].get();
		self.ps[y].set(x);
		self.is_root[y] = false;
	}
	pub fn same(&self, x: usize, y: usize) -> bool {
		self.find(x) == self.find(y)
	}
	pub fn size(&self, x: usize) -> usize {
		self.ps[self.find(x)].get()
	}
}
