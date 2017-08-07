#![allow(unused)]

extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

extern crate rand;

#[macro_use]
mod common;
mod lib;
mod ai;

global!(STIME: std::time::SystemTime = std::time::SystemTime::now());

const DEFAULT_AI: &'static str = "lightning";

#[derive(Serialize, Deserialize, Debug, Default)]
struct Input {
	punter: Option<usize>,
	punters: Option<usize>,
	map: Option<Map>,
	settings: Option<Settings>,
	#[serde(rename="move")]
	move_: Option<Moves>,
	state: Option<State>,
	you: Option<String>
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Map {
	sites: Vec<Site>,
	rivers: Vec<River>,
	mines: Vec<i64>
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Site {
	id: i64
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct River {
	source: i64,
	target: i64
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Settings {
	futures: bool,
	splurges: bool,
	options: bool
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Moves {
	moves: Vec<Move>
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Move {
	claim: Option<Claim>,
	splurge: Option<Splurge>,
	option: Option<Claim>
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Claim {
	punter: usize,
	source: i64,
	target: i64
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Splurge {
	punter: usize,
	route: Vec<i64>
}

#[derive(Serialize, Deserialize, Debug)]
struct Ready {
	ready: usize,
	state: State
}

#[derive(Serialize, Deserialize, Debug)]
struct Play {
	claim: Claim,
	state: State
}

#[derive(Serialize, Deserialize, Debug)]
pub struct State {
	p: usize,
	my: usize,
	turn: usize,
	es: Vec<(usize, usize)>,
	graph: Vec<Vec<(usize, usize)>>,
	mines: Vec<usize>,
	moves: Vec<(usize, usize)>,
	names: Vec<i64>,
	settings: Settings,
	ai: String
}

macro_rules! ai {
	($ai:ident, $state:ident, $f:ident;$($name:ident),*) => (
		match $ai {
			$(
				stringify!($name) => ai::$name::$f(&mut $state),
			)*
			_ => panic!("unknown ai: {}", $ai)
		}
	)
}

fn setup(input: Input, ai: &str) -> Ready {
	let mut names = vec![];
	let map = input.map.unwrap();
	for s in map.sites {
		names.push(s.id);
	}
	names.sort();
	names.dedup();
	let n = names.len();
	let mut es = vec![];
	let mut graph = vec![vec![]; n];
	for (i, e) in map.rivers.into_iter().enumerate() {
		let s = names.binary_search(&e.source).unwrap();
		let t = names.binary_search(&e.target).unwrap();
		es.push((s, t));
		graph[s].push((t, i));
		graph[t].push((s, i));
	}
	for i in 0..n {
		graph[i].sort();
	}
	let mut mines: Vec<_> = map.mines.iter().map(|m| names.binary_search(&m).unwrap()).collect();
	mines.sort();
	let p = input.punters.unwrap();
	let my = input.punter.unwrap();
	let mut state = State { p, my, turn: my, es, graph, mines, moves: vec![], names, settings: input.settings.unwrap(), ai: Default::default() };
	ai!(ai, state, setup; greedy, randw, obst, lightning, shortest, weighted, connected, connected2, selfish, shortest2);
	Ready { ready: my, state }
}

fn play(input: Input, ai: &str) -> Play {
	let moves = input.move_.unwrap().moves;
	let p = moves.len();
	let mut state = input.state.unwrap();
	for i in 0..state.p {
		let q = (state.my + i) % state.p;
		if moves[q].claim.is_some() {
			let Claim{punter, source, target} = moves[q].claim.clone().unwrap();
			let u = state.names.binary_search(&source).unwrap();
			let v = state.names.binary_search(&target).unwrap();
			let e = state.graph[u].binary_search_by(|&(w, _)| w.cmp(&v)).unwrap();
			state.moves.push((q, state.graph[u][e].1));
		} else if moves[q].splurge.is_some() {
			let Splurge{punter, route} = moves[q].splurge.clone().unwrap();
			for j in 0..route.len() - 1 {
				let u = state.names.binary_search(&route[j]).unwrap();
				let v = state.names.binary_search(&route[j + 1]).unwrap();
				let e = state.graph[u].binary_search_by(|&(w, _)| w.cmp(&v)).unwrap();
				state.moves.push((q, state.graph[u][e].1));
			}
		} else if moves[q].option.is_some() {
			let Claim{punter, source, target} = moves[q].option.clone().unwrap();
			let u = state.names.binary_search(&source).unwrap();
			let v = state.names.binary_search(&target).unwrap();
			let e = state.graph[u].binary_search_by(|&(w, _)| w.cmp(&v)).unwrap();
			state.moves.push((q, state.graph[u][e].1));
		}
	}
	let e = ai!(ai, state, play; greedy, randw, obst, lightning, shortest, weighted, connected, connected2, selfish, shortest2);
	let claim = Claim { punter: state.my, source: state.names[state.es[e].0], target: state.names[state.es[e].1] };
	state.turn += state.p;
	eprintln!("{}: {} {}", state.my, claim.source, claim.target);
	Play { claim, state }
}

fn read(n: usize) -> Vec<u8> {
	use std::io::Read;
	let mut a = vec![0; n];
	let mut stdin = std::io::stdin();
	stdin.lock().read_exact(&mut a).unwrap();
	a
}

fn read_n() -> usize {
	use std::io::Read;
	let mut n = String::new();
	loop {
		let a = read(1);
		if a[0] == b':' { break }
		n += &String::from_utf8(a).unwrap();
	}
	n.trim().parse().unwrap()
}

fn read_next() -> String {
	let n = read_n();
	let input = String::from_utf8(read(n)).unwrap();
	let input = input.trim();
	input.to_owned()
}

fn send(s: &str) {
	println!("{}:{}", s.len(), s);
}

fn main() {
	send("{\"me\" : \"unagi\"}");
	let mut input: Input = serde_json::from_str(&read_next()).unwrap();
	*STIME() = ::std::time::SystemTime::now();
	if input.you.is_some() {
		input = serde_json::from_str(&read_next()).unwrap();
	}
	if input.map.is_some() {
		let out = serde_json::to_string(&setup(input, &std::env::args().nth(1).unwrap_or(DEFAULT_AI.to_owned()))).unwrap();
		send(&out);
	} else if input.move_.is_some() {
		let out = serde_json::to_string(&play(input, &std::env::args().nth(1).unwrap_or(DEFAULT_AI.to_owned()))).unwrap();
		send(&out);
	} else {
		panic!("Invalid input: {:?}", input);
	}
	let d = STIME().elapsed().unwrap();
	let s = d.as_secs() as f64 + d.subsec_nanos() as f64 * 1e-9;
	eprintln!("time: {:.3}", s);
}
