#![allow(unused)]

extern crate serde;
extern crate serde_json;

#[macro_use]
extern crate serde_derive;

#[macro_use]
mod common;
mod lib;
mod ai;

use ai::greedy as strategy;

#[derive(Serialize, Deserialize, Debug, Default)]
struct Input {
	punter: Option<usize>,
	punters: Option<usize>,
	map: Option<Map>,
	#[serde(rename="move")]
	move_: Option<Moves>,
	state: Option<State>
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
struct Moves {
	moves: Vec<Move>
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Move {
	claim: Option<Claim>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Claim {
	punter: usize,
	source: i64,
	target: i64
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
struct State {
	p: usize,
	my: usize,
	graph: Vec<Vec<usize>>,
	mines: Vec<usize>,
	moves: Vec<Option<(usize, usize)>>,
	names: Vec<i64>,
	ai: strategy::AI
}

fn setup(input: Input) -> Ready {
	let mut names = vec![];
	let map = input.map.unwrap();
	for s in map.sites {
		names.push(s.id);
	}
	names.sort();
	names.dedup();
	let n = names.len();
	let mut graph = vec![vec![]; n];
	for e in map.rivers {
		let s = names.binary_search(&e.source).unwrap();
		let t = names.binary_search(&e.target).unwrap();
		graph[s].push(t);
		graph[t].push(s);
	}
	for i in 0..n {
		graph[i].sort();
	}
	let mut mines: Vec<_> = map.mines.iter().map(|m| names.binary_search(&m).unwrap()).collect();
	mines.sort();
	let p = input.punters.unwrap();
	let my = input.punter.unwrap();
	let ai = strategy::setup(p, my, graph.clone(), mines.clone());
	Ready { ready: my, state: State { p, my, graph, mines, moves: vec![], names, ai } }
}

fn play(input: Input) -> Play {
	let moves = input.move_.unwrap().moves;
	let p = moves.len();
	let mut last = vec![None; p];
	let mut state = input.state.unwrap();
	for m in moves {
		if let Some(Claim{punter, source, target}) = m.claim {
			last[punter] = Some((state.names.binary_search(&source).unwrap(), state.names.binary_search(&target).unwrap()));
		}
	}
	if state.moves.len() == 0 {
		for i in 0..state.my {
			state.moves.push(last[i]);
		}
	} else {
		for i in 1..state.p {
			state.moves.push(last[(state.my + i) % state.p]);
		}
	}
	let (s, t, ai) = strategy::play(state.p, state.my, state.graph.clone(), state.mines.clone(), state.moves.clone(), state.ai);
	let claim = Claim { punter: state.my, source: state.names[s], target: state.names[t] };
	state.moves.push(Some((s, t)));
	Play { claim, state: State { ai, .. state} }
}

fn self_battle() {
	use std::io::Read;
	let map = std::env::args().nth(1).unwrap();
	let p = std::env::args().nth(2).unwrap().parse().unwrap();
	let file = std::fs::File::open(map).unwrap();
	let mut reader = std::io::BufReader::new(file);
	let mut input = String::new();
	reader.read_to_string(&mut input).unwrap();
	let map: Map = serde_json::from_str(&input).unwrap();
	let mut states: Vec<_> = (0..p).map(|i| Some(setup(Input {
		punter: Some(i),
		punters: Some(p),
		map: Some(map.clone()),
		.. Default::default()
	} ).state)).collect();
	let mut moves = vec![Move { claim: None }; p];
	for i in 0..map.rivers.len() {
		let play = play(Input {
			move_: Some(Moves { moves: moves.clone() }),
			state: Some(std::mem::replace(&mut states[i % p], None).unwrap()),
			.. Default::default()
		});
		eprintln!("{}: {} {}", play.claim.punter, play.claim.source, play.claim.target);
		moves[i % p] = Move { claim: Some(play.claim) };
		states[i % p] = Some(play.state);
	}
}

fn main() {
	if std::env::args().count() > 1 {
		self_battle();
		return;
	}
	use std::io::Read;
	let mut input = String::new();
	std::io::stdin().read_to_string(&mut input).unwrap();
	let p = input.find(':').unwrap() + 1;
	input = input[p..].to_owned();
	let input: Input = serde_json::from_str(&input).unwrap();
	if input.map.is_some() {
		let out = serde_json::to_string(&setup(input)).unwrap();
		println!("{}", out);
	} else if input.move_.is_some() {
		let out = serde_json::to_string(&play(input)).unwrap();
		println!("{}", out);
	} else {
		panic!("Invalid input: {:?}", input);
	}
}
