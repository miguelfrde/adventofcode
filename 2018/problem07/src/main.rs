extern crate regex;

use std::{
    cmp::Reverse,
    collections::{
        BinaryHeap,
        HashMap,
        HashSet,
    },
    fs::File,
    io::{self, prelude::*, BufReader},
    str::FromStr,
};
use regex::Regex;

#[derive(PartialEq,Clone,Debug)]
struct Instruction {
    from: char,
    to: char,
}

struct Worker {
	time: u32,
    node: char,
}

type Graph = HashMap<char, HashSet<char>>;

impl FromStr for Instruction {
    type Err = regex::Error;

	fn from_str(repr: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"Step ([A-Z]) must be finished before step ([A-Z]) can begin.").unwrap();
        let caps = match re.captures(repr) {
			None => return Err(regex::Error::Syntax(repr.to_string())),
            Some(caps) => caps,
        };
        let cap = |n| caps.get(n).unwrap().as_str().chars().next().unwrap();
        Ok(Instruction {
            from: cap(1),
            to: cap(2),
        })
    }
}

impl Worker {
	fn new(node: char, delay: u32) -> Self {
		Worker {
			time: delay + (node as u32) - ('A' as u32) + 1,
			node: node,
		}
    }

	fn step(&mut self) {
		self.time -= 1;
    }

    fn done(&self) -> bool {
		self.time == 0
    }
}

fn build_out_graph(instructions: &Vec<Instruction>) -> Graph {
    let mut graph = Graph::new();
    for instruction in instructions {
        graph.entry(instruction.from)
            .or_insert(HashSet::<char>::new())
            .insert(instruction.to);
        graph.entry(instruction.to).or_insert(HashSet::<char>::new());
    }
    graph
}

fn build_in_graph(instructions: &Vec<Instruction>) -> Graph {
    let mut graph = Graph::new();

    for instruction in instructions {
        graph.entry(instruction.to)
            .or_insert(HashSet::<char>::new())
            .insert(instruction.from);
        graph.entry(instruction.from).or_insert(HashSet::<char>::new());
    }
    graph
}

fn init_queue(in_graph: &Graph) -> BinaryHeap<Reverse<char>> {
	in_graph
        .iter()
        .filter(|(_, in_edges)| in_edges.is_empty())
        .map(|(node, _)| Reverse(*node))
        .collect::<BinaryHeap<Reverse<char>>>()
}

fn part1(in_graph: &Graph, out_graph: &Graph) -> String {
	let mut queue = init_queue(in_graph);
    let mut order = String::new();
    while !queue.is_empty() {
        let c = queue.pop().unwrap().0;
        order.push(c);
        for node in out_graph[&c].iter() {
            let deps_visited = in_graph[&node].iter().all(|n| order.contains(*n));
            if deps_visited {
                queue.push(Reverse(*node));
            }
        }
    }
    order
}

fn process_with_workers(in_graph: &Graph, out_graph: &Graph, delay: u32, n_workers: usize) -> u32 {
	let mut queue = init_queue(in_graph);
    let mut workers = Vec::<Worker>::new();
	let mut visited = HashSet::<char>::new();

    workers.reserve(5);
	for sec in 0.. {
		for mut worker in workers.iter_mut() {
			worker.step();
			if worker.done() {
				visited.insert(worker.node);
        		for node in out_graph[&worker.node].iter() {
        		    let deps_visited = in_graph[&node].iter().all(|n| visited.contains(n));
        		    if deps_visited {
        		        queue.push(Reverse(*node));
        		    }
        		}
			}
		}
		workers.retain(|worker| !worker.done());
    	while !queue.is_empty() && workers.len() < n_workers {
        	let c = queue.pop().unwrap().0;
			workers.push(Worker::new(c, delay));
		}
		if workers.is_empty() {
			return sec;
		}
	}
	0
}

fn part2(in_graph: &Graph, out_graph: &Graph) -> u32 {
	process_with_workers(in_graph, out_graph, 60, 5)
}

fn main() -> io::Result<()> {
    let f = File::open("input.txt")?;
    let reader = BufReader::new(f);

    let instructions : Vec<Instruction> = reader
        .lines()
        .map(|line| Instruction::from_str(&line.unwrap()).unwrap())
        .collect();
    let in_graph = build_in_graph(&instructions);
    let out_graph = build_out_graph(&instructions);

    println!("Part 1: {}", part1(&in_graph, &out_graph));
    println!("Part 2: {}", part2(&in_graph, &out_graph));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::FromIterator;

    const INSTRUCTIONS : [Instruction; 7] = [
        Instruction { from: 'C', to: 'A' },
        Instruction { from: 'C', to: 'F' },
        Instruction { from: 'A', to: 'B' },
        Instruction { from: 'A', to: 'D' },
        Instruction { from: 'B', to: 'E' },
        Instruction { from: 'D', to: 'E' },
        Instruction { from: 'F', to: 'E' },
    ];

    #[test]
    fn test_instruction_from_str() {
        let instruction = Instruction { from: 'C', to: 'A' };
        assert_eq!(instruction,
                   Instruction::from_str("Step C must be finished before step A can begin.").unwrap());
    }

    #[test]
    fn test_build_out_graph() {
        let graph = build_out_graph(&INSTRUCTIONS.to_vec());
        assert_eq!(HashSet::from_iter(vec!['A', 'F']), *graph.get(&'C').unwrap());
        assert_eq!(HashSet::from_iter(vec!['B', 'D']), *graph.get(&'A').unwrap());
        assert_eq!(HashSet::from_iter(vec!['E']), *graph.get(&'F').unwrap());
        assert_eq!(HashSet::from_iter(vec!['E']), *graph.get(&'B').unwrap());
        assert_eq!(HashSet::from_iter(vec!['E']), *graph.get(&'D').unwrap());
        assert_eq!(HashSet::from_iter(vec![]), *graph.get(&'E').unwrap());
    }

    #[test]
    fn test_build_in_graph() {
        let graph = build_in_graph(&INSTRUCTIONS.to_vec());
        assert_eq!(HashSet::from_iter(vec!['B', 'D', 'F']), *graph.get(&'E').unwrap());
        assert_eq!(HashSet::from_iter(vec!['A']), *graph.get(&'B').unwrap());
        assert_eq!(HashSet::from_iter(vec!['A']), *graph.get(&'D').unwrap());
        assert_eq!(HashSet::from_iter(vec!['C']), *graph.get(&'F').unwrap());
        assert_eq!(HashSet::from_iter(vec!['C']), *graph.get(&'A').unwrap());
        assert_eq!(HashSet::from_iter(vec![]), *graph.get(&'C').unwrap());
    }

    #[test]
    fn test_part1() {
        let in_graph = build_in_graph(&INSTRUCTIONS.to_vec());
        let out_graph = build_out_graph(&INSTRUCTIONS.to_vec());
        assert_eq!("CABDFE", part1(&in_graph, &out_graph));
    }

	#[test]
	fn test_process_with_workers() {
		let in_graph = build_in_graph(&INSTRUCTIONS.to_vec());
		let out_graph = build_out_graph(&INSTRUCTIONS.to_vec());
		assert_eq!(15, process_with_workers(&in_graph, &out_graph, 0, 2));
	}
}
