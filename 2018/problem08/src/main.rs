use std::{
    collections::VecDeque,
    fs::File,
    io::{self, prelude::*, BufReader},
};

#[derive(PartialEq, Debug)]
struct Node {
    children: Vec<Node>,
    metadata: Vec<u32>,
}

type Tree = Node;

impl Tree {
    pub fn build(input: Vec<u32>) -> Tree {
        let mut queue = VecDeque::<u32>::from(input);
        fn build_node(input: &mut VecDeque<u32>) -> Node {
            let num_children = input.pop_front().unwrap();
            let num_metadata = input.pop_front().unwrap();
            let mut node = Node {
                children: Vec::new(),
                metadata: Vec::new(),
            };
            for _ in 0..num_children {
                node.children.push(build_node(input));
            }
            for _ in 0..num_metadata {
                node.metadata.push(input.pop_front().unwrap());
            }
            node
        };
        build_node(&mut queue)
    }
}

fn part1(tree: &Tree) -> u32 {
    fn sum_node(node: &Node) -> u32 {
        node.metadata.iter().sum::<u32>() + node.children.iter().fold(0, |acc, n| acc + sum_node(n))
    }
    sum_node(tree)
}

fn part2(tree: &Tree) -> u32 {
    fn sum_node(node: &Node) -> u32 {
        if node.children.is_empty() {
            return node.metadata.iter().sum::<u32>();
        }
        node.metadata
            .iter()
            .filter(|&&m| m > 0 && m <= node.children.len() as u32)
            .map(|m| &node.children[(m - 1) as usize])
            .map(sum_node)
            .sum::<u32>()
    }
    sum_node(tree)
}

fn main() -> io::Result<()> {
    let f = File::open("input.txt")?;
    let reader = BufReader::new(f);

    let input: Vec<u32> = reader
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .split(' ')
        .map(|num| num.parse::<u32>().unwrap())
        .collect();

    let tree = Tree::build(input);

    println!("Part 1: {}", part1(&tree));
    println!("Part 2: {}", part2(&tree));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: [u32; 16] = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2];

    #[test]
    fn test_build_tree() {
        let tree = Node {
            children: vec![
                Node {
                    children: vec![],
                    metadata: vec![10, 11, 12],
                },
                Node {
                    children: vec![Node {
                        children: vec![],
                        metadata: vec![99],
                    }],
                    metadata: vec![2],
                },
            ],
            metadata: vec![1, 1, 2],
        };
        assert_eq!(tree, Tree::build(INPUT.to_vec()));
    }

    #[test]
    fn test_part1() {
        assert_eq!(138, part1(&Tree::build(INPUT.to_vec())));
    }

    #[test]
    fn test_part2() {
        assert_eq!(66, part2(&Tree::build(INPUT.to_vec())));
    }
}
