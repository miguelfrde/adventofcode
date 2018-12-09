use std::{
    collections::HashSet,
    fs::File,
    io::{self, prelude::*, BufReader},
    iter::FromIterator,
};

fn part2(polymer: &str) -> Option<usize> {
    let chars = HashSet::<char>::from_iter(polymer.to_lowercase().chars());
    chars.iter().map(|to_strip| {
        let simple : String = polymer
            .chars()
            .filter(|&c| !c.eq_ignore_ascii_case(to_strip))
            .collect();
        part1(&simple)
    }).min()
}

fn part1(polymer: &str) -> usize {
	polymer.chars().fold(
        Vec::<char>::new(),
        |mut res, unit| {
            if let Some(&c) = res.last() {
              if c != unit && c.eq_ignore_ascii_case(&unit) {
                res.pop();
                return res;
              }
            }
            res.push(unit);
            res
        }
    ).len()
}

fn main() -> io::Result<()> {
    let f = File::open("input.txt")?;
    let reader = BufReader::new(f);

    let polymer : String = reader
        .lines()
        .next()
        .unwrap()
        .unwrap();

    println!("Part 1: {}", part1(&polymer));
    println!("Part 2: {}", part2(&polymer).unwrap());

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(0, part1("aA"));
        assert_eq!(0, part1("abBA"));
        assert_eq!(4, part1("abAB"));
        assert_eq!(6, part1("aabAAB"));
        assert_eq!(10, part1("dabAcCaCBAcCcaDA"));
    }

    #[test]
    fn test_part2() {
        assert_eq!(4, part2("dabAcCaCBAcCcaDA").unwrap());
    }
}
