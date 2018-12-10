extern crate regex;

use std::{
    cmp::{max, min},
    collections::HashMap,
    fs::File,
    io::{self, prelude::*, BufReader},
    str::FromStr,
};

use regex::Regex;

#[derive(PartialEq, Debug)]
struct Claim {
    id: u32,
    x: u32,
    y: u32,
    width: u32,
    height: u32,
}

struct ClaimPointsIterator<'c> {
    claim: &'c Claim,
    x: u32,
    y: u32,
}

impl Claim {
    pub fn points(&self) -> ClaimPointsIterator {
        ClaimPointsIterator {
            claim: self,
            x: self.x,
            y: self.y,
        }
    }

    pub fn overlaps(&self, other: &Claim) -> bool {
        min(self.x + self.width, other.x + other.width) > max(self.x, other.x)
            && min(self.y + self.height, other.y + other.height) > max(self.y, other.y)
    }
}

impl<'c> Iterator for ClaimPointsIterator<'c> {
    type Item = (u32, u32);

    fn next(&mut self) -> Option<Self::Item> {
        if self.x >= self.claim.x + self.claim.width {
            self.x = self.claim.x;
            self.y += 1;
        }
        if self.y >= self.claim.y + self.claim.height {
            return None;
        }
        self.x += 1;
        Some((self.x - 1, self.y))
    }
}

impl FromStr for Claim {
    type Err = regex::Error;

    fn from_str(repr: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)").unwrap();
        let caps = match re.captures(repr) {
            None => return Err(regex::Error::Syntax(repr.to_string())),
            Some(caps) => caps,
        };
        let cap = |x| caps.get(x).unwrap().as_str().parse::<u32>().unwrap();
        Ok(Claim {
            id: cap(1),
            x: cap(2),
            y: cap(3),
            width: cap(4),
            height: cap(5),
        })
    }
}

fn part1(claims: &Vec<Claim>) -> usize {
    let mut grid = HashMap::<(u32, u32), u32>::new();
    for claim in claims {
        for point in claim.points() {
            *grid.entry(point).or_insert(0) += 1;
        }
    }
    grid.values().filter(|&&x| x > 1).count()
}

fn part2(claims: &Vec<Claim>) -> Option<u32> {
    for (i, claim) in claims.iter().enumerate() {
        let has_overlapping = claims
            .iter()
            .enumerate()
            .find(|(j, c)| i != *j && claim.overlaps(c));
        if let None = has_overlapping {
            return Some(claim.id);
        }
    }
    None
}

fn main() -> io::Result<()> {
    let f = File::open("input.txt")?;
    let reader = BufReader::new(f);

    let claims: Vec<Claim> = reader
        .lines()
        .map(|line| Claim::from_str(&line.unwrap()).unwrap())
        .collect();

    println!("Part 1: {}", part1(&claims));
    println!("Part 2: {}", part2(&claims).unwrap_or(0u32));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn claim_from_str() {
        let claim = Claim {
            id: 1,
            x: 1,
            y: 3,
            width: 4,
            height: 4,
        };
        assert_eq!(claim, Claim::from_str("#1 @ 1,3: 4x4").unwrap());

        let claim = Claim {
            id: 2,
            x: 3,
            y: 1,
            width: 4,
            height: 4,
        };
        assert_eq!(claim, Claim::from_str("#2 @ 3,1: 4x4").unwrap());

        let claim = Claim {
            id: 3,
            x: 5,
            y: 5,
            width: 2,
            height: 2,
        };
        assert_eq!(claim, Claim::from_str("#3 @ 5,5: 2x2").unwrap());
    }

    #[test]
    fn claim_points_iterator() {
        let claim = Claim {
            id: 3,
            x: 5,
            y: 5,
            width: 2,
            height: 2,
        };
        let mut iter = claim.points();
        assert_eq!((5, 5), iter.next().unwrap());
        assert_eq!((6, 5), iter.next().unwrap());
        assert_eq!((5, 6), iter.next().unwrap());
        assert_eq!((6, 6), iter.next().unwrap());
        assert_eq!(None, iter.next());
    }

    #[test]
    fn claim_overlappiong() {
        let claim1 = Claim {
            id: 1,
            x: 1,
            y: 3,
            width: 4,
            height: 4,
        };
        let claim2 = Claim {
            id: 2,
            x: 3,
            y: 1,
            width: 4,
            height: 4,
        };
        let claim3 = Claim {
            id: 3,
            x: 5,
            y: 5,
            width: 2,
            height: 2,
        };
        assert_eq!(true, claim1.overlaps(&claim2));
        assert_eq!(false, claim1.overlaps(&claim3));
        assert_eq!(false, claim2.overlaps(&claim3));
    }
}
