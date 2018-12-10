use std::{
    fs::File,
    io::{self, prelude::*, BufReader},
    num::ParseIntError,
    str::FromStr,
};

#[derive(PartialEq, Clone, Debug)]
struct Coord {
    x: i32,
    y: i32,
}

impl Coord {
    fn distance(&self, other: &Coord) -> i32 {
        (self.x - other.x).abs() + (self.y - other.y).abs()
    }
}

impl FromStr for Coord {
    type Err = ParseIntError;

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let mut nums = line.split(", ");
        Ok(Coord {
            x: nums.next().unwrap().parse().unwrap(),
            y: nums.next().unwrap().parse().unwrap(),
        })
    }
}

fn get_bounds(coords: &Vec<Coord>) -> (Coord, Coord) {
    let max_x = coords.iter().max_by_key(|c| c.x).unwrap().x;
    let max_y = coords.iter().max_by_key(|c| c.y).unwrap().y;
    let min_x = coords.iter().min_by_key(|c| c.x).unwrap().x;
    let min_y = coords.iter().min_by_key(|c| c.y).unwrap().y;
    (Coord { x: min_x, y: min_y }, Coord { x: max_x, y: max_y })
}

fn part1(coords: &Vec<Coord>) -> i32 {
    let mut distances = vec![0; coords.len()];
    let (min, max) = get_bounds(coords);
    for x in min.x..=max.x {
        for y in min.y..=max.y {
            let coord = Coord { x: x, y: y };
            let mut coord_dists: Vec<(usize, i32)> = coords
                .iter()
                .enumerate()
                .map(|(i, c)| (i, c.distance(&coord)))
                .collect();
            coord_dists.sort_by_key(|(_, d)| d + 0);
            if coord_dists[0].1 != coord_dists[1].1 {
                // No two equal distances
                // If on edge, force to be smallest since it counts as infinite
                if x == min.x || x == max.x || y == min.y || y == max.y {
                    distances[coord_dists[0].0] = -1_000_000;
                } else {
                    distances[coord_dists[0].0] += 1
                }
            }
        }
    }
    *distances.iter().max().unwrap()
}

fn part2(coords: &Vec<Coord>, max_distance: i32) -> usize {
    let (min, max) = get_bounds(coords);
    let mut valid_coords = Vec::<Coord>::new();
    for x in min.x..=max.x {
        for y in min.y..=max.y {
            let coord = Coord { x: x, y: y };
            let sum: i32 = coords.iter().map(|c| c.distance(&coord)).sum();
            if sum < max_distance {
                valid_coords.push(coord);
            }
        }
    }
    valid_coords.len()
}

fn main() -> io::Result<()> {
    let f = File::open("input.txt")?;
    let reader = BufReader::new(f);

    let coords: Vec<Coord> = reader
        .lines()
        .map(|line| Coord::from_str(&line.unwrap()).unwrap())
        .collect();

    println!("Part 1: {}", part1(&coords));
    println!("Part 2: {}", part2(&coords, 10000));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    const COORDS: [Coord; 6] = [
        Coord { x: 1, y: 1 },
        Coord { x: 1, y: 6 },
        Coord { x: 8, y: 3 },
        Coord { x: 3, y: 4 },
        Coord { x: 5, y: 5 },
        Coord { x: 8, y: 9 },
    ];

    #[test]
    fn test_coord_from_str() {
        assert_eq!(Coord { x: 1, y: 2 }, Coord::from_str("1, 2").unwrap())
    }

    #[test]
    fn test_part1() {
        assert_eq!(17, part1(&COORDS.to_vec()));
    }

    #[test]
    fn test_part2() {
        assert_eq!(16, part2(&COORDS.to_vec(), 32));
    }
}
