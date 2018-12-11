extern crate regex;

use regex::Regex;
use std::{
    collections::HashSet,
    fs::File,
    io::{self, prelude::*, stdin, stdout, BufReader, Read, Write},
    str::FromStr,
};

#[derive(PartialEq, Clone, Debug)]
struct Star {
    position: (i32, i32),
    velocity: (i32, i32),
}

#[derive(PartialEq, Debug)]
struct Galaxy {
    stars: Vec<Star>,
    seconds: u32,
}

impl Star {
    fn step(&self) -> Self {
        let x = self.position.0 + self.velocity.0;
        let y = self.position.1 + self.velocity.1;
        Star {
            position: (x, y),
            velocity: self.velocity,
        }
    }
}

impl Galaxy {
    fn new(stars: Vec<Star>) -> Self {
        Galaxy {
            stars: stars,
            seconds: 0,
        }
    }

    fn step(&mut self) {
        self.stars = self.stars.iter().map(|star| star.step()).collect();
        self.seconds += 1;
    }

    fn display(&self) -> bool {
        let min_x = self.stars.iter().map(|star| star.position.0).min().unwrap();
        let max_x = self.stars.iter().map(|star| star.position.0).max().unwrap();
        let min_y = self.stars.iter().map(|star| star.position.1).min().unwrap();
        let max_y = self.stars.iter().map(|star| star.position.1).max().unwrap();
        if max_x - min_x > 100 || max_y - min_y > 100 {
            return false;
        }
        let points: HashSet<(i32, i32)> = self.stars.iter().map(|star| star.position).collect();
        println!("State after {} seconds:", self.seconds);
        for y in min_y..=max_y {
            for x in min_x..=max_x {
                if points.contains(&(x, y)) {
                    print!(".")
                } else {
                    print!(" ");
                }
            }
            println!("");
        }
        true
    }
}

impl FromStr for Star {
    type Err = regex::Error;

    fn from_str(repr: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(
            r"position=<((\-|\s)\d+), ((\-|\s)\d+)> velocity=<((\-|\s)\d+), ((\-|\s)\d+)>",
        )
        .unwrap();
        let caps = match re.captures(repr) {
            None => return Err(regex::Error::Syntax(repr.to_string())),
            Some(caps) => caps,
        };
        let cap = |x| caps.get(x).unwrap().as_str().trim().parse::<i32>().unwrap();
        Ok(Star {
            position: (cap(1), cap(3)),
            velocity: (cap(5), cap(7)),
        })
    }
}

fn pause() {
    let mut stdout = stdout();
    stdout.write(b"Press Enter to continue...").unwrap();
    stdout.flush().unwrap();
    stdin().read(&mut [0]).unwrap();
}

fn solve(mut galaxy: Galaxy) {
    for _ in 1.. {
        galaxy.step();
        if galaxy.display() {
            pause();
        }
    }
}

fn main() -> io::Result<()> {
    let f = File::open("input.txt")?;
    let reader = BufReader::new(f);

    let stars: Vec<Star> = reader
        .lines()
        .map(|line| Star::from_str(&line.unwrap()).unwrap())
        .collect();

    solve(Galaxy::new(stars.to_vec()));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_star_from_str() {
        let star = Star {
            position: (-6, 10),
            velocity: (2, -2),
        };
        assert_eq!(
            star,
            Star::from_str("position=<-6,  10> velocity=< 2, -2>").unwrap()
        );
    }

    #[test]
    fn test_start_step() {
        let star = Star {
            position: (-6, 10),
            velocity: (2, -2),
        };
        let expect_star = Star {
            position: (-4, 8),
            velocity: (2, -2),
        };
        assert_eq!(expect_star, star.step());
    }

    #[test]
    fn test_galaxy_step() {
        let mut galaxy = Galaxy::new(vec![
            Star {
                position: (-6, 10),
                velocity: (2, -2),
            },
            Star {
                position: (-2, 3),
                velocity: (1, 0),
            },
        ]);
        let expect_galaxy = Galaxy::new(vec![
            Star {
                position: (-4, 8),
                velocity: (2, -2),
            },
            Star {
                position: (-1, 3),
                velocity: (1, 0),
            },
        ]);
        galaxy.step();
        assert_eq!(expect_galaxy, galaxy);
    }
}
