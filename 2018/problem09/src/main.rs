extern crate regex;

use regex::Regex;
use std::{
    collections::VecDeque,
    fs::File,
    io::{self, prelude::*, BufReader},
    str::FromStr,
};

#[derive(PartialEq, Debug)]
struct Game {
    players: u32,
    last_marble_points: u32,
}

struct Circle<T> {
    queue: VecDeque<T>,
}

impl<T> Circle<T> {
    pub fn new() -> Self {
        Circle {
            queue: VecDeque::<T>::new(),
        }
    }

    pub fn rotate_clockwise(&mut self, rotations: u32) {
        for _ in 0..rotations {
            if let Some(m) = self.queue.pop_front() {
                self.queue.push_back(m);
            }
        }
    }

    pub fn rotate_counter_clockwise(&mut self, rotations: u32) {
        for _ in 0..rotations {
            if let Some(m) = self.queue.pop_back() {
                self.queue.push_front(m);
            }
        }
    }

    // Inserts in the position currently focused.
    pub fn insert(&mut self, value: T) {
        self.queue.push_front(value);
    }

    // Removes from the position currently focused.
    pub fn remove(&mut self) -> Option<T> {
        self.queue.pop_front()
    }
}

fn play(game: &Game) -> u32 {
    let mut scores: Vec<u32> = (0..game.players).map(|_| 0).collect();
    let mut circle = Circle::<u32>::new();
    circle.insert(0);
    for marble in 1..=game.last_marble_points {
        if marble % 23 == 0 {
            circle.rotate_counter_clockwise(7);
            let player = (marble % game.players) as usize;
            scores[player] += marble + circle.remove().unwrap();
        } else {
            circle.rotate_clockwise(2);
            circle.insert(marble);
        }
    }
    *scores.iter().max().unwrap()
}

impl FromStr for Game {
    type Err = regex::Error;

    fn from_str(repr: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"(\d+) players; last marble is worth (\d+) points").unwrap();
        let caps = match re.captures(repr) {
            None => return Err(regex::Error::Syntax(repr.to_string())),
            Some(caps) => caps,
        };
        let cap = |x| caps.get(x).unwrap().as_str().parse::<u32>().unwrap();
        Ok(Game {
            players: cap(1),
            last_marble_points: cap(2),
        })
    }
}

fn main() -> io::Result<()> {
    let f = File::open("input.txt")?;
    let reader = BufReader::new(f);

    let mut game: Game = Game::from_str(&reader.lines().next().unwrap().unwrap()).unwrap();
    println!("Part 1: {}", play(&game));

    game.last_marble_points *= 100;
    println!("Part 2: {}", play(&game));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_game_from_str() {
        let game = Game {
            players: 10,
            last_marble_points: 1618,
        };
        assert_eq!(
            game,
            Game::from_str("10 players; last marble is worth 1618 points").unwrap()
        );
    }

    #[test]
    fn test_game_play() {
        assert_eq!(
            32,
            play(&Game {
                players: 9,
                last_marble_points: 25
            })
        );
        assert_eq!(
            8317,
            play(&Game {
                players: 10,
                last_marble_points: 1618
            })
        );
        assert_eq!(
            146373,
            play(&Game {
                players: 13,
                last_marble_points: 7999
            })
        );
        assert_eq!(
            2764,
            play(&Game {
                players: 17,
                last_marble_points: 1104
            })
        );
        assert_eq!(
            54718,
            play(&Game {
                players: 21,
                last_marble_points: 6111
            })
        );
        assert_eq!(
            37305,
            play(&Game {
                players: 30,
                last_marble_points: 5807
            })
        );
    }
}
