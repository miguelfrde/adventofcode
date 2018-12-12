use std::{
    collections::HashSet,
    fs::File,
    io::{self, prelude::*, BufReader},
    str::FromStr,
};

#[derive(Hash, Eq, PartialEq, Debug)]
struct Note {
    rule: String,
    result: char,
}

#[derive(Debug)]
enum ParseError {
    Error,
}

#[derive(PartialEq, Clone, Debug)]
struct Pot {
    state: char,
    index: i32,
}

#[derive(PartialEq, Debug)]
struct PotState {
    pots: Vec<Pot>,
}

impl PotState {
    pub fn new(pots: Vec<char>) -> Self {
        PotState {
            pots: pots
                .iter()
                .enumerate()
                .map(|(i, s)| Pot {
                    state: *s,
                    index: i as i32,
                })
                .collect(),
        }
    }

    pub fn advance_generation(&mut self, notes: &HashSet<Note>) -> PotState {
        // Prepend and append some pots with negative and positive indeces.
        for _ in 0..5 {
            self.pots.insert(
                0,
                Pot {
                    state: '.',
                    index: self.pots[0].index - 1,
                },
            );
            self.pots.push(Pot {
                state: '.',
                index: self.pots[self.pots.len() - 1].index + 1,
            });
        }
        let pots : Vec<Pot> = self
                .pots
                .iter()
                .enumerate()
                .map(|(i, pot)| {
                    if i < 2 || i >= self.pots.len() - 2 {
                        return pot.clone();
                    }
                    let state = match notes.iter().find(|note| note.rule == self.pot_as_note(i)) {
                        Some(note) => note.result,
                        None => '.',
                    };
                    return Pot {
                        state: state,
                        index: pot.index,
                    };
                })
                .collect();
        let min = pots.iter().enumerate().find(|(_, p)| p.state == '#').unwrap().0;
        let max = pots.iter().enumerate().rev().find(|(_, p)| p.state == '#').unwrap().0;
        PotState { pots: pots[min..=max].to_vec() }
    }

    fn pot_as_note(&self, index: usize) -> String {
        (index - 2..=index + 2)
            .map(|i| self.pots[i].state)
            .collect()
    }
}

impl FromStr for PotState {
    type Err = ParseError;

    fn from_str(repr: &str) -> Result<Self, Self::Err> {
        if !repr.chars().all(|c| c == '#' || c == '.') {
            return Err(ParseError::Error);
        }
        Ok(PotState::new(repr.chars().collect()))
    }
}

impl FromStr for Note {
    type Err = ParseError;

    fn from_str(repr: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = repr.split(' ').collect();
        if parts.len() != 3 || parts[0].len() != 5 || parts[2].len() != 1 {
            return Err(ParseError::Error);
        }
        Ok(Note {
            rule: parts[0].to_string(),
            result: parts[2].chars().next().unwrap(),
        })
    }
}

fn part1(state: PotState, notes: &HashSet<Note>, generations: usize) -> i32 {
    let mut s = state;
    for _ in 0..generations {
        s = s.advance_generation(notes);
    }
    s.pots
        .iter()
        .filter(|pot| pot.state == '#')
        .map(|pot| pot.index)
        .sum()
}

fn main() -> io::Result<()> {
    let f = File::open("input.txt")?;
    let reader = BufReader::new(f);

    let mut lines = reader.lines();
    let state_repr = lines
        .next()
        .unwrap()
        .unwrap()
        .as_str()
        .replace("initial state: ", "");

    // Skip empty line
    lines.next();

    let notes: HashSet<Note> = lines
        .map(|line| Note::from_str(&line.unwrap()).unwrap())
        .collect();

    let state = PotState::from_str(state_repr.as_str()).unwrap();
    println!("Part 1: {}", part1(state, &notes, 20));
    let state = PotState::from_str(state_repr.as_str()).unwrap();
    println!("Part 1: {}", part1(state, &notes, 50000000000));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_note_from_str() {
        assert_eq!(
            Note {
                rule: "...##".to_string(),
                result: '#'
            },
            Note::from_str("...## => #").unwrap()
        );
        assert_eq!(
            Note {
                rule: ".####".to_string(),
                result: '.'
            },
            Note::from_str(".#### => .").unwrap()
        );
    }

    #[test]
    fn test_pot_list_from_str() {
        let pot = |p, i| Pot { state: p, index: i };
        assert_eq!(
            PotState {
                pots: vec![
                    pot('#', 0),
                    pot('#', 1),
                    pot('.', 2),
                    pot('#', 3),
                    pot('.', 4)
                ]
            },
            PotState::from_str("##.#.").unwrap()
        );
    }

    #[test]
    fn test_part1() {
        let state = PotState::from_str("#..#.#..##......###...###").unwrap();
        let notes: HashSet<Note> = vec![
            "...## => #",
            "..#.. => #",
            ".#... => #",
            ".#.#. => #",
            ".#.## => #",
            ".##.. => #",
            ".#### => #",
            "#.#.# => #",
            "#.### => #",
            "##.#. => #",
            "##.## => #",
            "###.. => #",
            "###.# => #",
            "####. => #",
        ]
        .iter()
        .map(|repr| Note::from_str(repr).unwrap())
        .collect();
        assert_eq!(325, part1(state, &notes, 20));
    }
}
