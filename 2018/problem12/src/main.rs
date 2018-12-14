use std::{
    collections::HashMap,
    fs::File,
    io::{self, prelude::*, BufReader},
};

type Pot = (i32, bool);

pub fn advance_generation(prev_pots: Vec<Pot>, notes: &HashMap<Vec<bool>, bool>) -> Vec<Pot> {
    // Prepend and append some pots with negative and positive indeces.
    let mut pots = prev_pots;
    for _ in 0..3 {
        pots.insert(0, (pots[0].0 - 1, false));
        pots.push((pots[pots.len() - 1].0 + 1, false));
    }
    let result: Vec<Pot> = pots
        .iter()
        .enumerate()
        .map(|(i, &(index, value))| {
            if i < 2 || i >= pots.len() - 2 {
                return (index, value);
            }
            let new_state = notes.get(&pot_as_note(&pots, i)).unwrap_or(&false);
            (index, *new_state)
        })
        .collect();
    // This cleanup speeds things up even when it requires to store the index per element.
    let min = result.iter().enumerate().find(|(_, &(_, s))| s).unwrap().0;
    let max = result
        .iter()
        .enumerate()
        .rev()
        .find(|(_, &(_, s))| s)
        .unwrap()
        .0;
    result[min..=max].to_vec()
}

fn pot_as_note(pots: &Vec<Pot>, index: usize) -> Vec<bool> {
    (index - 2..=index + 2).map(|i| pots[i].1).collect()
}

fn state_from_str(repr: &str) -> Vec<Pot> {
    repr.chars()
        .enumerate()
        .map(|(i, c)| (i as i32, c == '#'))
        .collect()
}

fn note_from_str(repr: &str) -> (Vec<bool>, bool) {
    let parts: Vec<&str> = repr.split(' ').collect();
    let rule = parts[0].chars().map(|c| c == '#').collect();
    let result = parts[2].chars().next().unwrap() == '#';
    (rule, result)
}

fn part1(state: Vec<Pot>, notes: &HashMap<Vec<bool>, bool>, generations: usize) -> i32 {
    let mut curr_state = state;
    for _ in 0..generations {
        curr_state = advance_generation(curr_state, notes);
    }
    curr_state.iter().filter(|(_, s)| *s).map(|&(i, _)| i).sum()
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

    let notes: HashMap<Vec<bool>, bool> = lines.map(|line| note_from_str(&line.unwrap())).collect();

    let state = state_from_str(state_repr.as_str());
    println!("Part 1: {}", part1(state.clone(), &notes, 20));
    let state = state_from_str(state_repr.as_str());
    // Notice the pattern for 50b
    println!("Part 2(500): {}", part1(state.clone(), &notes, 500));
    println!("Part 2(5000): {}", part1(state.clone(), &notes, 5000));
    println!("Part 2(50000): {}", part1(state.clone(), &notes, 50000));
    println!("Part 2(500000): {}", part1(state.clone(), &notes, 500000));
    //println!("Part 1: {}", part1(state, &notes, 50000000000));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_note_from_str() {
        assert_eq!(
            (vec![false, false, false, true, true], true),
            note_from_str("...## => #"),
        );
        assert_eq!(
            (vec![false, true, true, true, true], false),
            note_from_str(".#### => ."),
        );
    }

    #[test]
    fn test_pot_list_from_str() {
        assert_eq!(
            PotState {
                pots: vec![(0, true), (1, true), (2, false), (3, true), (4, false),]
            },
            PotState::from_str("##.#.").unwrap(),
        );
    }

    #[test]
    fn test_part1() {
        let state = state_from_str("#..#.#..##......###...###").unwrap();
        let notes: HashMap<Vec<bool>, bool> = vec![
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
        .map(|repr| note_from_str(repr))
        .collect();
        assert_eq!(325, part1(state, &notes, 20));
    }
}
