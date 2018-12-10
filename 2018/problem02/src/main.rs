use std::{
    collections::HashMap,
    fs::File,
    io::{self, prelude::*, BufReader},
};

fn letter_counts(word: &String) -> HashMap<char, u32> {
    let mut letters = HashMap::<char, u32>::new();
    for c in word.chars() {
        *letters.entry(c).or_insert(0) += 1
    }
    letters
}

fn part1(inputs: &Vec<String>) -> u32 {
    let mut count2 = 0;
    let mut count3 = 0;
    for input in inputs.iter() {
        let counts = letter_counts(input);
        let has = |x| counts.values().any(|&y| y == x);
        if has(2) {
            count2 += 1;
        }
        if has(3) {
            count3 += 1;
        }
    }
    count2 * count3
}

fn off_by_one(a: &String, b: &String) -> Option<String> {
    let common_letters: String = a
        .chars()
        .zip(b.chars())
        .filter_map(|(x, y)| if x == y { Some(x) } else { None })
        .collect();
    if common_letters.len() == a.len() - 1 {
        Some(common_letters)
    } else {
        None
    }
}

fn part2(inputs: &Vec<String>) -> Option<String> {
    for (i, x) in inputs.iter().enumerate() {
        for y in &inputs[i + 1..] {
            if let Some(common) = off_by_one(&x, &y) {
                return Some(common);
            }
        }
    }
    None
}

fn main() -> io::Result<()> {
    let f = File::open("input.txt")?;
    let reader = BufReader::new(f);

    let words: Vec<String> = reader.lines().map(|line| line.unwrap()).collect();

    println!("Part 1 {}", part1(&words));
    println!(
        "Part 2 {}",
        part2(&words).unwrap_or("No solution".to_string())
    );

    Ok(())
}
