use std::{
    collections::HashSet,
    fs::File,
    io::{self, prelude::*, BufReader},
};

fn main() -> io::Result<()> {
    let f = File::open("input.txt")?;
    let reader = BufReader::new(f);

    let values : Vec<i32> = reader
        .lines()
        .map(|line| line.unwrap().parse::<i32>().unwrap())
        .collect();

    println!("Part 1: {}", values.iter().sum::<i32>());

    let mut seen = HashSet::new();
    let first = values
        .iter()
        .cycle()
        .scan(0, |acc, &x| {
            let res = *acc;
            *acc += x;
            Some(res)
        })
        .find(|&value| !seen.insert(value))
        .unwrap();

    println!("Part 2: {}", first);

    Ok(())
}
