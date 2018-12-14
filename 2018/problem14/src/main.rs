use std::io;

fn part1(goal: usize) -> u64 {
    let mut recipes :Vec<u8> = vec![3, 7];
    let mut elfs : [usize; 2] = [0, 1];
    while recipes.len() < goal + 10 {
        let sum = recipes[elfs[0]] + recipes[elfs[1]];
        if sum >= 10 {
            recipes.push(sum / 10);
        }
        recipes.push(sum % 10);
        for i in 0..elfs.len() {
           elfs[i] = (elfs[i] + 1 + recipes[elfs[i]] as usize) % recipes.len();
        }
    }
    recipes[goal..goal+10].iter().fold(0, |acc, &x| acc*10 + x as u64)
}

fn part2(goal: &str) -> usize {
    let digits : Vec<u8> = goal.chars().map(|c| c.to_digit(10).unwrap() as u8).collect();
    let mut recipes :Vec<u8> = vec![3, 7];
    let mut elfs : [usize; 2] = [0, 1];
    let mut checked_index = 0;
    loop {
        let sum = recipes[elfs[0]] + recipes[elfs[1]];
        if sum >= 10 {
            recipes.push(sum / 10);
        }
        recipes.push(sum % 10);
        for i in 0..elfs.len() {
           elfs[i] = (elfs[i] + 1 + recipes[elfs[i]] as usize) % recipes.len();
        }
        if recipes.len() < digits.len() {
            continue;
        }
        if let Some(i) = recipes[checked_index..].windows(digits.len()).position(|w| w.to_vec() == digits) {
            return checked_index + i;
        }
        checked_index = recipes.len() - digits.len() + 1;
    }
}

fn main() -> io::Result<()> {
	println!("Part 1: {}", part1(825401));
	println!("Part 2: {}", part2("825401"));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(5158916779, part1(9));
        assert_eq!(0124515891, part1(5));
        assert_eq!(9251071085, part1(18));
        assert_eq!(5941429882, part1(2018));
    }

    #[test]
    fn test_part2() {
        assert_eq!(9, part2("51589"));
        assert_eq!(5, part2("01245"));
        assert_eq!(18, part2("92510"));
        assert_eq!(2018, part2("59414"));
    }
}
