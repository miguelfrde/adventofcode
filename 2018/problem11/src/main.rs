use std::io;

struct KernelResult {
    x: usize,
    y: usize,
    sum: i32,
}

const GRID_SIZE: usize = 300;

fn cell_power(x: usize, y: usize, serial: i32) -> i32 {
    let rack_id = x as i32 + 10;
    let power = (rack_id * (y as i32) + serial) * rack_id;
    (power % 1000) / 100 - 5
}

fn max_in_kernels(serial: i32, kernel_size: usize) -> KernelResult {
    let mut max_sum = 0;
    let mut coord = (0, 0);
    for i in 0..(GRID_SIZE - kernel_size) {
        for j in 0..(GRID_SIZE - kernel_size) {
            let mut sum = 0;
            for l in 0..kernel_size {
                for k in 0..kernel_size {
                    sum += cell_power(i + l, j + k, serial);
                }
            }
            if sum > max_sum {
                coord = (i, j);
                max_sum = sum;
            }
        }
    }
    KernelResult {
        x: coord.0,
        y: coord.1,
        sum: max_sum,
    }
}

fn part1(serial: i32) -> (usize, usize) {
    let result = max_in_kernels(serial, 3);
    (result.x, result.y)
}

fn part2(serial: i32) -> (usize, usize, usize) {
    (1..=GRID_SIZE)
        .map(|kernel_size| (max_in_kernels(serial, kernel_size), kernel_size))
        .max_by_key(|(result, _)| result.sum)
        .map(|(result, size)| (result.x, result.y, size))
        .unwrap()
}

fn main() -> io::Result<()> {
    let result = part1(7165);
    println!("Part 1: {},{}", result.0, result.1);
    let result = part2(7165);
    println!("Part 2: {},{},{}", result.0, result.1, result.2);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_cell_power() {
        assert_eq!(4, cell_power(3, 5, 8));
        assert_eq!(-5, cell_power(122, 79, 57));
        assert_eq!(0, cell_power(217, 196, 39));
        assert_eq!(4, cell_power(101, 153, 71));
    }

    #[test]
    fn test_part1() {
        assert_eq!((33, 45), part1(18));
        assert_eq!((21, 61), part1(42));
    }

    #[test]
    fn test_part2() {
        assert_eq!((90, 269, 16), part2(18));
        assert_eq!((232, 251, 12), part2(42));
    }
}
