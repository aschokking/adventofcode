use itertools::Itertools;
use std::fs;

fn day1(window_size: usize) {
    let contents =
        fs::read_to_string("../inputs/day1.txt").expect("Something went wrong reading the file");

    let lines = contents.lines();

    let numbers = lines.filter_map(|line| line.parse::<u32>().ok());

    for combo in numbers.combinations(window_size) {
        if combo.iter().sum::<u32>() == 2020 {
            println!("Bingo! {}", combo.iter().product::<u32>());
            break;
        }
    }
}

fn day1_1() {
    day1(2)
}

fn day1_2() {
    day1(3)
}

fn main() {
    day1_1();
    day1_2();
}
