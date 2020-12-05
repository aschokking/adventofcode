use itertools::Itertools;
use std::fs;

fn main() {
    println!("Hello, world!");

    let contents =
        fs::read_to_string("../inputs/day1.txt").expect("Something went wrong reading the file");

    let lines = contents.lines();

    let numbers = lines.map(|line| line.parse::<i32>().unwrap());

    let combos = numbers.combinations(3);

    for i in 0..numbers.len() {
        for j in 0..numbers.len() {
            let number1 = numbers.get(i);
            let number2 = numbers.get(j);

            if number1.is_some() && number2.is_some() {
                if number1.unwrap() + number2.unwrap() == 2020 {
                    println!(
                        "Found numbers {} and {}",
                        number1.unwrap(),
                        number2.unwrap()
                    );
                    println!("Answer is {}", number1.unwrap() * number2.unwrap());
                    return;
                }
            }
        }
    }
}
