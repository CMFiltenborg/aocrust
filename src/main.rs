#![allow(unused_imports)]
#![allow(dead_code)]
#![allow(unused_variables)]

use std::env;
use std::{cmp, fs};
use std::str::Lines;
use std::collections::HashSet;
use std::hash::Hash;
use std::slice::Chunks;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        panic!("Invalid num of args given.");
    }
    let day_num = match args[1].parse::<i32>() {
        Ok(day) => day,
        Err(error) => panic!("Specify the day exercise to run: {}", error)
    };
    let part = match args[2].parse::<i32>() {
        Ok(part) => part,
        Err(error) => panic!("Specify the part of the day (1 or 2): {}", error)
    };
    let file_path = format!("data/input{day_num}.txt");
    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    let callable = match (day_num, part) {
        (1, 1) => day1,
        (1, 2) => day1p2,
        (2, 1) => day2p1,
        (2, 2) => day2p2,
        (3, 1) => day3p1,
        (3, 2) => day3p2,
        (4, 1) => day4p1,
        (4, 2) => day4p2,
        (5, 1) => day5p1,
        (5, 2) => day5p2,
        (6, 1) => day6p1,
        (6, 2) => day6p2,
        _ => panic!("Unimplemented day")
    };
    let solution: String = callable(contents);

    println!("The solution for day {day_num} part {part} is: {solution}")
}

fn day1(contents: String) -> String {
    let lines = contents.lines();
    let mut max_cals = 0;
    let mut curr = 0;
    for line in lines {
        if line == "" {
            max_cals = cmp::max(curr, max_cals);
            curr = 0;
            continue;
        }

        let cals = line.parse::<i32>().unwrap();
        curr = curr + cals;
    }
    max_cals.to_string()
}

fn day1p2(contents: String) -> String {
    let lines = contents.lines();
    let mut elves: Vec<i32> = vec![];
    let mut accum = 0;
    for line in lines {
        if line == "" {
            elves.push(accum);
            accum = 0;
            continue;
        }
        let cals = line.parse::<i32>().unwrap();
        accum += cals
    }
    elves.sort();
    let largest_3 = elves.iter().rev().take(3).sum::<i32>();
    largest_3.to_string()
}


// The winner of the whole tournament is the player with the highest score. Your total score is the sum of your scores for each round.
// The score for a single round is the score for the shape you selected
// (1 for Rock, 2 for Paper, and 3 for Scissors)
// plus the score for the outcome of the round
// (0 if you lost, 3 if the round was a draw, and 6 if you won).
// A for Rock, B for Paper, and C for Scissors.
// X for Rock, Y for Paper, and Z for Scissors.
fn day2p1(contents: String) -> String {
    let lines = contents.lines();

    fn game_score(s: &str) -> i32 {
        let items: Vec<&str> = s.split(' ').collect();
        if items.len() != 2 {
            return 0;
        }
        let score = match items[1] {
            "X" => 1,
            "Y" => 2,
            "Z" => 3,
            _ => 0,
        };
        let win_score = match (items[0], items[1]) {
            ("A", "Y") => 6,
            ("A", "Z") => 0,
            ("B", "X") => 0,
            ("B", "Z") => 6,
            ("C", "X") => 6,
            ("C", "Y") => 0,
            _ => 3,
        };

        score + win_score
    }

    let objs: i32 = lines.map(game_score).sum();
    objs.to_string()
}

fn day2p2(contents: String) -> String {
    let lines = contents.lines();

    fn game_score(s: &str) -> i32 {
        let items: Vec<&str> = s.split(' ').collect();
        if items.len() != 2 {
            return 0;
        }
        // X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win. Good luck!"
        let their_move = items[0];
        let orig_move = items[1];
        let our_move = match (orig_move, their_move) {
            ("X", "A") => "C",
            ("X", "B") => "A",
            ("X", "C") => "B",
            ("Z", "A") => "B",
            ("Z", "B") => "C",
            ("Z", "C") => "A",
            ("Y", _) => their_move,
            _ => panic!("Not supposed to happen")
        };

        let score = match our_move {
            "A" => 1,
            "B" => 2,
            "C" => 3,
            _ => panic!("Not supposed to happen"),
        };

        let win_score = match orig_move {
            "X" => 0,
            "Y" => 3,
            "Z" => 6,
            _ => panic!("Not supposed to happen"),
        };

        score + win_score
    }

    let objs: i32 = lines.map(game_score).sum();
    objs.to_string()
}

fn split_middle(line: &str) -> (&str, &str) {
    let mid = line.len() / 2;
    (&line[0..mid], &line[mid..])
}

fn common_letter((p1, p2): (&str, &str)) -> char {
    let a: HashSet<char> = HashSet::from_iter(p1.chars());
    let b: HashSet<char> = HashSet::from_iter(p2.chars());
    let same: Vec<&char> = a.intersection(&b).collect();
    if same.len() == 0 {
        panic!("Expected larger sameset then 0")
    }
    return same[0].clone();
}


fn intersection_sets<T: Eq + Hash>(a: HashSet<T>, b: &HashSet<T>) -> HashSet<T> {
    a.into_iter().filter(|e| b.contains(e)).collect()
}

fn common_letter_groups(groups: &[&str]) -> char {
    let i1 = HashSet::from_iter(groups[0].chars());
    let i2 = HashSet::from_iter(groups[1].chars());
    let i3 = HashSet::from_iter(groups[2].chars());

    let intersection = intersection_sets(intersection_sets(i1, &i2), &i3);
    let items: Vec<&char> = intersection.iter().collect();
    items[0].clone()
}

fn letter_to_priority(a: char) -> u32 {
    // Lowercase item types a through z have priorities 1 through 26.
    // Uppercase item types A through Z have priorities 27 through 52.
    let base = if a.is_uppercase() {
        26
    } else {
        0
    };
    base + a.to_digit(36).unwrap() - 9
}

fn day3p1(contents: String) -> String {
    let lines = contents.lines();
    let result = lines.map(split_middle)
        .map(common_letter)
        .map(letter_to_priority)
        .sum::<u32>() as i32;

    result.to_string()
}

fn day3p2(contents: String) -> String {
    let lines: Vec<&str> = contents.lines().collect();

    let result =
        lines.chunks(3)
            .map(common_letter_groups)
            .map(letter_to_priority)
            .sum::<u32>() as i32;

    result.to_string()
}

fn parse_pair(line: &str) -> (i32, i32, i32, i32) {
    let pairs: Vec<&str> = line.split(',').collect();
    let pair1 = pairs[0];
    let pair1: Vec<&str> = pair1.split('-').collect();
    let (d1, d2) = (pair1[0], pair1[1]);
    let pair2 = pairs[1];
    let pair2: Vec<&str> = pair2.split('-').collect();
    let (d3, d4) = (pair2[0], pair2[1]);
    (d1.parse::<i32>().unwrap(), d2.parse::<i32>().unwrap(), d3.parse::<i32>().unwrap(), d4.parse::<i32>().unwrap())
}

fn day4p1(contents: String) -> String {
    let lines: Vec<&str> = contents.lines().collect();
    let result = lines.into_iter()
        .map(parse_pair)
        .map(contains)
        .filter(|x| *x == true)
        .count() as i32;

    result.to_string()
}

fn contains((d1, d2, d3, d4): (i32, i32, i32, i32)) -> bool {
    (d3 >= d1 && d4 <= d2) || (d3 <= d1 && d4 >= d2)
}

fn day4p2(contents: String) -> String {
    let lines: Vec<&str> = contents.lines().collect();
    let result = lines.into_iter()
        .map(parse_pair)
        .map(overlaps)
        .filter(|x| *x == true)
        .count() as i32;

    fn overlaps((d1, d2, d3, d4): (i32, i32, i32, i32)) -> bool {
        (d1 <= d4 && d2 >= d3) ||// X is smaller or eq but touching
            (d2 >= d3 && d1 <= d4) // X is bigger or eq but touching
    }

    result.to_string()
}

fn day5p1(contents: String) -> String {
    let lines: Vec<&str> = contents.lines().collect();
    let (stack_lines, moves) = lines.split_at(8);
    let (_, moves) = moves.split_at(2);

    let stacks = vec![vec![]; 9];
    let stacks: Vec<Vec<char>> = stack_lines.iter()
        .map(parse_stack_line)
        .fold(stacks, |acc, stack_line| {
            let mut acc = acc.clone();
            for (i, x) in stack_line.into_iter().enumerate() {
                if x.is_some() {
                    acc[i].push(x.unwrap());
                }
            }
            acc
        });
    let stacks: Vec<Vec<char>> = stacks.into_iter().map(|mut x| {
        x.reverse();
        x
    }).collect();

    fn parse_moves(x: &&str) -> Option<(i32, i32, i32)> {
        let split: Vec<&str> = x.split(' ').collect();
        if split.len() < 3 {
            return None;
        }
        let nums: Vec<i32> = vec![split[1], split[3], split[5]].into_iter().map(|x| x.parse::<i32>().unwrap()).collect();
        Some((nums[0], nums[1], nums[2]))
    }
    let moves: Vec<(i32, i32, i32)> = moves.into_iter().map(parse_moves)
        .filter(|x| x.is_some())
        .map(|x| x.unwrap())
        .collect();

    let result_stacks: Vec<Vec<char>> = moves
        .into_iter()
        .fold(stacks, |curr_stacks, entry| {
            let mut stacks = curr_stacks.clone();
            let (from, to) = (entry.1 as usize, entry.2 as usize);
            for i in 0..entry.0 {
                let item = stacks[from - 1].pop().unwrap();
                stacks[to - 1].push(item);
            }
            stacks
        });
    let top_crates: String = result_stacks
        .into_iter()
        .map(|x| *x.last().unwrap())
        .collect();

    top_crates
}

fn parse_stack_line<'a>(line: &'a &'a str) -> Vec<Option<char>> {
    line.as_bytes().chunks(4)
        .map(|x| {
            if x[1] == b' ' {
                return None;
            }
            return Some(x[1] as char);
        })
        .collect()
}

fn day5p2(contents: String) -> String {
    let lines: Vec<&str> = contents.lines().collect();
    let (stack_lines, moves) = lines.split_at(8);
    let (_, moves) = moves.split_at(2);

    let stacks = vec![vec![]; 9];
    let stacks: Vec<Vec<char>> = stack_lines.iter()
        .map(parse_stack_line)
        .fold(stacks, |acc, stack_line| {
            let mut acc = acc.clone();
            for (i, x) in stack_line.into_iter().enumerate() {
                if x.is_some() {
                    acc[i].push(x.unwrap());
                }
            }
            acc
        });
    let stacks: Vec<Vec<char>> = stacks.into_iter().map(|mut x| {
        x.reverse();
        x
    }).collect();

    fn parse_moves(x: &&str) -> Option<(i32, i32, i32)> {
        let split: Vec<&str> = x.split(' ').collect();
        if split.len() < 3 {
            return None;
        }
        let nums: Vec<i32> = vec![split[1], split[3], split[5]].into_iter().map(|x| x.parse::<i32>().unwrap()).collect();
        Some((nums[0], nums[1], nums[2]))
    }
    let moves: Vec<(i32, i32, i32)> = moves.into_iter().map(parse_moves)
        .filter(|x| x.is_some())
        .map(|x| x.unwrap())
        .collect();

    let result_stacks: Vec<Vec<char>> = moves
        .into_iter()
        .fold(stacks, |curr_stacks, entry| {
            let mut stacks = curr_stacks.clone();
            let (num, from, to) = (entry.0 as usize, entry.1 as usize, entry.2 as usize);

            let from_stack = &mut stacks[from - 1];
            let items: Vec<char> = from_stack[(from_stack.len() - num)..].to_vec();
            let to_stack = &mut stacks[to - 1];

            for item in items {
                to_stack.push(item);
            }
            for i in 0..entry.0 {
                stacks[from - 1].pop();
            }

            stacks
        });
    let top_crates: String = result_stacks
        .into_iter()
        .map(|x| *x.last().unwrap())
        .collect();

    top_crates
}

fn day6p1(contents: String) -> String {
    get_first_marker(&contents, 4).to_string()
}

fn day6p2(contents: String) -> String {
    get_first_marker(&contents, 14).to_string()
}

fn get_first_marker(line: &str, marker_size: i32) -> i32 {
    let is_marker = |c: char, pos: i32, chars: &str| -> bool {
        let hs: HashSet<char> = HashSet::from_iter(chars.chars());
        hs.len() == (marker_size as usize)
    };

    for (i, marker) in line[3..].chars().enumerate() {
        if is_marker(marker, i as i32, &line[i..i+marker_size as usize]) {
            return (i) as i32 + marker_size
        }
    }

    0
}

fn day7p1(contents: String) -> String {
    String::new()
}

fn day7p2(contents: String) -> String {
    String::new()
}

fn day8p1(contents: String) -> String {
    String::new()
}

fn day8p2(contents: String) -> String {
    String::new()
}

fn day9p1(contents: String) -> String {
    String::new()
}

fn day9p2(contents: String) -> String {
    String::new()
}

fn day10p1(contents: String) -> String {
    String::new()
}

fn day10p2(contents: String) -> String {
    String::new()
}

fn day11p1(contents: String) -> String {
    String::new()
}

fn day11p2(contents: String) -> String {
    String::new()
}

fn day12p1(contents: String) -> String {
    String::new()
}

fn day12p2(contents: String) -> String {
    String::new()
}

fn day13p1(contents: String) -> String {
    String::new()
}

fn day13p2(contents: String) -> String {
    String::new()
}

fn day14p1(contents: String) -> String {
    String::new()
}

fn day14p2(contents: String) -> String {
    String::new()
}

fn day15p1(contents: String) -> String {
    String::new()
}

fn day15p2(contents: String) -> String {
    String::new()
}

fn day16p1(contents: String) -> String {
    String::new()
}

fn day16p2(contents: String) -> String {
    String::new()
}

fn day17p1(contents: String) -> String {
    String::new()
}

fn day17p2(contents: String) -> String {
    String::new()
}

fn day18p1(contents: String) -> String {
    String::new()
}

fn day18p2(contents: String) -> String {
    String::new()
}

fn day19p1(contents: String) -> String {
    String::new()
}

fn day19p2(contents: String) -> String {
    String::new()
}

fn day20p1(contents: String) -> String {
    String::new()
}

fn day20p2(contents: String) -> String {
    String::new()
}

fn day21p1(contents: String) -> String {
    String::new()
}

fn day21p2(contents: String) -> String {
    String::new()
}

fn day22p1(contents: String) -> String {
    String::new()
}

fn day22p2(contents: String) -> String {
    String::new()
}

fn day23p1(contents: String) -> String {
    String::new()
}

fn day23p2(contents: String) -> String {
    String::new()
}

fn day24p1(contents: String) -> String {
    String::new()
}

fn day24p2(contents: String) -> String {
    String::new()
}

fn day25p1(contents: String) -> String {
    String::new()
}

fn day25p2(contents: String) -> String {
    String::new()
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_stack_line() {
        let r = parse_stack_line(&"[V]     [B]                     [F]");

        assert_eq!(r, vec![Some('V'), None, Some('B'), None, None, None, None, None, Some('F')]);
    }

    #[test]
    fn test_char_marker() {
        let r = get_first_marker("bvwbjplbgvbhsrlpgdmjqwftvncz", 4);

        assert_eq!(r, 5);
    }
}