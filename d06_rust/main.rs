use std::env;
use std::fs;

fn find_start_of_marker(datastream: &str, num: usize) -> usize {
    let mut recent_chars = Vec::new(); // Vector to store the most recent characters
    for (i, c) in datastream.chars().enumerate() {
        recent_chars.push(c);
        if recent_chars.len() > num {
            recent_chars.remove(0);
        }
        if recent_chars.len() == num
            && recent_chars
                .iter()
                .collect::<std::collections::HashSet<_>>()
                .len()
                == num
        {
            // Check if the characters are all different
            return i + 1;
        }
    }
    return datastream.len();
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: program <filename>");
        return;
    }
    let filename = &args[1];
    let datastream = fs::read_to_string(filename).expect("Error reading file");
    let result1 = find_start_of_marker(&datastream, 4);
    let result2 = find_start_of_marker(&datastream, 14);
    println!("Part 1: {}", result1);
    println!("Part 2: {}", result2);
}
