#!/usr/bin/env nu 

let scores = {
    "A": 0
    "B": 1
    "C": 2
    "X": 0
    "Y": 1
    "Z": 2
}

let score_record_p1 = {
    "A X": [1 3],
    "A Y": [2 6],
    "A Z": [3 0],
    "B X": [1 0],
    "B Y": [2 3],
    "B Z": [3 6],
    "C X": [1 6],
    "C Y": [2 0],
    "C Z": [3 3]
}

let score_record_p2 = {
    "A X": [3 0],
    "A Y": [1 3],
    "A Z": [2 6],
    "B X": [1 0],
    "B Y": [2 3],
    "B Z": [3 6],
    "C X": [2 0],
    "C Y": [3 3],
    "C Z": [1 6]
}

def main [file: string --fast (-f)] {
    let parsed = (open $file | lines)

    if not $fast {
        $parsed
        | to_scores_direct_calc 1
        | math sum | wrap "Part 1"

        $parsed
        | to_scores_direct_calc 2
        | math sum | wrap "Part 2"
    } else {
        $parsed
        | to_scores $score_record_p1
        | math sum | wrap "Part 1"

        $parsed
        | to_scores $score_record_p2
        | math sum | wrap "Part 2"
    }
}

def to_scores_direct_calc [part: int] {
    each { |turn|
        $turn
        | split row ' '
        | each {|it| $scores | get $it }
    } | each { |it|
        let a = $it.0
        let b = $it.1
        if ($part == 1) {
            return ((($b + 4 - $a) mod 3) * 3 + ($b + 1))
        } else {
            return ($b * 3 + ($a + $b + 2) mod 3 + 1)
        }
    }
}

def to_scores [record: record] {
    each { |it|
        $record | get $it | math sum
    }
}
