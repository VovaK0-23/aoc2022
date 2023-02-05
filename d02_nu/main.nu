#!/usr/bin/env nu 

# A dictionary that maps shapes to scores
let scores = {
    "A": 0
    "B": 1
    "C": 2
    "X": 0
    "Y": 1
    "Z": 2
}

# Define a dictionaries containing scores for each combination of two characters
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
    # Read the contents of the file
    let parsed = (open $file | lines)

    if not $fast {
        # Calculate score using the direct calculation method
        $parsed
        | to_scores_direct_calc 1
        | math sum | wrap "Part 1"

        $parsed
        | to_scores_direct_calc 2
        | math sum | wrap "Part 2"
    } else {
        # Calculate score using the record lookup method
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
        # Each turn string is split into a list of strings using the split function
        $turn
        | split row ' '
        | each {|it| $scores | get $it }
    }
    | each { |it|
        # The `each` function takes a list of pairs of values and transforms them into a new list
        # In this case, each pair of values represents the score of two players in a turn
        let a = $it.0
        let b = $it.1
        if ($part == 1) {
               # (($b + 4 - $a) mod 3) * 3 computes the difference between the two moves and maps it to a value between 0 and 6.
               # For example, if $a is 0 and $b is 1, the difference is 1 and the mapped value would be 3.
               # If $a is 2 and $b is 0, the difference is 2 and the mapped value would be 6.
               # ($b + 1) adds the value of the move made by you to the final score.
               # Finally, the function returns the sum of the two values calculated above, which represents the total score of a single round following the strategy guide.
            return ((($b + 4 - $a) mod 3) * 3 + ($b + 1))
        } else {
               # The first term, $b * 3, provides a base score of 3, 6 or 9 depending on the required outcome ($b),
               # which could be a win, draw or loss respectively.
               # The second term, ($a + $b + 2) mod 3, is used to determine the outcome of the round.
               # If the outcome is a win, the result is 1;
               # if the outcome is a draw, the result is 0;
               # if the outcome is a loss, the result is 2.
               # The final term, 1, is added to the result to account for the basic point that you get in each round,
               # regardless of the outcome.
            return ($b * 3 + ($a + $b + 2) mod 3 + 1)
        }
    }
}

def to_scores [record: record] {
    each { |it|
        # The `get` function is used to retrieve the value in the record for the given key `it`
        # The result is then passed to the `math sum` function, which calculates the sum of the values in the list
        $record | get $it | math sum
    }
}
