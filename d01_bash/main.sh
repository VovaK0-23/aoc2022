#! /bin/bash

file=$1

# The get_calories function reads the file line by line, adding up the calories for each Elf.
# The resulting calorie count for each Elf is stored as a new line in the 'elves' variable.
get_calories() {
    # The elve variable holds the calorie count for the current Elf.
    elve=0
    # The elves variable holds the calorie counts for all Elves, separated by newlines.
    elves=''
    # The while loop reads the file line by line.
    while read -r line; do
        # If the line is not empty (i.e., it contains a calorie count), add it to the current Elf's calorie count.
        if [[ "$line" != "" ]]; then
            ((elve+="$line"))
            # If the line is empty, it signals the end of the current Elf's calorie count.
            # Add the current Elf's calorie count to the list of calorie counts for all Elves.
            # Reset the current Elf's calorie count for the next Elf.
        else
            elves="${elves}${elve}\n"
            elve=0
        fi
    done < "$file"
    # Add the calorie count for the last Elf in the file to the list of calorie counts for all Elves.
    elves="${elves}${elve}\n"
    echo "$elves"
}

part1() {
    # Sort the calorie counts in descending order, and take the first (i.e., largest) calorie count.
    max="$(printf "$1" | sort -nr | head -n 1)"
    echo 'Part 1:'
    echo "$max"
}

part2() {
    # This line prints the first argument ($1) to the console,
    # sorts it in descending order (-nr),
    # takes the top 3 lines (head -n 3),
    # replaces newline characters with plus signs (sed -z 's/\n/+/g'),
    # removes the last plus sign (sed 's/.$/\n/'),
    # and passes it to bc for computation.
    # The result is stored in the total_calories variable.
    total_calories="$(printf "$1" \
        | sort -nr \
        | head -n 3 \
        | sed -z 's/\n/+/g' \
        | sed 's/.$/\n/' \
        | bc)"
    echo 'Part 2:'
    echo "$total_calories"
}

elves="$(get_calories)"
part1 "$elves"
part2 "$elves"
