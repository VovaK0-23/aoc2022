#! /bin/bash

file=$1
part1() {
    elve=0
    max=0
    while read -r line; do
        if [[ "$line" != "" ]]; then
            ((elve+="$line"))
        else
            test "$elve" -gt "$max" && max=$elve
            elve=0
        fi
    done < "$file"
    test "$elve" -gt "$max" && max=$elve
    echo 'Part 1:'
    echo "$max"
}

part2() {
    elve=0
    elves=''
    while read -r line; do
        if [[ "$line" != "" ]]; then
            ((elve+="$line"))
        else
            elves="${elves}${elve}\n"
            elve=0
        fi
    done < "$file"
    elves="${elves}${elve}\n"

    elves="$(printf "$elves" \
            | sort -nr \
            | head -n 3 \
            | sed -z 's/\n/+/g' \
            | sed 's/.$/\n/' \
            | bc)" 
    echo 'Part 2:'
    echo "$elves"
}

part1
part2
