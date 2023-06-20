using LinearAlgebra
using PrettyTables

mutable struct Tower
    data::Matrix{Bool}
end

function Tower(width, height)
    Tower(fill(false, height, width))
end

mutable struct Jets
    buffer::Vector{Char}
    index::Int
end

function Jets(jets::Vector{Char})
    return Jets(jets, 1)
end

function next_jet!(jets::Jets)
    jet = jets.buffer[jets.index]
    n = length(jets.buffer)
    jets.index = (jets.index % n) + 1
    return jet
end

TOWER_WIDTH = 7

function main()
    arg1 = ARGS[1]
    println(arg1)
    file = open(arg1, "r")
    contents = rstrip(read(file, String))
    close(file)
    jets = Jets(collect(contents))
    tower_height = calculate_tower_height(jets, 2022)
    print("Part 1: ")
    println(tower_height)
end

function calculate_tower_height(jets, num_rocks)
    rocks =
        [["####"], [".#.", "###", ".#."], ["..#", "..#", "###"], fill("#", 4), ["##", "##"]]
    leftpad = 2

    rocks = [
        [
            (idx = i - leftpad; idx >= 1 && idx <= length(block) && block[idx] == '#')
            for block in rock, i = 1:TOWER_WIDTH
        ] for rock in rocks
    ]

    println(rocks)

    tower = Tower(TOWER_WIDTH, 3)
    height = 0
    for i = 1:num_rocks
        rock_index = mod(i - 1, length(rocks)) + 1
        rock = rocks[rock_index]
        tower.data = simulate_rock_falling(rock, jets, tower, height)
        height = find_highest_row(tower.data)
    end

    return height
end

function simulate_rock_falling(rock, jets, tower, height)
    newTower = Tower(TOWER_WIDTH, height + 3)
    newTower.data = vcat(rock, newTower.data)

    diff = size(newTower.data, 1) - size(tower.data, 1)
    if diff > 0
        data = vcat(fill(false, diff, TOWER_WIDTH), tower.data)
    elseif diff < 0
        newTower.data = vcat(fill(false, -1 * diff, TOWER_WIDTH), newTower.data)
        data = tower.data
    else
        data = tower.data
    end

    falling = true
    shifted = false
    while falling
        if shifted
            if any(newTower.data[size(newTower.data, 1), :])
                falling = false
                continue
            end
            buf = circshift(newTower.data, (1, 0))
            if any(buf .& data)
                falling = false
                continue
            end
            newTower.data = buf
            shifted = false
        else
            jet = next_jet!(jets)
            if jet == '>'
                if !any(newTower.data[:, TOWER_WIDTH])
                    buf = circshift(newTower.data, (0, 1))
                    if !any(buf .& data)
                        newTower.data = buf
                    end
                end
            else
                if !any(newTower.data[:, 1])
                    buf = circshift(newTower.data, (0, -1))
                    if !any(buf .& data)
                        newTower.data = buf
                    end
                end
            end
            shifted = true
        end
    end

    return newTower.data .| data
end

function find_highest_row(matrix::Matrix{Bool})
    nrows, _ = size(matrix)

    return nrows + 1 - findfirst(vec(any(matrix, dims = 2)))
end



main()
