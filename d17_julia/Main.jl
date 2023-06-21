using LinearAlgebra
using Printf

const TOWER_WIDTH = 7
const PADDING = (x = 2, y = 3)

mutable struct Tower
    data::Matrix{Bool}
    heights::Vector{Union{Int64,Nothing}}  # Store height by column
end

function Tower(height)
    Tower(fill(false, height, TOWER_WIDTH), [nothing for _ = 1:TOWER_WIDTH])
end

function update_tower!(tower::Tower, matrix::BitMatrix)
    min = typemax(Int64)
    max = 0
    for coord in argmax(matrix, dims = 1)
        row, col = Tuple(coord)
        tower.heights[col] = if any(matrix[:, col])
            if min > row
                min = row
            end
            if max !== nothing && max < row
                max = row
            end
            row
        else
            max = nothing
            nothing
        end
    end
    tower.data = matrix[min:(max !== nothing ? max + 8 : end), :]
end

struct Rocks
    data::Vector{Matrix{Bool}}
end

function Rocks(rocks::Vector{Vector{String}})
    Rocks([
        [
            (idx = i - PADDING.x; idx >= 1 && idx <= length(block) && block[idx] == '#') for
            block in rock, i = 1:TOWER_WIDTH
        ] for rock in rocks
    ])
end

struct Jets
    buffer::Vector{Char}
    index::Ref{Int}
end

function Jets(jets::Vector{Char})
    Jets(jets, Ref(1))
end

function next_jet!(jets::Jets)
    jet = jets.buffer[jets.index[]]
    n = length(jets.buffer)
    jets.index[] = (jets.index[] % n) + 1
    jet
end

function main()
    filepath = ARGS[1]
    jets = Jets(read_file(filepath))
    part1 = calculate_tower_height(jets, 2022)
    print("Part 1: ")
    println(@sprintf("%.f", part1))

    jets.index[] = 1
    part2 = calculate_tower_height(jets, 1000000000000)
    print("Part 2: ")
    println(@sprintf("%.f", part2))
end

function read_file(filepath)
    println(filepath)
    file = open(filepath, "r")
    contents = rstrip(read(file, String))
    close(file)
    collect(contents)
end

function calculate_tower_height(jets, num_rocks)
    rocks =
        [["####"], [".#.", "###", ".#."], ["..#", "..#", "###"], fill("#", 4), ["##", "##"]]
    rocks = Rocks(rocks)

    tower_cache = Dict{Tuple,Tuple}()
    tower = Tower(0)
    height = 0

    remainder = 0
    divisor = 0
    cache_height = 0

    for i = 1:num_rocks
        rock_index = mod(i - 1, length(rocks.data)) + 1
        rock = rocks.data[rock_index]

        height = simulate_rock_falling(rock, jets, tower, height)

        cache_key = (tower.data, jets.index[])
        if cache_key in keys(tower_cache)
            (remainder, cache_height) = tower_cache[cache_key]
            divisor = i - remainder
            break
        else
            tower_cache[cache_key] = (i, height)
        end
    end

    if cache_height > 0
        closest_num = num_rocks - (num_rocks % divisor) + remainder

        if closest_num > num_rocks
            closest_num -= divisor
        end

        repeats = (closest_num - remainder) / divisor
        height = (height - cache_height) * repeats + cache_height
        for i = closest_num+1:num_rocks
            rock_index = mod(i - 1, length(rocks.data)) + 1
            rock = rocks.data[rock_index]

            height = simulate_rock_falling(rock, jets, tower, height)
        end
    end

    return height
end

function simulate_rock_falling(rock, jets, tower, height)
    rock_nrows = size(rock, 1)
    add_rows = PADDING.y + rock_nrows
    tower.data = vcat(fill(false, add_rows, TOWER_WIDTH), tower.data)
    nrows = size(tower.data, 1)
    bufTower = fill(false, nrows, TOWER_WIDTH)
    bufTower[1:rock_nrows, :] = rock
    new_height = add_rows + height

    shifted = false
    while true
        if shifted
            if (nothing in tower.heights && any(bufTower[nrows, :]))
                break
            end
            buf = circshift(bufTower, (1, 0))
            if any(buf .& tower.data)
                break
            end
            if height < new_height
                new_height -= 1
            end
            bufTower = buf
            shifted = false
        else
            jet = next_jet!(jets)
            if jet == '>'
                if !(true in bufTower[:, TOWER_WIDTH])
                    buf = circshift(bufTower, (0, 1))
                    if !any(buf .& tower.data)
                        bufTower = buf
                    end
                end
            else
                if !(true in bufTower[:, 1])
                    buf = circshift(bufTower, (0, -1))
                    if !any(buf .& tower.data)
                        bufTower = buf
                    end
                end
            end
            shifted = true
        end
    end

    update_tower!(tower, (bufTower .| tower.data))

    return new_height
end

main()
