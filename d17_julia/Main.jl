using LinearAlgebra
using PrettyTables

const TOWER_WIDTH = 7
const PADDING = (x = 2, y = 3)

mutable struct Tower
    data::Matrix{Bool}
    heights::Vector{Union{Int64,Nothing}}  # Store height by column
    min::Int64
end

function Tower(width, height)
    Tower(fill(false, height, width), [nothing for _ = 1:TOWER_WIDTH], 0)
end

function update_tower_heights!(tower::Tower)
    tower.heights = vec([
        any(tower.data[:, coord[2]]) ? coord[1] : nothing for
        coord in argmax(tower.data, dims = 1)
    ])
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
    println(part1)
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

    tower = Tower(TOWER_WIDTH, 0)
    height = 0
    for i = 1:num_rocks
        if i % 100 == 0
            println(i)
        end
        rock_index = mod(i - 1, length(rocks.data)) + 1
        rock = rocks.data[rock_index]
        height = simulate_rock_falling(rock, jets, tower, height)
    end

    return height
end

function simulate_rock_falling(rock, jets, tower, height)
    rock_nrows = size(rock, 1)
    add_rows = PADDING.y + rock_nrows
    tower.data = vcat(fill(false, add_rows, TOWER_WIDTH), tower.data)
    nrows = size(tower.data, 1)
    bufTower = Tower(TOWER_WIDTH, nrows)
    bufTower.data[1:rock_nrows, :] = rock
    new_height = add_rows + height

    shifted = false
    while true
        if shifted
            if (nothing in tower.heights && any(bufTower.data[nrows, :]))
                break
            end
            buf = circshift(bufTower.data, (1, 0))
            if any(buf .& tower.data)
                break
            end
            if height < new_height
                new_height -= 1
            end
            bufTower.data = buf
            shifted = false
        else
            jet = next_jet!(jets)
            if jet == '>'
                if !(true in bufTower.data[:, TOWER_WIDTH])
                    buf = circshift(bufTower.data, (0, 1))
                    if !any(buf .& tower.data)
                        bufTower.data = buf
                    end
                end
            else
                if !(true in bufTower.data[:, 1])
                    buf = circshift(bufTower.data, (0, -1))
                    if !any(buf .& tower.data)
                        bufTower.data = buf
                    end
                end
            end
            shifted = true
        end
    end

    tower.data = (bufTower.data .| tower.data)
    update_tower_heights!(tower)
    max = nothing in tower.heights ? 0 : maximum(tower.heights)
    min = minimum(filter(x -> x !== nothing, tower.heights))
    tower.data = tower.data[min:(max > 0 ? max + 8 : end), :]

    return new_height
end

main()
