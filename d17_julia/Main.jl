using LinearAlgebra
using Printf
show = "--show" in ARGS
if show
    import Pkg
    if Base.find_package("Plots") == nothing
        Pkg.add("Plots")
    end
    using Plots
    default(legend = false, c =:viridis, xticks=[], yticks=[])
end


const TOWER_WIDTH = 7
const PADDING = (x = 2, y = 3)

mutable struct Tower
    data::Matrix{Bool}
    bottom::Bool
end

Tower(height) = Tower(fill(false, height, TOWER_WIDTH), true)

function update_tower!(tower::Tower, matrix::BitMatrix)
    min, max = typemax(Int64), 0
    tower.bottom = false

    for coord in argmax(matrix, dims = 1)
        row, col = Tuple(coord)
        if any(matrix[:, col])
            min = min > row ? row : min
            max = (!tower.bottom && max < row) ? row : max
        else
            tower.bottom = true
        end
    end

    tower.data = matrix[min:(!tower.bottom ? max + 8 : end), :]
end

struct Rocks
    data::Vector{Matrix{Bool}}
end

Rocks(rocks::Vector{Vector{String}}) = Rocks([
    [
        (idx = i - PADDING.x; idx >= 1 && idx <= length(block) && block[idx] == '#') for
        block in rock, i = 1:TOWER_WIDTH
    ] for rock in rocks
])

get_rock(rocks::Rocks, i::Int) = rocks.data[mod(i - 1, length(rocks.data))+1]

struct Jets
    buffer::Vector{Char}
    index::Ref{Int}
end

Jets(jets::Vector{Char}) = Jets(jets, Ref(1))

function next_jet!(jets::Jets)
    jet = jets.buffer[jets.index[]]
    n = length(jets.buffer)
    jets.index[] = (jets.index[] % n) + 1
    jet
end

function read_file(filepath)
    println("Input file: ", filepath)
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

    divisor = 0
    cached_index, cached_height = 0, 0

    for i = 1:num_rocks
        rock = get_rock(rocks, i)
        height = simulate_rock_falling(rock, jets, tower, height)

        if show && i % 2 == 0
          display(heatmap((tower.data), yflip=true, aspect_ratio = 1))
        end

        cache_key = (tower.data, jets.index[])
        if cache_key in keys(tower_cache)
            (cached_index, cached_height) = tower_cache[cache_key]
            divisor = i - cached_index
            break
        else
            tower_cache[cache_key] = (i, height)
        end
    end

    if cached_height > 0
        closest_num = num_rocks - divisor - (num_rocks % divisor) + cached_index
        height =
            (height - cached_height) * ((closest_num - cached_index) / divisor) +
            cached_height

        for i = closest_num+1:num_rocks
            rock = get_rock(rocks, i)
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
    current_rock = fill(false, nrows, TOWER_WIDTH)
    current_rock[1:rock_nrows, :] = rock
    new_height = add_rows + height

    shifted_down = true
    while true
        if shifted_down
            if next_jet!(jets) == '>'
                if !(true in current_rock[:, TOWER_WIDTH])
                    buf = circshift(current_rock, (0, 1))
                    if !any(buf .& tower.data)
                        current_rock = buf
                    end
                end
            else
                if !(true in current_rock[:, 1])
                    buf = circshift(current_rock, (0, -1))
                    if !any(buf .& tower.data)
                        current_rock = buf
                    end
                end
            end
            shifted_down = false
        else
            if tower.bottom && any(current_rock[nrows, :])
                break
            end
            buf = circshift(current_rock, (1, 0))
            if any(buf .& tower.data)
                break
            end
            new_height -= (height < new_height) ? 1 : 0
            current_rock = buf
            shifted_down = true
        end
    end

    update_tower!(tower, (current_rock .| tower.data))

    return new_height
end

if length(ARGS) > 0
    filepath = ARGS[1]
    jets = Jets(read_file(filepath))

    part1 = calculate_tower_height(jets, 2022)
    print("Part 1: ")
    println(@sprintf("%.f", part1))

    jets.index[] = 1
    part2 = calculate_tower_height(jets, 1000000000000)
    print("Part 2: ")
    println(@sprintf("%.f", part2))
else
    println("Usage: julia Main.jl <input-path> --show")
end
