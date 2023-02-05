module Solution = struct
  open Array
  type direction = Up | Down | Left | Right

  (* iter_grid takes a 2D grid and a function, and applies the function
     to every cell in the grid by going through every column then every row.
  *)
  let iter_grid grid fn =
    let rows = length grid in
    let cols = length grid.(0) in
    for col = 0 to cols - 1 do
      for row = 0 to rows - 1 do
        fn (row, col)
      done
    done

  (* look takes a direction, a 2D grid, a row, and a column, and returns a count of
     the number of cells visible in the given direction, and a boolean indicating
     if the cell itself is visible.
  *)
  let look dir grid row col =
    let rows = length grid in
    let cols = length grid.(0) in
    let count = ref 0 in
    let visible = ref true in

    (* loop is a helper function for look that iterates through the grid in a
       given direction and increments the count for every cell with a lower value,
       until it finds blocking tree or edge.
    *)
    let loop start limit inc comparison =
      let i = ref start in
      while !i <> limit && !visible do
        i := !i + inc;
        if comparison i < grid.(col).(row) then incr count
        else (
          incr count;
          visible := false)
      done;
      (!count, !visible)
    in

    match dir with
    | Up -> loop col 0 (-1) (fun x -> grid.(!x).(row))
    | Down -> loop col (cols - 1) 1 (fun x -> grid.(!x).(row))
    | Left -> loop row 0 (-1) (fun x -> grid.(col).(!x))
    | Right -> loop row (rows - 1) 1 (fun x -> grid.(col).(!x))

  (* count_visible_trees takes a 2D grid and returns the number of cells
     in the grid that are visible in at least one direction.
  *)
  let count_visible_trees grid =
    let count_visible = ref 0 in
    iter_grid grid (fun (row, col) ->
        (* If the cell is visible from at least one direction, increment the counter *)
        if
          snd (look Up grid row col)
          || snd (look Down grid row col)
          || snd (look Left grid row col)
          || snd (look Right grid row col)
        then incr count_visible);
    !count_visible

  (* count_scenic_score takes a 2D grid and returns the maximum scenic score
     of any cell in the grid. The scenic score of a cell is the product of
     the number of cells visible in each direction.
  *)
  let count_scenic_score grid =
    let max_scenic_score = ref 0 in
    iter_grid grid (fun (row, col) ->
        let scenic_score =
          fst (look Up grid row col)
          * fst (look Down grid row col)
          * fst (look Left grid row col)
          * fst (look Right grid row col)
        in
        max_scenic_score := max !max_scenic_score scenic_score);
    !max_scenic_score

  let part1 grid =
    let result = count_visible_trees grid in
    print_string "Part 1: ";
    print_int result;
    print_newline ()

  let part2 grid =
    let result = count_scenic_score grid in
    print_string "Part 2: ";
    print_int result;
    print_newline ()
end

module File = struct
  (* read function reads the input from a file with the given filename.
   * It returns the contents of the file as a 2D integer array where each row is represented as an array of integers.
   * The values of the integers are obtained by subtracting the ASCII code of '0' from the ASCII code of the character.
   * This is done so that the characters are converted to their numerical equivalent.
   *)
  let read filename =
    let to_int_array string =
      Array.init (String.length string) (fun i ->
          int_of_char string.[i] - int_of_char '0')
    in
    let input_channel = open_in filename in
    let rec read_lines acc =
      try
        let line = input_line input_channel in
        read_lines (to_int_array line :: acc)
      with End_of_file ->
        close_in input_channel;
        acc
    in
    (* the input from the file is reversed before being converted to an array.
     * This is done so that the contents of the file are stored in the array from the top to bottom order,
     * as it was in the file.
     *)
    Array.of_list (List.rev (read_lines []))
end

let () =
  let filename = Sys.argv.(1) in
  let grid = File.read filename in
  Solution.part1 grid;
  Solution.part2 grid
