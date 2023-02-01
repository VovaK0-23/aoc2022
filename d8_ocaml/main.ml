module Solution = struct
  open Array
  type direction = Up | Down | Left | Right

  let look dir grid row col =
    let rows = length grid in
    let cols = length grid.(0) in
    let count = ref 0 in
    try
      match dir with
      | Up ->
          for i = col - 1 downto 0 do
            if grid.(i).(row) < grid.(col).(row) then incr count else raise Exit
          done;
          (!count, true)
      | Down ->
          for i = col + 1 to rows - 1 do
            if grid.(i).(row) < grid.(col).(row) then incr count else raise Exit
          done;
          (!count, true)
      | Left ->
          for i = row - 1 downto 0 do
            if grid.(col).(i) < grid.(col).(row) then incr count else raise Exit
          done;
          (!count, true)
      | Right ->
          for i = row + 1 to cols - 1 do
            if grid.(col).(i) < grid.(col).(row) then incr count else raise Exit
          done;
          (!count, true)
    with Exit ->
      incr count;
      (!count, false)

  let count_visible_trees grid =
    let rows = length grid in
    let cols = length grid.(0) in
    let count_visible = ref 0 in
    for col = 0 to cols - 1 do
      for row = 0 to rows - 1 do
        if col = 0 || row = 0 || col = cols - 1 || row = rows - 1 then
          incr count_visible
        else if
          snd (look Up grid row col)
          || snd (look Down grid row col)
          || snd (look Left grid row col)
          || snd (look Right grid row col)
        then incr count_visible
      done
    done;
    !count_visible

  let count_scenic_score grid =
    let rows = length grid in
    let cols = length grid.(0) in
    let max_scenic_score = ref 0 in
    for col = 0 to cols - 1 do
      for row = 0 to rows - 1 do
        let scenic_score =
          fst (look Up grid row col)
          * fst (look Down grid row col)
          * fst (look Left grid row col)
          * fst (look Right grid row col)
        in
        max_scenic_score := max !max_scenic_score scenic_score
      done
    done;
    !max_scenic_score

  let part1 grid =
    let result = count_visible_trees grid in
    print_int result;
    print_newline ()

  let part2 grid =
    let result = count_scenic_score grid in
    print_int result;
    print_newline ()
end

module File = struct
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
    Array.of_list (List.rev (read_lines []))
end

let () =
  let filename = Sys.argv.(1) in
  let grid = File.read filename in
  Solution.part1 grid;
  Solution.part2 grid
