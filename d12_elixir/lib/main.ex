defmodule NodeM do
  @type t :: %NodeM{
          id: integer,
          height: integer,
          distance: integer,
          neighbors: [integer],
          final: boolean,
          start: boolean,
          data: String.t()
        }
  defstruct [:id, :height, :distance, :neighbors, :final, :start, :data]

  @spec parse([String.t()], integer) :: [NodeM.t()]
  def parse(lines, row_length) do
    height = fn letter ->
      case letter do
        83 ->
          1

        69 ->
          26

        _ ->
          letter - ?a + 1
      end
    end

    chars =
      lines
      |> Enum.map(&String.to_charlist(&1))
      |> Enum.flat_map(& &1)

    node_length = length(chars)

    Enum.with_index(chars, fn letter, index ->
      node_id = index + 1
      node_height = height.(letter)

      [up, down, left, right] = [
        row_length + node_id,
        node_id - row_length,
        node_id - 1,
        node_id + 1
      ]

      # if last in row no right neighbor
      ids =
        if rem(node_id, row_length) == 0 do
          [up, down, left]
        else
          # if first in row no left neighbor
          if rem(node_id - 1, row_length) == 0 do
            [up, down, right]
          else
            [up, down, left, right]
          end
        end
        |> Enum.filter(&(&1 >= 1 and &1 <= node_length))
        |> Enum.filter(&(node_height + 1 >= height.(Enum.at(chars, &1 - 1))))

      %NodeM{
        id: node_id,
        height: node_height,
        distance: node_length,
        neighbors: ids,
        final: letter == 69,
        start: letter == 83,
        data: to_string(<<letter>>)
      }
    end)
  end
end

defmodule Search do
  @spec bfs(NodeM.t(), [NodeM.t()], boolean, integer) :: nil | {[NodeM.t()], String.t()}
  def bfs(start_node, nodes, print, row_length) do
    acc = %{
      queue: [start_node],
      visited: MapSet.new(),
      parents: Map.new(),
      nodes: nodes,
      start_node: start_node,
      print: print,
      row_length: row_length
    }

    bfs(acc)
  end

  @spec bfs(%{
          :queue => [NodeM.t()],
          :visited => MapSet.t(),
          :parents => map,
          :nodes => [NodeM.t()],
          :start_node => NodeM.t(),
          :print => boolean,
          :row_length => integer
        }) :: nil | {[NodeM.t()], String.t()}
  def bfs(acc) do
    if Enum.empty?(acc.queue) do
      nil
    else
      {current_node, queue} = List.pop_at(acc.queue, 0)
      acc = %{acc | queue: queue}

      if current_node.final do
        path =
          backtrack(%{path: [current_node], current_node: current_node, parents: acc.parents})

        {path, print_nodes(acc.row_length, acc.nodes, acc.visited, acc.start_node, path)}
      else
        bfs(process_node(acc, current_node))
      end
    end
  end

  defp process_node(acc, current_node) do
    case MapSet.member?(acc.visited, current_node.id) do
      true ->
        acc

      false ->
        acc = %{acc | visited: MapSet.put(acc.visited, current_node.id)}

        if acc.print do
          frame = print_nodes(acc.row_length, acc.nodes, acc.visited, acc.start_node)
          IO.puts(IO.ANSI.clear())
          IO.puts(frame)
        end

        Enum.reduce(current_node.neighbors, acc, fn neighbor_id, acc ->
          neighbor = Enum.at(acc.nodes, neighbor_id - 1)

          if neighbor.distance > current_node.distance do
            neighbor = %NodeM{neighbor | distance: current_node.distance + 1}

            %{
              acc
              | queue: acc.queue ++ [neighbor],
                nodes: Enum.map(acc.nodes, &if(&1.id == neighbor.id, do: neighbor, else: &1)),
                parents: Map.put(acc.parents, neighbor_id, current_node)
            }
          else
            acc
          end
        end)
    end
  end

  defp backtrack(acc) do
    if Map.get(acc.parents, acc.current_node.id) != nil do
      current_node = Map.get(acc.parents, acc.current_node.id)

      backtrack(%{
        acc
        | current_node: current_node,
          path: [current_node | acc.path]
      })
    else
      acc.path
    end
  end

  defp print_nodes(row_length, nodes, visited, start_node, path \\ []) do
    nodes
    |> Enum.map(fn node ->
      x =
        if(node.final,
          do: "*",
          else:
            if(start_node.id == node.id,
              do: "@",
              else:
                if(Enum.member?(path, node),
                  do: "+",
                  else: if(MapSet.member?(visited, node.id), do: node.data, else: ".")
                )
            )
        )

      if rem(node.id, row_length) == 0, do: x <> "\n", else: x
    end)
    |> Enum.join("")
  end
end

defmodule Main do
  @spec main :: :ok
  def main do
    args = System.argv()
    if args > 0 do
      case File.read(Enum.at(args, 0) ) do
        {:ok, content} ->
          solve(content, Enum.at(args, 1) != "--no-print")

        {:error, :enoent} ->
          IO.puts("Error: File not found")

        {:error, reason} ->
          IO.puts("Error: #{reason}")
      end
    else
      IO.puts("Usage: elixir main.ex <input-path> --no-print")
    end
  end

  @spec solve(String.t(), boolean) :: :ok
  def solve(content, print \\ true) do
    lines =
      content
      |> String.trim()
      |> String.split("\n")

    row_length = String.length(hd(lines))
    nodes = NodeM.parse(lines, row_length)
    IO.puts("Computing...")
    {part1, part1_frame} = part1(nodes, print, row_length)
    {part2, part2_frame} = part2(nodes, print, row_length)
    IO.puts(IO.ANSI.clear())
    IO.puts("----------------")
    IO.puts(part1_frame)
    IO.puts("----------------")
    IO.puts(part2_frame)
    IO.puts("----------------")
    IO.puts("Part 1: #{part1}")
    IO.puts("Part 2: #{part2}")
  end

  @spec part1([NodeM.t()], boolean, integer) :: {integer, String.t()} | {nil, nil}
  def part1(nodes, print, row_length) do
    start_node = %NodeM{(nodes |> Enum.find(& &1.start)) | distance: 0}

    nodes =
      Enum.map(nodes, fn node ->
        if node.start do
          start_node
        else
          node
        end
      end)

    case Search.bfs(start_node, nodes, print, row_length) do
      {path, nodes_frame} -> {length(path) - 1, nodes_frame}
      _ -> {nil, nil}
    end
  end

  @spec part2([NodeM.t()], boolean, integer) :: {integer, String.t()} | {nil, nil}
  def part2(nodes, print, row_length) do
    super_node = %NodeM{
      id: length(nodes) + 1,
      height: 1,
      distance: 0,
      neighbors: nodes |> Enum.filter(&(&1.height == 1)) |> Enum.map(& &1.id),
      final: false,
      start: true,
      data: "a"
    }

    nodes =
      Enum.map(nodes, fn node ->
        if node.height == 1,
          do: %NodeM{node | neighbors: node.neighbors ++ [super_node.id]},
          else: node
      end)

    nodes = nodes ++ [super_node]

    case Search.bfs(super_node, nodes, print, row_length) do
      {path, nodes_frame} -> {length(path) - 2, nodes_frame}
      _ -> {nil, nil}
    end
  end
end

Main.main()
