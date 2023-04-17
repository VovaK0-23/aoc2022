defmodule Main do
  defmodule Node do
    @type t :: %Node{
            height: integer,
            id: integer,
            neighbors: [integer],
            final: bool,
            start: boolean
          }
    defstruct [:id, :height, :neighbors, :final, :start]

    @spec new(integer, integer, bool) :: Node.t()
    def new(id, height, final \\ false, start \\ false) do
      %Node{id: id, height: height, neighbors: [], final: final, start: start}
    end
  end

  @spec main :: :ok
  def main do
    args = System.argv()

    if length(args) == 1 do
      case File.read(hd(args)) do
        {:ok, content} ->
          solve(content)

        {:error, :enoent} ->
          IO.puts("File not found.")

        {:error, reason} ->
          IO.puts("Error: #{reason}")
      end
    else
      IO.puts("Usage: elixir main.ex <input-path>")
    end
  end

  @spec parse([String.t()]) :: [[Node.t()]]
  def parse(lines) do
    lines
    |> Enum.map(&String.to_charlist(&1))
    |> Enum.flat_map(& &1)
    |> Enum.with_index(fn letter, index ->
      height =
        case letter do
          83 ->
            1

          69 ->
            26

          _ ->
            alphabet_to_number(letter)
        end

      Node.new(index + 1, height, letter == 69, letter == 83)
    end)
  end

  def init_neighbors(nodes, row_length) do
    Enum.reduce(nodes, nodes, fn node, nodes ->
      ids =
        if rem(node.id, row_length) == 0 do
          [row_length + node.id, node.id - row_length, node.id - 1]
        else
          if rem(node.id - 1, row_length) == 0 do
            [row_length + node.id, node.id - row_length, node.id + 1]
          else
            [row_length + node.id, node.id - row_length, node.id - 1, node.id + 1]
          end
        end
        |> Enum.filter(&(&1 >= 1 and &1 <= length(nodes)))
        |> Enum.filter(&(node.height + 1 >= Enum.at(nodes, &1 - 1).height))

      List.replace_at(nodes, node.id - 1, %Node{node | neighbors: ids})
    end)
  end

  @spec solve(String.t()) :: :ok
  def solve(content) do
    lines =
      content
      |> String.trim()
      |> String.split("\n")

    row_length = String.length(hd(lines))

    nodes =
      parse(lines)
      |> init_neighbors(row_length)

    shortest_path =
      Enum.reduce(Enum.filter(nodes, &(&1.height == 1)), nil, fn start_node, shortest_path ->
        path_len =
          case bfs(start_node, nodes) do
            nil -> nil
            path -> length(path) - 1
          end

        case shortest_path do
          nil -> path_len
          _ -> [shortest_path, path_len] |> Enum.min()
        end
      end)

    IO.puts(shortest_path)
    IO.puts(length(bfs(Enum.find(nodes, & &1.start), nodes)) - 1)
  end

  @spec alphabet_to_number(integer) :: integer
  def alphabet_to_number(char_code) do
    number = char_code - ?a + 1
    number
  end

  def bfs(start_node, nodes) do
    acc = %{
      queue: [start_node],
      visited: MapSet.new(),
      parents: Map.new(),
      nodes: nodes,
      start_node: start_node
    }

    bfs(acc)
  end

  def bfs(acc) do
    if Enum.empty?(acc.queue) do
      nil
    else
      {current_node, queue} = List.pop_at(acc.queue, 0)
      acc = %{acc | queue: queue}

      if current_node.final do
        backtrack(%{path: [current_node], current_node: current_node, parents: acc.parents})
      else
        acc =
          if !MapSet.member?(acc.visited, current_node.id) do
            acc = %{acc | visited: MapSet.put(acc.visited, current_node.id)}
            print_nodes(163, acc.nodes, acc.visited, acc.start_node)

            Enum.reduce(current_node.neighbors, acc, fn neighbor_id, acc ->
              if MapSet.member?(acc.visited, neighbor_id) do
                acc
              else
                neighbor = Enum.at(acc.nodes, neighbor_id - 1)

                %{
                  acc
                  | queue: acc.queue ++ [neighbor],
                    parents: Map.put(acc.parents, neighbor, current_node)
                }
              end
            end)
          else
            acc
          end

        bfs(acc)
      end
    end
  end

  defp backtrack(acc) do
    if Map.get(acc.parents, acc.current_node) != nil do
      current_node = Map.get(acc.parents, acc.current_node)

      backtrack(%{
        acc
        | current_node: current_node,
          path: [current_node | acc.path]
      })
    else
      acc.path
    end
  end

  defp print_nodes(row_length, nodes, visited, start_node) do
    x =
      Enum.join(
        Enum.map(nodes, fn node ->
          x =
            if node.final do
              "*"
            else
              if start_node.id == node.id do
                "@"
              else
                if MapSet.member?(visited, node.id) do
                  "!"
                else
                  "."
                end
              end
            end

          x =
            if rem(node.id, row_length) == 0 do
              x <> "\n"
            else
              x
            end

          x
        end),
        ""
      )

    IO.puts(IO.ANSI.clear())
    IO.puts(x)
  end
end

Main.main()
