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
    Enum.with_index(lines, fn line, row ->
      Enum.with_index(String.to_charlist(line), fn letter, col ->
        id = 1 + col + row * String.length(line)

        height =
          case letter do
            83 ->
              1

            69 ->
              26

            _ ->
              alphabet_to_number(letter)
          end

        Node.new(id, height, letter == 69, letter == 83)
      end)
    end)
  end

  @spec solve(String.t()) :: :ok
  def solve(content) do
    lines =
      content
      |> String.trim()
      |> String.split("\n")

    nodes2d = parse(lines)

    row_length = length(hd(nodes2d))
    nodes_length = length(nodes2d) * row_length

    nodes = Enum.flat_map(nodes2d, & &1)

    IO.puts(row_length)

    nodes =
      Enum.reduce(nodes, nodes, fn node, nodes ->
        ids =
          if rem(node.id, row_length) == 0 do
            IO.puts("right edge")
            [row_length + node.id, node.id - row_length, node.id - 1]
          else
            if rem(node.id - 1, row_length) == 0 do
              IO.puts("left edge")
              [row_length + node.id, node.id - row_length, node.id + 1]
            else
              [row_length + node.id, node.id - row_length, node.id - 1, node.id + 1]
            end
          end

        IO.puts(Enum.join(ids, " "))

        ids =
          ids
          |> Enum.filter(&(&1 >= 1 and &1 <= nodes_length))

        IO.puts(Enum.join(ids, " "))

        ids = ids |> Enum.filter(&(node.height + 1 >= Enum.at(nodes, &1 - 1).height))

        List.replace_at(nodes, node.id - 1, %Node{node | neighbors: ids})
      end)

    IO.inspect(lines)
    IO.inspect(length(bfs(Enum.find(nodes, & &1.start), nodes)) - 1)
  end

  @spec alphabet_to_number(integer) :: integer
  def alphabet_to_number(char_code) do
    number = char_code - ?a + 1
    String.to_integer(to_string(number))
  end

  def bfs(start_node, nodes) do
    acc = %{queue: [start_node], visited: MapSet.new(), parents: Map.new(), nodes: nodes}
    # IO.inspect(acc.nodes)
    bfs_iteration(acc)
  end

  def bfs_iteration(acc) do
    if Enum.empty?(acc.queue) do
      nil
    else
      {current_node, queue} = List.pop_at(acc.queue, 0)
      acc = %{acc | queue: queue}

      if current_node.final do
        IO.puts("yey!")
        backtrack(%{path: [current_node], current_node: current_node, parents: acc.parents})
      else
        acc =
          if !MapSet.member?(acc.visited, current_node.id) do
            acc = %{acc | visited: MapSet.put(acc.visited, current_node.id)}

            row_length = 163

            x =
              Enum.join(
                Enum.map(acc.nodes, fn node ->
                  x =
                    if node.final do
                      "*"
                    else
                      if node.start do
                        "@"
                      else
                        if MapSet.member?(acc.visited, node.id) do
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

        bfs_iteration(acc)
      end
    end
  end

  defp backtrack(acc) do
    if Map.get(acc.parents, acc.current_node) != nil do
      current_node = Map.get(acc.parents, acc.current_node)

      acc = %{
        acc
        | current_node: current_node,
          path: [current_node | acc.path]
      }

      backtrack(acc)
    else
      acc.path
    end
  end
end

Main.main()
