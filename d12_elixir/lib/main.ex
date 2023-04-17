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

  defmodule Graph do
    @type t :: %Graph{
            edges: [{integer, integer}],
            nodes: [Node.t()]
          }
    defstruct [:nodes, :edges]

    @spec new([Node.t()], {integer, integer}) :: Graph.t()
    def new(nodes, edges) do
      edges = Enum.filter(edges, &(&1 != nil))
      %Graph{nodes: nodes, edges: edges}
    end

    @spec add_edge(Node.t(), Node.t()) :: {Node.t(), Node.t()}
    def add_edge(node1, node2) do
      node1 = add_neighbor(node1, node2.id)
      node2 = add_neighbor(node2, node1.id)
      {node1, node2}
    end

    defp add_neighbor(node, neighbor_id) do
      neighbors = Enum.filter(node.neighbors, &(&1 == neighbor_id))
      %Node{node | neighbors: [neighbor_id | neighbors]}
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
        id = col + row * String.length(line)

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

    nodes =
      Enum.reduce(nodes, nodes, fn node, nodes ->
        ids =
          [row_length + node.id, node.id - row_length, node.id - 1, node.id + 1]
          |> Enum.filter(&(&1 >= 0 and &1 < nodes_length))
          |> Enum.filter(&(node.height + 1 >= Enum.at(nodes, &1).height))

        List.replace_at(nodes, node.id, %Node{node | neighbors: ids})
      end)

    IO.inspect(lines)
    IO.inspect(length(bfs(Enum.find(nodes, & &1.start), nodes)))
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
    {current_node, queue} = List.pop_at(acc.queue, 0)
    acc = %{acc | queue: queue}

    if current_node.final do
      backtrack(%{path: [current_node], current_node: current_node, parents: acc.parents})
    else
      acc = %{acc | visited: MapSet.put(acc.visited, current_node.id)}

      acc =
        Enum.reduce(current_node.neighbors, acc, fn neighbor_id, acc ->
          if MapSet.member?(acc.visited, neighbor_id) do
            acc
          else
            neighbor = Enum.at(acc.nodes, neighbor_id)

            %{
              acc
              | queue: acc.queue ++ [neighbor],
                parents: Map.put(acc.parents, neighbor, current_node)
            }
          end
        end)

      if Enum.empty?(acc.queue) do
        nil
      else
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
