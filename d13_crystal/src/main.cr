struct Packet
  property value : Array(Packet) | Int32
  property is_divider : Bool

  def initialize(@value : Array(Packet) | Int32, @is_divider : Bool = false)
  end
end

alias Pair = NamedTuple(left: Packet, right: Packet)

def read_file(argv : Array(String)) : String?
  if argv.size > 0
    file_path = argv[0]
    if File.exists?(file_path)
      content = File.read(file_path)
      return content
    else
      puts "File '#{file_path}' does not exist."
    end
  else
    puts "Usage: ./main <input-path>"
    puts "Or: crystal run src/main.cr -- <input-path>"
  end
end

def parse_value(line : String, is_divider : Bool = false) : Packet
  if line.starts_with?("[") && line.ends_with?("]")
    parse_list(line, is_divider)
  else
    Packet.new(line.to_i)
  end
end

# It is possible to use JSON parsing or other techniques,
# but I chose to avoid dependencies and wanted to practice parsing nested arrays.
def parse_list(line : String, is_divider : Bool = false) : Packet
  contents = line[1..-2]
  result = [] of Packet
  element = ""
  nested_level = 0

  contents.each_char.with_index do |char, index|
    case char
    when '['
      nested_level += 1
      element += char
    when ']'
      nested_level -= 1
      element += char
      if nested_level.zero?
        result << parse_value(element)
        element = ""
      end
    when ','
      if nested_level.zero?
        if element != ""
          result << parse_value(element)
          element = ""
        end
      else
        element += char
      end
    else
      element += char
      result << parse_value(element) if index == contents.size - 1
    end
  end
  Packet.new(result, is_divider)
end

def parse_pairs(input : String) : Array(Pair)
  pairs = input.split("\n\n")
  pairs.map do |pair|
    left, right = pair.split("\n")
    {left: parse_value(left), right: parse_value(right)}
  end
end

def parse_packets(input : String) : Array(Packet)
  input = input.gsub(/\n\s*\n/, "\n")
  res = [parse_value("[[2]]", true), parse_value("[[6]]", true)]
  input.each_line do |line|
    res << parse_value(line)
  end
  res
end

def compare(left : Packet, right : Packet)
  left_v = left.value
  right_v = right.value
  case {left_v, right_v}
  when {Int32, Int32}
    return right_v <=> left_v
  when {Array(Packet), Array(Packet)}
    left_v.each_with_index do |left_value, index|
      if index < right_v.size
        res = compare(left_value, right_v[index])
        return res if res != 0
      end
    end
    return right_v.size <=> left_v.size
  when {Int32, _}
    return compare(Packet.new([left]), (right))
  when {_, Int32}
    return compare(left, Packet.new([right]))
  else
    return 0
  end
end

module Main
  content = read_file(ARGV)
  if content
    pairs = parse_pairs(content)
    part1 = 0
    pairs.each_with_index do |pair, i|
      if compare(pair[:left], pair[:right]) > 0
        part1 += i + 1
      end
    end
    puts "Part 1: #{part1}"

    packets = parse_packets(content)
    sorted_packets = packets.sort { |a, b| compare(b, a) }
    part2 = 1
    sorted_packets.each_with_index do |value, i|
      if value.is_divider
        part2 *= i + 1
      end
    end
    puts "Part 2: #{part2}"
  end
end
