struct ParsedValue
  property value : Array(ParsedValue) | Int32

  def initialize(@value : Array(ParsedValue) | Int32)
  end
end

alias Pair = NamedTuple(left: ParsedValue, right: ParsedValue)

module Main
  content = read_file(ARGV)
  if (content)
    pairs = parse_pairs(content)
    index = 0
    pairs.each_with_index do |pair, i|
      if compare(pair[:left], pair[:right]) > 0
        index += i + 1
      end
    end
    puts "Part 1: #{index}"
  end
end

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

def parse_pairs(input : String) : Array(Pair)
  pairs = input.split("\n\n")
  pairs.map do |pair|
    left, right = pair.split("\n")
    {left: parse_line(left), right: parse_line(right)}
  end
end

def parse_line(line : String) : ParsedValue
  if line.starts_with?("[") && line.ends_with?("]")
    parse_list(line)
  else
    ParsedValue.new(line.to_i)
  end
end

def parse_list(line : String) : ParsedValue
  contents = line[1..-2]
  result = [] of ParsedValue
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
        result << parse_line(element)
        element = ""
      end
    when ','
      if nested_level.zero?
        if element != ""
          result << parse_line(element)
          element = ""
        end
      else
        element += char
      end
    else
      element += char
      if index == contents.size - 1
        result << parse_line(element)
      end
    end
  end

  return ParsedValue.new(result)
end

def compare(left : ParsedValue, right : ParsedValue)
  left_v = left.value
  right_v = right.value
  if left_v.is_a?(Int32) && right_v.is_a?(Int32)
    return right_v <=> left_v
  end

  if left_v.is_a?(Array(ParsedValue)) && right_v.is_a?(Array(ParsedValue))
    left_v.each_with_index do |left_value, index|
      if index < right_v.size
        res = compare(left_value, right_v[index])
        if res != 0
          return res
        end
      end
    end

    return right_v.size <=> left_v.size
  end

  if left_v.is_a?(Int32)
    return compare(ParsedValue.new([left]), (right))
  end

  if right_v.is_a?(Int32)
    return compare(left, ParsedValue.new([right]))
  end

  return 0
end
