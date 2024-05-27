# frozen_string_literal: true

module Day5
  Input   = Struct.new(:input)
  Point   = Struct.new(:x, :y)
  Line    = Struct.new(:a, :b)
  EXAMPLE = Input.new(File.read('examples/example5.txt'))
  INPUT   = Input.new(File.read('inputs/input5.txt'))

  def self.method_added(name)
    puts "*********************************************** #{name}"
  end

  class << self
    def problem_a(input)
      input.input.split("\n")
        .map(&method(:line))
        .filter_map(&method(:line_to_points))
        .flatten.tally.count { |_, v| v > 1 }
    end

    def problem_b(input)
      input.input.split("\n")
        .map(&method(:line))
        .map(&method(:line_to_points_b))
        .flatten.tally.count { |_, v| v > 1 }
    end

    def line(line)
      Line::new *line.split('->').map(&method(:point))
    end

    def point(point)
      Point::new *point.split(',').map(&:to_i)
    end

    def line_direction(line)
      case [line.a.x <=> line.b.x, line.a.y <=> line.b.y]
      when [0, -1] then :up
      when [0, 1]  then :down
      when [-1, 0] then :right
      when [1, 0]  then :left
      end
    end

    def line_to_points(line)
      case line_direction(line)
      when :down, :up    then Range::new(*[line.b.y, line.a.y].sort).map { |y| Point::new(line.a.x, y) }
      when :left, :right then Range::new(*[line.b.x, line.a.x].sort).map { |x| Point::new(x, line.a.y) }
      end
    end

    def line_to_points_b(line)
      zip(range(line.a.x, line.b.x), range(line.a.y, line.b.y)).map { |x, y| Point::new(x, y) }
    end

    def range(a, b)
      case
      when a == b then [a]
      when a > b  then a.downto(b).to_a
      when a < b  then a.upto(b).to_a
      end
    end

    def zip(a, b)
      case
      when a.size == b.size then a.zip(b)
      when a.size == 1 then a.cycle(b.size).zip(b)
      when b.size == 1 then a.zip(b.cycle(a.size))
      else raise "sizes are different #{{a:, b:}}"
      end
    end
  end
end
