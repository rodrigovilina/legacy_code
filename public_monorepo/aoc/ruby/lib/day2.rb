# frozen_string_literal: true

module Day2
  Input   = Struct.new(:input)
  EXAMPLE = Input.new(File.read('examples/example2.txt'))
  INPUT   = Input.new(File.read('inputs/input2.txt'))

  def self.problem_a(input)
    input.input.split.each_slice(2)
         .group_by { |name, _value| name }
         .transform_values { |arr| arr.sum { |_name, value| value.to_i } }
         .then { |hash| (hash['down'] - hash['up']) * hash['forward'] }
  end

  def self.problem_b(input)
    input.input.split.each_slice(2).map { |name, value| { name: name, value: value.to_i } }
         .each_with_object({ aim: 0, hor: 0, depth: 0 }) do |inst, pos|
      case inst[:name]
      when 'down'
        pos[:aim] += inst[:value]
      when 'up'
        pos[:aim] -= inst[:value]
      when 'forward'
        pos[:hor] += inst[:value]
        pos[:depth] += pos[:aim] * inst[:value]
      end
    end.then { |pos| pos[:depth] * pos[:hor] }
  end
end
