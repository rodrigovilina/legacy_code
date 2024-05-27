# typed: strict
# frozen_string_literal: true

module Day8
  Input   = Struct.new(:input)
  EXAMPLE = Input.new(File.read('examples/example8.txt'))
  INPUT   = Input.new(File.read('inputs/input8.txt'))

  class << self
    def problem_a(input)
      input.input.split("\n").map do |line|
        line.split('|').last.split.filter_map { |i| i if [2,3,4,7].include? i.chars.size }.count
      end.sum
    end

    def problem_b(input)
    end

  end
end
