# frozen_string_literal: true

module Day7
  Input   = Struct.new(:input)
  EXAMPLE = Input.new(File.read('examples/example7.txt'))
  INPUT   = Input.new(File.read('inputs/input7.txt'))

  class << self
    def problem_a(input)
      input.input.split(',').map(&:to_i).then do |positions|
        Range::new(*positions.minmax).map do |i|
          positions.map do |j|
            (j - i).abs
          end.sum
        end.min
      end
    end

    def problem_b(input)
      input.input.split(',').map(&:to_i).then do |positions|
        Range::new(*positions.minmax).map do |i|
          positions.map do |j|
            (1..(j - i).abs).sum
          end.sum
        end.min
      end
    end
  end
end
