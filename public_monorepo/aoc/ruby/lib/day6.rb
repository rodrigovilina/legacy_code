# frozen_string_literal: true

module Day6
  Input   = Struct.new(:input)
  EXAMPLE = Input.new(File.read('examples/example6.txt'))
  INPUT   = Input.new(File.read('inputs/input6.txt'))

  class << self
    def problem_a(input)
      80.times.reduce(input.input.split(',').map(&:to_i)) do |ac, _|
        ->(a) { a.flat_map { |i| ->(j) { j.zero? ? [6, 8] : [j - 1] }.call(i) } }.call(ac)
      end.count
    end

    def problem_b(input)
      256.times.reduce(Hash.new(0).merge(input.input.split(',').map(&:to_i).tally)) do |ac, _i|
        Hash.new(0).merge({ 0 => ac[1], 1 => ac[2], 2 => ac[3], 3 => ac[4], 4 => ac[5], 5 => ac[6], 6 => ac[0] + ac[7],
                            7 => ac[8], 8 => ac[0] })
      end.values.sum
    end
  end
end
