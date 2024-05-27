# frozen_string_literal: true

module Day3
  Input   = Struct.new(:input)
  EXAMPLE = Input.new(File.read('examples/example3.txt'))
  INPUT   = Input.new(File.read('inputs/input3.txt'))

  def self.problem_a(input)
    a = input.input.split.first.size.times.map do |index|
      input.input.split.map { |line| line[index] }.tally.max_by { |_k, v| v }.first
    end.join.to_i(2)
    b = input.input.split.first.size.times.map do |index|
      input.input.split.map { |line| line[index] }.tally.min_by { |_k, v| v }.first
    end.join.to_i(2)
    a * b
  end

  def self.problem_b(input)
    # # tallied_bites = input.input.split.first.size.times.map { |index| input.input.split.map { |line| line[index] }.tally }
    # first.size.times.map { |index| input.input.split.map { |line| line[index] }.tally }
    # input.input.split.first.size.times.reduce(input.input.split) do |string, index|
    #   counts = input.input.split.map { |line| line[index] }.tally
    #   case (counts['0'] <=> counts['1'])
    #   when (-1) then '0'
    #   when 0  then '0'
    #   when 1  then '1'
    #   end.then { |char| string + char }
    # end.tap { puts _1 }.to_i(2)
    # b = tallied_bites.reduce('') do |string, counts|
    #   case (counts['1'] <=> counts['0'])
    #   when (-1) then '0'
    #   when 0  then '1'
    #   when 1  then '1'
    #   end.then { |char| string + char }
    # end.to_i(2)

    # a * b
    size = input.input.split.first.size
    initial_accumulator = input.input.split
    ox = size.times.reduce(initial_accumulator) do |acc, index|
      raise 'nope' if acc.size == 0
      break acc    if acc.size == 1

      count_for_index = acc.map { |line| line[index] }.tally
      most_common_byte =
        case (count_for_index['0'] <=> count_for_index['1'])
        when (-1) then '1'
        when 0  then '1'
        when 1  then '0'
        end
     acc.reject { |line| line[index] == most_common_byte }
    end.first.to_i(2)
    gam = size.times.reduce(initial_accumulator) do |acc, index|
      raise 'nope' if acc.size == 0
      break acc    if acc.size == 1

      count_for_index = acc.map { |line| line[index] }.tally
      least_common_byte =
        case (count_for_index['1'] <=> count_for_index['0'])
        when (-1) then '1'
        when 0  then '0'
        when 1  then '0'
        end
     acc.reject { |line| line[index] == least_common_byte }
    end.first.to_i(2)

    ox * gam
  end

end
