# frozen_string_literal: true

require 'awesome_print'
require 'matrix'

module Day4
  Input   = Struct.new(:input)
  EXAMPLE = Input.new(File.read('examples/example4.txt'))
  INPUT   = Input.new(File.read('inputs/input4.txt'))
  Bingo   = Struct.new(:numbers, :marks)

  class << self
    def problem_a(input)
      catch(:bingo) do
        numbers(input).reduce(bingos(input)) do |bingos, number|
          bingos.map do |bingo|
            marked_bingo(bingo, number).tap do |bingo|
              throw :bingo, number * unmarked_sum(bingo) if won_bingo? bingo
            end
          end
        end
      end
    end

    def problem_b(input)
      numbers(input).reduce(bingos(input)) do |bingos, number|
        marked_bingos = bingos.map { |bingo| marked_bingo(bingo, number) }
        filtered_bingos = marked_bingos.reject { |bingo| won_bingo? bingo }
        if marked_bingos.size == 1 && filtered_bingos.size == 0
          break number * unmarked_sum(marked_bingos.first)
        else
          filtered_bingos
        end
      end
    end

    def unmarked_sum(bingo)
      bingo.numbers.zip(bingo.marks).reduce(0) do |sum, (number, mark)|
        sum + (mark ? 0 : number)
      end
    end

    def numbers(input)
      input.input.lines.first.chomp.split(',').map(&:to_i)
    end

    def bingos(input)
      input.input.split("\n")[2..].join(' ').split.map(&:to_i).each_slice(25).map { |slice| Bingo.new(slice, Array.new(25, false)) }
    end

    def marked_bingo(bingo, number)
      return bingo unless index = bingo.numbers.find_index(number)

      Bingo.new(bingo.numbers, bingo.marks.tap { |marks| marks[index] = true })
    end

    def won_bingo?(bingo)
      Matrix[*bingo.marks.each_slice(5).to_a].then do |matrix|
        matrix.row_vectors.any?(&:all?) || matrix.column_vectors.any?(&:all?)
      end
    end
  end
end
