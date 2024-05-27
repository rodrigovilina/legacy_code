module Brainfuck
  module Interpreter
    class Memory
      def self.default
        new(30000.times.map { Byte.new(0) })
      end

      def initialize(cells)
        raise TypeError unless cells.is_a?(Array)
        raise TypeError unless cells.all? { |cell| cell.is_a?(Byte) }

        @cells = cells
        freeze
      end

      attr_reader :cells

      def to_s
        return 'empty' if @cells.all? { |cell| cell.to_i.zero? }

        @cells.each_with_index.filter_map do |cell, index|
          if cell.to_i > 0
            "\n  #{index}: #{cell}"
          end
        end.join
      end

      def add(index)
        cells = @cells.dup
        cells[index] = Byte.new(cells[index].to_i + 1)
        Memory.new(cells)
      end

      def sub(index)
        cells = @cells.dup
        cells[index] = cells[index].wrap_minus_one
        Memory.new(cells)
      end

      def write(to_i, index)
        cells = @cells.dup
        cells[index] = to_i
        Memory.new(cells)
      end
    end
  end
end
