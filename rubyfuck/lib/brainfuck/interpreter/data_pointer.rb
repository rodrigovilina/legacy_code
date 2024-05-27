module Brainfuck
  module Interpreter
    class DataPointer
      def self.default
        new(0)
      end

      def initialize(to_i)
        raise TypeError unless to_i.is_a?(Integer)
        raise ArgumentError unless to_i.between?(0, 29_999)

        @to_i = to_i
        freeze
      end

      attr_reader :to_i

      def to_s
        to_i.to_s
      end

      def left
        DataPointer.new((to_i - 1).modulo 30000)
      end

      def right
        DataPointer.new((to_i + 1).modulo 30000)
      end

      def ==(other)
        other.class == self.class && to_i == other.to_i
      end
    end
  end
end
