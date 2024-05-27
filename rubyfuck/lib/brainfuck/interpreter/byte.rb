module Brainfuck
  module Interpreter
    class Byte
      def self.default
        new(0)
      end

      def initialize(to_i)
        raise TypeError unless to_i.is_a?(Integer)
        raise ArgumentError unless to_i.between?(0, 255)

        @to_i = to_i
        freeze
      end

      attr_reader :to_i

      def to_s
        to_i.to_s
      end

      def zero?
        to_i.zero?
      end

      def wrap_minus_one
        Byte.new((to_i - 1).modulo 256)
      end
    end
  end
end
