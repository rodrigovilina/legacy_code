module Brainfuck
  module Interpreter
    class InstructionPointer
      def self.default
        new(0)
      end

      def initialize(to_i)
        raise TypeError unless to_i.is_a?(Integer)
        raise ArgumentError unless to_i >= 0

        @to_i = to_i
        freeze
      end

      attr_reader :to_i

      def to_s
        to_i.to_s
      end

      def advance
        InstructionPointer.new(to_i + 1)
      end

      def ==(other)
        other.class == self.class && to_i == other.to_i
      end
    end
  end
end
