module Brainfuck
  module Interpreter
    class Output
      def self.default
        new([])
      end

      def initialize(chars)
        raise TypeError unless chars.is_a?(Array)
        raise TypeError unless chars.all? { |char| char.is_a?(Byte) }

        @chars = chars
        freeze
      end

      attr_reader :chars

      def to_s
        "'#{@chars.map(&:to_i).map(&:chr).join}'"
      end

      def write(byte)
        self.class.new(@chars.dup + [byte])
      end
    end
  end
end
