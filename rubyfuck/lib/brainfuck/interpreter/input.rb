module Brainfuck
  module Interpreter
    class Input
      def self.default
        new("")
      end

      def initialize(string)
        raise TypeError unless string.is_a?(String)
        chars = string.chars.filter { |char| %w{< > + - . , [ ]}.include? char }

        @chars = chars
        freeze
      end

      attr_reader :chars

      def to_s
        chars.join
      end
    end
  end
end
