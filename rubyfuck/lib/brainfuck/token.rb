# frozen_string_literal: true

module Brainfuck
  class Token
    def ==(other)
      self.class == other.class
    end

    def inspect
      "t(#{lexeme})"
    end

    def self.token(lexeme)
      Class.new(self) do
        define_method :lexeme do
          lexeme
        end
      end
    end

    Left  = token('<')
    Right = token('>')
    Plus  = token('+')
    Minus = token('-')
    Comma = token(',')
    Dot   = token('.')
    Loop  = token('[')
    End   = token(']')

    class Other < self
      def initialize(lexeme)
        @lexeme = lexeme
      end
      attr_reader :lexeme
    end
  end
end
