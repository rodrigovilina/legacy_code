module Brainfuck
  class AbstractNode
    def ==(other)
      self.class == other.class
    end

    class Program < self
      def initialize(nodes)
        @nodes = nodes
        freeze
      end

      attr_reader :nodes

      def ==(other)
        self.class == other.class && @nodes == other.nodes
      end

      def inspect
        nodes = self.nodes.map(&:inspect).join(' ')
        "s(prog #{nodes})"
      end
    end

    class Left < self
      def tokens
        [Token::Left.new]
      end

      def inspect = 's(<)'
    end

    class Right < self
      def tokens
        [Token::Right.new]
      end

      def inspect = 's(>)'
    end

    class Dot < self
      def tokens
        [Token::Dot.new]
      end

      def inspect = 's(.)'
    end

    class Comma < self
      def tokens
        [Token::Comma.new]
      end

      def inspect = 's(,)'
    end

    class Plus < self
      def tokens
        [Token::Plus.new]
      end

      def inspect = 's(+)'
    end

    class Minus < self
      def tokens
        [Tokens::Minus.new]
      end

      def inspect = 's(-)'
    end

    class Loop < self
      def initialize(tokens)
        @tokens = tokens
        freeze
      end

      attr_reader :tokens
    end
  end
end
