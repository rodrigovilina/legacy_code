module Brainfuck
  class ConcreteNode
    def ==(other)
      other.class == self.class
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

      def abstract
        AbstractNode::Program.new(nodes.map(&:abstract))
      end
    end

    class Left < self
      def tokens
        [Token::Left.new]
      end

      def abstract
        AbstractNode::Left.new
      end
    end

    class Right < self
      def tokens
        [Token::Right.new]
      end

      def abstract
        AbstractNode::Right.new
      end
    end

    class Dot < self
      def tokens
        [Token::Dot.new]
      end

      def abstract
        AbstractNode::Dot.new
      end
    end

    class Comma < self
      def tokens
        [Token::Comma.new]
      end

      def abstract
        AbstractNode::Comma.new
      end
    end

    class Plus < self
      def tokens
        [Token::Plus.new]
      end

      def abstract
        AbstractNode::Plus.new
      end
    end

    class Minus < self
      def tokens
        [Tokens::Minus.new]
      end

      def abstract
        AbstractNode::Minus.new
      end
    end

    class Loop < self
      def initialize(tokens)
        raise 'First token is not Loop' unless tokens.first == Token::Loop.new
        raise 'Last token is not End'   unless tokens.last == Token::End.new

        @tokens = tokens
        freeze
      end

      attr_reader :tokens

      def abstract
        AbstractNode::Loop.new(tokens[1..-2])
      end
    end

    class Comment < self
      def initialize(token)
        @tokens = [token]
        freeze
      end

      attr_reader :tokens

      def abstract
        nil
      end
    end
  end
end
