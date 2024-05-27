module Brainfuck
  class Parser
    def self.call(tokens)
      nodes = []
      tokens = tokens

      until tokens.empty? do
        node, tokens = parse_single(tokens)
        nodes << node
      end

      ConcreteNode::Program.new(nodes)
    end

    def self.parse_single(tokens)
      first_token = tokens.first
      rest = tokens[1..]
      node = case tokens.first
      when Token::Left  then ConcreteNode::Left.new
      when Token::Right then ConcreteNode::Right.new
      when Token::Plus  then ConcreteNode::Plus.new
      when Token::Minus then ConcreteNode::Minus.new
      when Token::Dot   then ConcreteNode::Dot.new
      when Token::Comma then ConcreteNode::Comma.new
      when Token::Other then ConcreteNode::Comment.new(tokens.first.lexeme)
      when Token::Loop
        count = 0
        loop_tokens = tokens.take_while do
          case _1
          when Token::Loop then count += 1
          when Token::End  then count -= 1
          end

          !count.zero?
        end
        loop_tokens = tokens[0..loop_tokens.size]
        rest = tokens[loop_tokens.size..]
        ConcreteNode::Loop.new(loop_tokens)
      end

      return node, rest
    end
  end
end
