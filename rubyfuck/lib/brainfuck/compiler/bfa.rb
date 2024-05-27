# frozen_string_literal: true

module Brainfuck
  module Compiler
    class Bfa
    end

    class << Bfa
      def call(program)
        program.nodes.map do |node|
          case node
          when AbstractNode::Plus  then compile_plus
          when AbstractNode::Minus then compile_minus
          when AbstractNode::Left  then compile_left
          when AbstractNode::Right then compile_right
          when AbstractNode::Comma then compile_comma
          when AbstractNode::Dot   then compile_dot
          end
        end.join("\n") + "\n"
      end

      def compile_left
        'LEFT'
      end

      def compile_right
        'RIGHT'
      end

      def compile_plus
        'INC'
      end

      def compile_minus
        'DEC'
      end

      def compile_dot
        'WRITE'
      end

      def compile_comma
        'READ'
      end

      def compile_loop

      end
    end
  end
end
