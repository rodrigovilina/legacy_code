module Brainfuck
  module Interpreter
    class Balancer
      def self.call(input)
        instance = new(input)

        until instance.done? do
          instance = instance.step
        end

        raise IndexError, 'Unmatched open brackets' if instance.send(:stack).any?

        instance
      end

      def done?
        index == input.length
      end

      def step
        case input[index]
        when '[' then open_bracket
        when ']' then close_bracket
        else next_balancer
        end
      end

      attr_reader :pairs

      private

      def initialize(input, index = 0, stack = [], pairs = {})
        @input = input
        @index = index
        @stack = stack
        @pairs = pairs
        freeze
      end

      attr_reader :input, :index, :stack

      def next_balancer
        Balancer.new(
          input,
          index + 1,
          stack,
          pairs
        )
      end

      def open_bracket
        Balancer.new(
          input,
          index + 1,
          stack.dup + [index],
          pairs
        )
      end

      def close_bracket
        Balancer.new(
          input,
          index + 1,
          stack[0..-2].dup,
          pairs.dup.merge(stack.fetch(-1) => index)
        )
      rescue IndexError
        raise IndexError, 'Unmatched close brackets'
      end
    end
  end
end
