module Brainfuck
  module Interpreter
    class Machine
      def self.default
        new(
          data_pointer: DataPointer.default,
          instruction_pointer: InstructionPointer.default,
          memory: Memory.default,
          input: Input.default,
          output: Output.default
        )
      end

      def self.from_string(string)
        input = Input.new(string)
        pairs = Balancer.(input.to_s).pairs
        new(
          data_pointer: DataPointer.default,
          instruction_pointer: InstructionPointer.default,
          memory: Memory.default,
          input:,
          output: Output.default,
          pairs:
        )
      end

      def initialize(data_pointer:, instruction_pointer:, memory:, input:, output:, pairs: {})
        raise TypeError unless data_pointer.is_a?(DataPointer)
        raise TypeError unless instruction_pointer.is_a?(InstructionPointer)
        raise TypeError unless memory.is_a?(Memory)
        raise TypeError unless input.is_a?(Input)
        raise TypeError unless output.is_a?(Output)

        @data_pointer = data_pointer
        @instruction_pointer = instruction_pointer
        @memory = memory
        @input = input
        @output = output
        @pairs = pairs
        freeze
      end

      attr_reader :data_pointer, :instruction_pointer, :memory, :input, :output, :pairs

      def to_s
        <<~STR
          ===
          data_pointer: #{data_pointer}
          instruction_pointer: #{instruction_pointer}
          memory: #{memory}
          input: #{input}
          output: #{output.to_s}
          pairs: #{pairs}
        STR
      end

      def step
        instruction = input.chars[instruction_pointer.to_i]

        case instruction
        in "+" then Ops::Plus.call(self)
        in '-' then Ops::Minus.call(self)
        in '<' then Ops::Left.call(self)
        in '>' then Ops::Right.call(self)
        in '.' then Ops::Dot.call(self)
        in ',' then Ops::Comma.call(self)
        in '[' then Ops::LBracket.call(self)
        in ']' then Ops::RBracket.call(self)
        end
      end

      def memory_add
        memory.add(data_pointer.to_i)
      end

      def memory_sub
        memory.sub(data_pointer.to_i)
      end

      def get_input
        value = Byte.new(STDIN.getch.ord)
        memory.write(value, data_pointer.to_i)
      end

      def write_io
        output.write(current_value)
      end

      def current_value
        memory.cells[data_pointer.to_i]
      end

      def current_value_zero?
        current_value.zero?
      end

      def with(**opts)
        new_attrs = { data_pointer:, instruction_pointer:, memory:, input:, output:, pairs:}.merge(opts)
        self.class.new(**new_attrs)
      end

      def halted?
        instruction_pointer.to_i >= input.chars.length
      end

      def move_right_of_pair
        InstructionPointer.new(pairs.fetch(instruction_pointer.to_i) + 1)
      end

      def move_right_of_pair_inverse
        InstructionPointer.new(pairs.invert.fetch(instruction_pointer.to_i) + 1)
      end
    end
  end
end
