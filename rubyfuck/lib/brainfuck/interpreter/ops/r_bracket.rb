module Brainfuck
  module Interpreter
    module Ops
      class RBracket
        def self.call(machine)
          new(machine).__send__(:perform)
        end

        private

        def initialize(machine)
          @machine = machine
          freeze
        end

        attr_reader :machine

        def perform
          if machine.current_value_zero?
            instruction_pointer = machine.instruction_pointer.advance
          else
            instruction_pointer = machine.move_right_of_pair_inverse
          end
          machine.with(instruction_pointer: instruction_pointer)
        end
      end
    end
  end
end
