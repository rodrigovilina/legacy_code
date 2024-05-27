module Brainfuck
  module Interpreter
    module Ops
      class LBracket
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
            instruction_pointer = machine.move_right_of_pair
          else
            instruction_pointer = machine.instruction_pointer.advance
          end
          machine.with(instruction_pointer: instruction_pointer)
        end
      end
    end
  end
end
