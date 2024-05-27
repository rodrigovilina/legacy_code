module Brainfuck
  module Interpreter
    module Ops
      class Right
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
          data_pointer = machine.data_pointer.right
          instruction_pointer = machine.instruction_pointer.advance
          machine.with(instruction_pointer:, data_pointer:)
        end
      end
    end
  end
end
