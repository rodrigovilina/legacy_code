module Brainfuck
  module Interpreter
    module Ops
      class Comma
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
          memory = machine.get_input
          instruction_pointer = machine.instruction_pointer.advance
          machine.with(instruction_pointer:, memory:)
        end
      end
    end
  end
end
