module Brainfuck
  module Interpreter
    module Ops
      class Minus
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
          memory = machine.memory_sub
          instruction_pointer = machine.instruction_pointer.advance
          machine.with(instruction_pointer:, memory:)
        end
      end
    end
  end
end
