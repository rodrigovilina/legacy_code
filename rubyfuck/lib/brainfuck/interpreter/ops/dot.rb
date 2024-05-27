module Brainfuck
  module Interpreter
    module Ops
      class Dot
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
          output = machine.write_io
          instruction_pointer = machine.instruction_pointer.advance
          machine.with(instruction_pointer:, output:)
        end
      end
    end
  end
end
