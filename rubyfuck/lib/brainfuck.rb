# frozen_string_literal: true

require 'zeitwerk'
loader = Zeitwerk::Loader.for_gem
loader.setup # ready!

module Brainfuck
  class Error < StandardError; end

  def self.call
    require 'awesome_print'
    require 'io/console'
    machine = Machine.from_string(<<~BF)
      +[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+.
    BF
    until machine.halted?
      puts machine
      machine = machine.step
    end
    puts '======================================================'
    puts machine
  end
end

loader.eager_load
