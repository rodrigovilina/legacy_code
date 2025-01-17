# frozen_string_literal: true

require 'simplecov'
SimpleCov.start

require 'wood/lexer'

RSpec.configure do |config|
  config.example_status_persistence_file_path = '.rspec_status'
  config.disable_monkey_patching!
  config.expect_with(:rspec) { |c| c.syntax = :expect }
end
