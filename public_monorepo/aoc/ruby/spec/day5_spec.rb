# frozen_string_literal: true

require_relative '../lib/day5'

describe Day5 do
  specify(aggregate_failures: true) do
    expect(described_class::problem_a(described_class::EXAMPLE)).to be(5)
    expect(described_class::problem_a(described_class::INPUT)).to   be(6283)
    expect(described_class::problem_b(described_class::EXAMPLE)).to be(12)
    expect(described_class::problem_b(described_class::INPUT)).to   be(18864)
  end
end
