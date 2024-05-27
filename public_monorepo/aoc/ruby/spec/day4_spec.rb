# frozen_string_literal: true

require_relative '../lib/day4'

describe Day4 do
  specify(aggregate_failures: true) do
    expect(described_class::problem_a(described_class::EXAMPLE)).to be(4512)
    expect(described_class::problem_a(described_class::INPUT)).to   be(39902)
    expect(described_class::problem_b(described_class::EXAMPLE)).to be(1924)
    expect(described_class::problem_b(described_class::INPUT)).to   be(26936)
  end
end
