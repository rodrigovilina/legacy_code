# frozen_string_literal: true

require_relative '../lib/day8'

describe Day8 do
  specify(aggregate_failures: true) do
    expect(described_class.problem_a(described_class::EXAMPLE)).to be(26)
    expect(described_class.problem_a(described_class::INPUT)).to   be(355)
    expect(described_class.problem_b(described_class::EXAMPLE)).to be(168)
    expect(described_class.problem_b(described_class::INPUT)).to   be(104149091)
  end
end
