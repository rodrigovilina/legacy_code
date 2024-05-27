# frozen_string_literal: true

require_relative '../lib/day6'

describe Day6 do
  specify(aggregate_failures: true) do
    expect(described_class.problem_a(described_class::EXAMPLE)).to be(5934)
    expect(described_class.problem_a(described_class::INPUT)).to   be(386_640)
    expect(described_class.problem_b(described_class::EXAMPLE)).to be(26984457539)
    expect(described_class.problem_b(described_class::INPUT)).to   be(1733403626279)
  end
end
