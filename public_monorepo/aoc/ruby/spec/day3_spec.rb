# frozen_string_literal: true

require_relative '../lib/day3'

describe Day3 do
  specify do
    expect(described_class.problem_a(described_class::EXAMPLE)).to be(198)
    expect(described_class.problem_a(described_class::INPUT)).to   be(4006064)
    expect(described_class.problem_b(described_class::EXAMPLE)).to be(230)
    expect(described_class.problem_b(described_class::INPUT)).to   be(5941884)
  end
end
