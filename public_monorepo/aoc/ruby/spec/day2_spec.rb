# frozen_string_literal: true

require_relative '../lib/day2'

describe Day2 do
  specify do
    expect(described_class.problem_a(described_class::EXAMPLE)).to be(150)
    expect(described_class.problem_a(described_class::INPUT)).to   be(1_868_935)
    expect(described_class.problem_b(described_class::EXAMPLE)).to be(900)
    expect(described_class.problem_b(described_class::INPUT)).to   be(1_965_970_888)
  end
end
