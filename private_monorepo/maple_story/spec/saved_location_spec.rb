# frozen_string_literal: true

RSpec.describe SavedLocation do
  specify do
    expect(build(:saved_location)).to be_a(described_class)
  end
end
