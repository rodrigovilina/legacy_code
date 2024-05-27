# frozen_string_literal: true

RSpec.describe SummonMovementType do
  describe '#value' do
    it 'returns 0 for STATIONARY' do
      expect(SummonMovementType::STATIONARY.value).to eq(0)
    end

    it 'returns 1 for FOLLOW' do
      expect(SummonMovementType::FOLLOW.value).to eq(1)
    end

    it 'returns 3 for CIRCLE_FOLLOW' do
      expect(SummonMovementType::CIRCLE_FOLLOW.value).to eq(3)
    end
  end

  describe '.from_i' do
    it 'returns STATIONARY for 0' do
      expect(described_class.from_i(0)).to eq(SummonMovementType::STATIONARY)
    end

    it 'returns FOLLOW for 1' do
      expect(described_class.from_i(1)).to eq(SummonMovementType::FOLLOW)
    end

    it 'returns CIRCLE_FOLLOW for 3' do
      expect(described_class.from_i(3)).to eq(SummonMovementType::CIRCLE_FOLLOW)
    end
  end
end
