# typed: false
# frozen_string_literal: true

RSpec.describe FieldLimit do
  describe '.from_i' do
    specify do
      expect(described_class.from_i(0x01)).to eq(described_class::JUMP)
    end
  end

  describe '#value' do
    specify do
      expect(described_class::JUMP.value).to eq(0x01)
    end
  end

  describe '#check' do
    it 'returns true if the bit flag is set' do
      expect(described_class::JUMP.check(0b1010)).to be(false)
    end

    it 'returns false if the bit flag is not set' do
      expect(described_class::JUMP.check(0b0101)).to be(true)
    end
  end
end
