# typed: false
# frozen_string_literal: true

RSpec.describe Point do
  let(:origin) { build(:point, x: 0, y: 0) }

  describe '#move' do
    specify do
      expect(origin.move(1, 1)).to eq(build(:point, x: 1, y: 1))
    end
  end

  describe '#==' do
    specify do
      expect(origin).not_to eq(Object.new)
    end
  end
end
