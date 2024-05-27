# typed: false
# frozen_string_literal: true

RSpec.describe MaplePacketCreator do
  describe '.remove_map_effect' do
    specify do
      expect(described_class.remove_map_effect).to eq([])
    end
  end

  describe '.start_map_effect' do
    specify do
      expect(described_class.start_map_effect('MyString', 1, true)).to eq([])
    end
  end
end
