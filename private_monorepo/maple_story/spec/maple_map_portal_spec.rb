# frozen_string_literal: true

RSpec.describe MapleMapPortal do
  describe '.new' do
    specify do
      instance = described_class.new
      expect(instance.type).to eq(MaplePortal::MAP_PORTAL)
    end
  end
end
