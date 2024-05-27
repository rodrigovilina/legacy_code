# frozen_string_literal: true

RSpec.describe MapleMapEffect do
  let(:maple_map_effect) { build(:maple_map_effect) }

  specify { expect(maple_map_effect).to be_a(described_class) }

  describe '#make_destroy_data' do
    specify '', aggregate_failures: true do
      allow(MaplePacketCreator).to receive(:remove_map_effect).and_call_original
      expect(maple_map_effect.make_destroy_data).to eq([])
      expect(MaplePacketCreator).to have_received(:remove_map_effect)
    end
  end

  describe '#make_start_data' do
    specify '', aggregate_failures: true do
      allow(MaplePacketCreator).to receive(:start_map_effect).and_call_original
      expect(maple_map_effect.make_start_data).to eq([])
      expect(MaplePacketCreator).to have_received(:start_map_effect)
    end
  end

  describe '#send_start_data' do
    specify do
      client = build(:maple_client)
      allow(client).to receive(:announce).and_return(nil)
      maple_map_effect.send_start_data(client)
      expect(client).to have_received(:announce)
    end
  end
end
