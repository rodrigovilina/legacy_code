# frozen_string_literal: true

RSpec.describe ReactorDropEntry do
  subject(:reactor_drop_entry) { described_class.new(item_id, chance, quest_id) }

  let(:item_id) { 123 }
  let(:chance) { 50 }
  let(:quest_id) { 456 }

  describe '#initialize' do
    it 'sets the item_id attribute' do
      expect(reactor_drop_entry.item_id).to eq(item_id)
    end

    it 'sets the chance attribute' do
      expect(reactor_drop_entry.chance).to eq(chance)
    end

    it 'sets the quest_id attribute' do
      expect(reactor_drop_entry.quest_id).to eq(quest_id)
    end
  end
end
