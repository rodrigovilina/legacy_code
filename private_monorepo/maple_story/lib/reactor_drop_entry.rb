# typed: strict
# frozen_string_literal: true

class ReactorDropEntry
  extend T::Sig

  sig { params(item_id: Integer, chance: Integer, quest_id: Integer).void }
  def initialize(item_id, chance, quest_id)
    @item_id = item_id
    @chance = chance
    @quest_id = quest_id
  end

  sig { returns(Integer) }
  attr_reader :item_id

  sig { returns(Integer) }
  attr_reader :chance

  sig { returns(Integer) }
  attr_reader :quest_id
end


__END__

package server.maps;

public class ReactorDropEntry {

  public ReactorDropEntry(int itemId, int chance, int questId) {
    this.itemId = itemId;
    this.chance = chance;
    this.questid = questId;
  }
  public int itemId, chance, questid;
  public int assignedRangeStart, assignedRangeLength;
}
