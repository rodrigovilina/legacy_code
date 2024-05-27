# typed: strict
# frozen_string_literal: true

class MaplePacketCreator
  extend T::Sig

  sig { returns(T::Array[Byte]) }
  def self.remove_map_effect
    []
  end

  sig { params(_message: String, _item_id: Integer, _active: TrueClass).returns(T::Array[Byte]) }
  def self.start_map_effect(_message, _item_id, _active)
    []
  end
end
