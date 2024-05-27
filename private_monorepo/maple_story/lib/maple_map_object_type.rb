# typed: strict
# frozen_string_literal: true

# HeavenMS/src/server/maps/MapleMapObjectType.java

class MapleMapObjectType < T::Enum
  enums do
    DOOR = new
    DRAGON = new
    HIRED_MERCHANT = new
    ITEM = new
    KITE = new
    MINI_GAME = new
    MIST = new
    MONSTER = new
    NPC = new
    PLAYER = new
    PLAYER_NPC = new
    REACTOR = new
    SHOP = new
    SUMMON = new
  end
end

__END__

package server.maps;

public enum MapleMapObjectType {
    NPC, MONSTER, ITEM, PLAYER, DOOR, SUMMON, SHOP, MINI_GAME, MIST, REACTOR, HIRED_MERCHANT, PLAYER_NPC, DRAGON, KITE;
}
