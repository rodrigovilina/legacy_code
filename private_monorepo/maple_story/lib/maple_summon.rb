# typed: strict
# frozen_string_literal: true

class MapleSummon
  extend T::Sig

  sig { params(owner: MapleCharacter, skill: int, position: Point, movement_type: SummonMovementType).void }
  def initialize(owner, skill, position, movement_type)
    @owner = owner
    @skill = skill
    @skill_level = owner.get_skill_level(SkillFactory.getSkill(skill))
    raise 'Skill level is 0' if @skill_level.zero?

    @movement_type = movement_type
    @position = position
  end

  sig { returns MapleCharacter }
  attr_reader :owner

  sig { returns Integer }
  attr_reader :skill

  sig { returns Byte }
  attr_reader :skill_level

  sig { returns SummonMovementType }
  attr_reader :movement_type
end

__END__

package server.maps;

import java.awt.Point;
import client.MapleCharacter;
import client.MapleClient;
import client.SkillFactory;
import tools.MaplePacketCreator;

public class MapleSummon extends AbstractAnimatedMapleMapObject {
    private MapleCharacter owner;
    private byte skillLevel;
    private int skill, hp;
    private SummonMovementType movementType;

    @Override
    public void sendSpawnData(MapleClient client) {
        client.announce(MaplePacketCreator.spawnSummon(this, false));
    }

    @Override
    public void sendDestroyData(MapleClient client) {
        client.announce(MaplePacketCreator.removeSummon(this, true));
    }


    public int getHP() {
        return hp;
    }

    public void addHP(int delta) {
        this.hp += delta;
    }


    public boolean isStationary() {
        return (skill == 3111002 || skill == 3211002 || skill == 5211001 || skill == 13111004);
    }

    @Override
    public MapleMapObjectType getType() {
        return MapleMapObjectType.SUMMON;
    }

    public final boolean isPuppet() {
      switch (skill) {
        case 3111002:
        case 3211002:
        case 13111004:
          return true;
      }
      return false;
    }
}
