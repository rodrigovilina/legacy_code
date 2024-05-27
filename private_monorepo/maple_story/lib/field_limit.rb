# typed: strict
# frozen_string_literal: true

class FieldLimit < T::Enum
  extend T::Sig

  enums do
    JUMP = new
  end

  sig { params(int: Integer).returns(FieldLimit) }
  def self.from_i(int)
    { 0x01 => JUMP }.fetch(int)
  end

  sig { returns(Integer) }
  def value
    { JUMP => 0x01 }.fetch(self)
  end

  sig { params(field_limit: Integer).returns(T::Boolean) }
  def check(field_limit)
    field_limit & value == value
  end
end

__END__
package server.maps;

public enum FieldLimit {
  JUMP(0x01),
  MOVEMENTSKILLS(0x02),
  SUMMON(0x04),
  DOOR(0x08),
  CANNOTMIGRATE(0x10),    //change channel, town portal scroll, access cash shop, etc etc
  //NO_NOTES(0x20),
  CANNOTVIPROCK(0x40),
  CANNOTMINIGAME(0x80),
  //SPECIFIC_PORTAL_SCROLL_LIMIT(0x100), // APQ and a couple quest maps have this
  CANNOTUSEMOUNTS(0x200),
  //STAT_CHANGE_ITEM_CONSUME_LIMIT(0x400), // Monster carnival?
  //PARTY_BOSS_CHANGE_LIMIT(0x800), // Monster carnival?
  CANNOTUSEPOTION(0x1000),
  //WEDDING_INVITATION_LIMIT(0x2000), // No notes
  //CASH_WEATHER_CONSUME_LIMIT(0x4000),
  //NO_PET(0x8000), // Ariant colosseum-related?
  //ANTI_MACRO_LIMIT(0x10000), // No notes
  CANNOTJUMPDOWN(0x20000),
  //SUMMON_NPC_LIMIT(0x40000); // Seems to .. disable Rush if 0x2 is set

  //......... EVEN MORE LIMITS ............
  //SUMMON_NPC_LIMIT(0x40000),
  NO_EXP_DECREASE(0x80000),
  //NO_DAMAGE_ON_FALLING(0x100000),
  //PARCEL_OPEN_LIMIT(0x200000),
  DROP_LIMIT(0x400000);
  //ROCKETBOOSTER_LIMIT(0x800000)     //lol we don't even have mechanics <3

  private long i;

  private FieldLimit(long i) {
    this.i = i;
  }

  public long getValue() {
    return i;
  }

  public boolean check(int fieldlimit) {
    return (fieldlimit & i) == i;
  }
}
