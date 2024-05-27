# typed: strict
# frozen_string_literal: true

class SavedLocationType < T::Enum
  enums do
    BOSSPQ = new
    DEVELOPER = new
    EVENT = new
    FLORINA = new
    FREE_MARKET = new
    HAPPYVILLE = new
    INTRO = new
    MIRROR = new
    MONSTER_CARNIVAL = new
    SUNDAY_MARKET = new
    WORLDTOUR = new
  end
end

__END__
package server.maps;

public enum SavedLocationType {
    FREE_MARKET,
    WORLDTOUR,
    FLORINA,
    INTRO,
    SUNDAY_MARKET,
    MIRROR,
    EVENT,
    BOSSPQ,
    HAPPYVILLE,
    MONSTER_CARNIVAL,
    DEVELOPER;

    public static SavedLocationType fromString(String Str) {
        return valueOf(Str);
    }
}
