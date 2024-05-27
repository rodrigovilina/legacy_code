# typed: strict
# frozen_string_literal: true

class SavedLocation
  extend T::Sig

  SOME_CONSTANT = 102_000_000

  sig { params(map_id: Integer, portal: Integer).void }
  def initialize(map_id, portal)
    @map_id = map_id
    @portal = portal
  end

  sig { returns(Integer) }
  attr_reader :map_id

  sig { returns(Integer) }
  attr_reader :portal
end

__END__

package server.maps;

public class SavedLocation {
    private int mapid = 102000000,  portal;

    public SavedLocation(int mapid, int portal) {
        this.mapid = mapid;
        this.portal = portal;
    }

    public int getMapId() {
        return mapid;
    }

    public int getPortal() {
        return portal;
    }
}
