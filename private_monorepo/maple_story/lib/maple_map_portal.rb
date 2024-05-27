# typed: strict
# frozen_string_literal: true

class MapleMapPortal < MapleGenericPortal
  sig { void }
  def initialize
    super(MaplePortal::MAP_PORTAL)
  end
end

__END__

package server.maps;

public class MapleMapPortal extends MapleGenericPortal {
    public MapleMapPortal() {
        super(MaplePortal.MAP_PORTAL);
    }
}
