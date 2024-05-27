# typed: strict
# frozen_string_literal: true

module MapleMapObject
  extend T::Sig
  extend T::Helpers

  interface!

  sig { abstract.returns(Integer) }
  def object_id; end

  sig { abstract.params(id: Integer).void }
  def object_id=(id); end

  sig { abstract.returns(MapleMapObjectType) }
  def type; end

  sig { abstract.returns(Point) }
  def position; end

  sig { abstract.params(position: Point).void }
  def position=(position); end

  sig { abstract.params(client: MapleClient).void }
  def send_spawn_data(client); end

  sig { abstract.params(client: MapleClient).void }
  def send_destroy_data(client); end

  sig { abstract.void }
  def nullify_position; end
end

__END__

package server.maps;

import java.awt.Point;
import client.MapleClient;

public interface MapleMapObject {
    public int getObjectId();
    public void setObjectId(int id);
    public MapleMapObjectType getType();
    public Point getPosition();
    public void setPosition(Point position);
    public void sendSpawnData(MapleClient client);
    public void sendDestroyData(MapleClient client);
    public void nullifyPosition();
}
