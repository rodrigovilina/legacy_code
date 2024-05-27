# typed: strict
# frozen_string_literal: true

class MapleMapEffect
  extend T::Sig

  sig { params(message: String, item_id: Integer).void }
  def initialize(message, item_id)
    @message = message
    @item_id = item_id
    @active = T.let(true, TrueClass)
  end

  sig { returns(T::Array[Byte]) }
  def make_destroy_data
    MaplePacketCreator.remove_map_effect
  end

  sig { returns(T::Array[Byte]) }
  def make_start_data
    MaplePacketCreator.start_map_effect(@message, @item_id, @active)
  end

  sig { params(client: MapleClient).void }
  def send_start_data(client)
    client.announce make_start_data
  end
end


__END__

package server.maps;

import client.MapleClient;
import tools.MaplePacketCreator;

public class MapleMapEffect {
  private String msg;
  private int itemId;
  private boolean active = true;

  public MapleMapEffect(String msg, int itemId) {
    this.msg = msg;
    this.itemId = itemId;
  }

  public final byte[] makeDestroyData() {
    return MaplePacketCreator.removeMapEffect();
  }

  public final byte[] makeStartData() {
    return MaplePacketCreator.startMapEffect(msg, itemId, active);
  }

  public void sendStartData(MapleClient client) {
    client.announce(makeStartData());
  }
}
