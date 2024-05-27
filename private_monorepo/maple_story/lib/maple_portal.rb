# typed: strict
# frozen_string_literal: true

module MaplePortal
  extend T::Sig
  extend T::Helpers

  interface!

  TELEPORT_PORTAL = 1
  MAP_PORTAL = 2
  DOOR_PORTAL = 6
  OPEN = true
  CLOSED = false

  sig { abstract.returns Integer }
  def type; end

  sig { abstract.returns Integer }
  def id; end

  sig { abstract.returns Point }
  def position; end

  sig { abstract.returns String }
  def name; end

  sig { abstract.returns String }
  def target; end

  sig { abstract.returns String }
  def script_name; end

  sig { abstract.params(new_name: String).void }
  def script_name=(new_name); end

  sig { abstract.params(new_status: T::Boolean).void }
  def portal_status=(new_status); end

  sig { abstract.returns T::Boolean }
  def portal_status; end

  sig { abstract.returns Integer }
  def target_map_id; end

  sig { abstract.params(client: MapleClient).void }
  def enter_portal(client); end

  sig { abstract.params(state: T::Boolean).void }
  def portal_state=(state); end

  sig { abstract.returns T::Boolean }
  def portal_state; end
end

__END__

package server.maps;

import java.awt.Point;
import client.MapleClient;

public interface MaplePortal {
    public final int TELEPORT_PORTAL = 1;
    public final int MAP_PORTAL = 2;
    public final int DOOR_PORTAL = 6;
    public static boolean OPEN = true;
    public static boolean CLOSED = false;
    int getType();
    int getId();
    Point getPosition();
    String getName();
    String getTarget();
    String getScriptName();
    void setScriptName(String newName);
    void setPortalStatus(boolean newStatus);
    boolean getPortalStatus();
    int getTargetMapId();
    void enterPortal(MapleClient c);
    void setPortalState(boolean state);
    boolean getPortalState();
}
