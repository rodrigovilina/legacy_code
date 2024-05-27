# typed: strict
# frozen_string_literal: true

module AnimatedMapleMapObject
  extend T::Sig
  extend T::Helpers
  extend MapleMapObject

  interface!

  sig { abstract.returns(Integer) }
  def stance; end

  sig { abstract.params(stance: Integer).void }
  def stance=(stance); end

  sig { abstract.returns(T::Boolean) }
  def facing_left?; end
end

__END__

package server.maps;

public interface AnimatedMapleMapObject extends MapleMapObject {
    int getStance();
    void setStance(int stance);
    boolean isFacingLeft();
}
