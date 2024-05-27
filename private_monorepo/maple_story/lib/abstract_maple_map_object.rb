# typed: strict
# frozen_string_literal: true

class AbstractMapleMapObject
  extend T::Sig
  extend T::Helpers

  NilPoint = T.type_alias { T.nilable(Point) }

  abstract!

  sig { params(position: NilPoint, object_id: Integer).void }
  def initialize(position, object_id)
    @position = T.let((position || Point.new(0, 0)), T.nilable(Point))
    @object_id = object_id
  end

  sig { abstract.returns(MapleMapObjectType) }
  def type; end

  sig { returns NilPoint }
  attr_reader :position

  sig { params(position: Point).void }
  def position=(position)
    @position = T.must(@position).move(position.x, position.y)
  end

  sig { returns(Integer) }
  attr_accessor :object_id

  sig { void }
  def nullify_position
    @position = nil
  end
end

__END__

package server.maps;

import java.awt.Point;

public abstract class AbstractMapleMapObject implements MapleMapObject {
  private Point position = new Point();
  private int objectId;

  @Override
  public abstract MapleMapObjectType getType();

  @Override
  public Point getPosition() {
    return new Point(position);
  }

  @Override
  public void setPosition(Point position) {
    this.position.move(position.x, position.y);
  }

  @Override
  public int getObjectId() {
    return objectId;
  }

  @Override
  public void setObjectId(int id) {
    this.objectId = id;
  }

  @Override
  public void nullifyPosition() {
    this.position = null;
  }
}
