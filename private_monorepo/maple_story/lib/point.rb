# typed: strict
# frozen_string_literal: true

class Point
  extend T::Sig

  sig { params(x: Integer, y: Integer).void }
  def initialize(x, y) # rubocop:disable Naming/MethodParameterName
    @x = x
    @y = y
  end

  sig { returns Integer }
  attr_reader :x

  sig { returns Integer }
  attr_reader :y

  sig { params(x: Integer, y: Integer).returns(T.self_type) }
  def move(x, y) # rubocop:disable Naming/MethodParameterName
    Point.new(x, y)
  end

  sig { params(other: BasicObject).returns(T::Boolean) }
  def ==(other)
    case other
    when Point then x == other.x && y == other.y
    else false
    end
  end
end
