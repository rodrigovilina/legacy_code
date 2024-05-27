# typed: strict
# frozen_string_literal: true

class SummonMovementType < T::Enum
  extend T::Sig

  enums do
    STATIONARY = new
    FOLLOW = new
    CIRCLE_FOLLOW = new
  end

  sig { returns(Integer) }
  def value
    { STATIONARY => 0, FOLLOW => 1, CIRCLE_FOLLOW => 3 }.fetch(self)
  end

  sig { params(val: Integer).returns(T.self_type) }
  def self.from_i(val)
    { 0 => STATIONARY, 1 => FOLLOW, 3 => CIRCLE_FOLLOW }.fetch(val)
  end
end
