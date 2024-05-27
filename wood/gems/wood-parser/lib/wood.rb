# typed: strong

module Wood
  extend T::Sig

  sig { params(string: String).returns(Wood::Node) }
  def self::parse(string)
    Parser::(string)
  end
end
