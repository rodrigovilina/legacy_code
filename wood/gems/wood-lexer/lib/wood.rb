# typed: strong

module Wood
  extend T::Sig

  sig { params(string: String).returns(T::Array[Token]) }
  def self::lex(string)
    Lexer::(string)
  end
end
W = Wood
