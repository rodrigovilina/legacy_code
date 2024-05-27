# frozen_string_literal: true
# typed: strong

module Wood
  module Factories
    extend T::Sig

    sig { params(type: T::any(TokenType,Symbol), lexeme: String).returns(Wood::Token) }
    def Token(type, lexeme)
      type = TokenType::deserialize(type.to_s)

      Token::new(type, lexeme)
    end
  end
end
