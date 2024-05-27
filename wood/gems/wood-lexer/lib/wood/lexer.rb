# typed: strict
# frozen_string_literal: true

require 'sorbet-runtime'
require_relative 'lexer/version'
require_relative 'maybe'
require_relative 'token_type'
require_relative 'token'
require_relative 'factories'

module Wood
  module Lexer

    extend Factories
    extend T::Sig

    sig { params(string: String).returns(T::Array[Token]) }
    def self.call(string)
      string.chars.reduce([]) do |tokens, char|
        lex_step(tokens, char)
      end#.compact
    end

    sig { params(tokens: T::Array[Token], char: String).returns T::Array[Token] }
    def self.lex_step(tokens, char)
      last_token = tokens.last

      new_token = (last_token.then { for_string(_1.lexeme + char) } if last_token)

      if new_token
        tokens.take(tokens.size - 1) + [new_token]
      else
        tokens.concat([T.must(for_char(char))])
      end
    end

    sig { params(char: String).returns(T::nilable(Token)) }
    def self.for_char(char)
      # raise unless char.length.equal?(1)

      type = TokenType::for_char char

      case type
      in TokenType then Token(T::must(type), char)
      in nil    then nil
      end
    end

    sig { params(string: String).returns(T::nilable(Token)) }
    def self.for_string(string)
      type = TokenType::for_string string

      case type
      in TokenType then Token(T::must(type), string)
      in nil    then nil
      end
    end
  end
  L = Lexer
end
W = Wood
