# frozen_string_literal: true
# typed: strong

module Wood
  class Token
    extend T::Sig

    sig { returns(TokenType) }
    attr_reader :type

    sig { returns(String) }
    attr_reader :lexeme

    sig { params(type: TokenType, lexeme: String).void  }
    def initialize(type, lexeme)
      @type = type
      @lexeme = lexeme
      freeze
    end

    sig { params(other: ::Wood::Token).returns(T::Boolean) }
    def ==(other)
      type.equal?(other.type) && lexeme.eql?(other.lexeme)
    end
    alias eql? ==

    sig { returns(String) }
    def to_s
      "<#{lexeme}:#{type}>"
    end
    alias_method :inspect, :to_s
  end
end
