# typed: strict

module Wood
  class TokenType < T::Enum
    extend T::Sig

    enums do
      Asgn  = new
      Char  = new
      Dot   = new
      IChar = new
      ID    = new
      Int   = new
      Plus  = new
    end

    sig { params(char: String).returns(T::nilable(TokenType)) }
    def self.for_char(char)
      raise unless char.length.equal?(1)

      case char
      in ('0'..'9') then Int
      in ('a'..)    then ID
      in '+'        then Plus
      in '='        then Asgn
      in "'"        then IChar
      in '.'        then Dot
      else nil
      end
    end

    sig { params(string: String).returns(T::nilable(TokenType)) }
    def self.for_string(string)
      case string
      in '='        then Asgn
      in '.'         then Dot
      in '+'         then Plus
      in /\A'\w'\z/    then Char
      in /\A'\w?\z/    then IChar
      in /\A\d+\z/     then Int
      in /\A\w+\??\z/  then ID
      else nil
      end
    end

    sig { returns String }
    def to_s
      serialize
    end
    alias_method :inspect, :to_s
  end
end
