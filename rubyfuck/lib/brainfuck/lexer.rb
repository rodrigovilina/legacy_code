module Brainfuck
  class Lexer
    def self.call(string)
      tokens = []
      string = string

      until string.empty? do
        token, string = lex_token(string)
        tokens << token
      end

      tokens
    end

    def self.lex_token(string)
      char = string[0]
      rest = string[1..]

      token = case char
      in '<' then Token::Left.new
      in '>' then Token::Right.new
      in '.' then Token::Dot.new
      in ',' then Token::Comma.new
      in '+' then Token::Plus.new
      in '-' then Token::Minus.new
      in '[' then Token::Loop.new
      in ']' then Token::End.new
      else
        lexeme = string.chars.take_while { !%w{[ ] , . + - < >}.include? _1 }.join
        rest = string[lexeme.length..]
        Token::Other.new(lexeme)
      end

      return token, rest
    end
  end
end
