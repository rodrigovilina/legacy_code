# typed: strict

module Wood
  class Parser
    char = tt(:char).bind { ret(Char(_1.lexeme)) }
    CHAR = T::let(char, Wood::Parser)
  end
end
