# typed: strict

module Wood
  class Parser
    int = tt(:int).bind { ret(Int(_1.lexeme.to_i)) }
    INT = T::let(int, Wood::Parser)
  end
end
