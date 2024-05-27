# typed: strict

module Wood
  class Parser
    zero = new { [] }
    ZERO = T::let(zero, Wood::Parser)
  end
end
