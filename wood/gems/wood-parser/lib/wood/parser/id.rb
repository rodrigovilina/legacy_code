
module Wood
  class Parser
    id = tt(:id).bind { ret(_1.lexeme) }
    ID = T::let(id, Wood::Parser)
  end
end
