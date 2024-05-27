module Wood
  class Parser
    item = new { |tokens| tokens.first ? [result(tokens.first, tokens.drop(1))] : [] }
    ITEM = T::let(item, Wood::Parser)
  end
end
