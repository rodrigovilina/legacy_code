module Wood
  class Parser
    add = INT.bind { |l| tt(:plus).bind { INT.bind { |r| ret(Add(l, r)) } } }
    ADD = T::let(add, Wood::Parser)
  end
end
