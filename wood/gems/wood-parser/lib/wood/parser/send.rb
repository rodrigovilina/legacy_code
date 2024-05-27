module Wood
  class Parser
    send = INT.bind { |r| tt(:dot).bind { ID.bind { |m| ret(Send(r, m)) } } }
    SEND = T::let(send, Wood::Parser)
  end
end
