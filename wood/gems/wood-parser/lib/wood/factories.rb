# typed: strong

module Wood
  module Factories
    extend T::Sig

    sig { params(value: ::Integer).returns(Wood::Node::Int) }
    def Int(value)
      Node::Int::new(value)
    end

    sig { params(left: Wood::Node, right: Wood::Node).returns(Wood::Node::Add) }
    def Add(left, right)
      Node::Add::new(left, right)
    end

    sig { params(char: String).returns(Wood::Node::Char) }
    def Char(char)
      Node::Char::new(char.gsub("'", ''))
    end

    sig { params(receiver: T.untyped, message: T.untyped).returns(Wood::Node::Send) }
    def Send(receiver, message)
      Node::Send::new(receiver, message)
    end

    sig { params(constant: String, value: Node).returns(::Wood::Node::Asgn) }
    def Asgn(constant, value)
      Node::Asgn::new(constant, value)
    end
  end
end
