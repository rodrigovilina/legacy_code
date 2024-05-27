# typed: strict

module Wood
  class Node
    extend T::Sig

    sig { params(other: BasicObject).returns(T::Boolean) }
    def ==(other)
      case other
      when self.class then self.class.children.all? { |child| public_send(child) == other.public_send(child) }
      else false
      end
    end

    sig { returns(T::Array[Symbol]) }
    def self.children
      []
    end

    class Int < Node
      extend T::Sig

      sig { returns Integer }
      attr_reader :value

      sig { params(value: Integer).void }
      def initialize(value)
        @value = value
        freeze
      end

      sig { returns(T::Array[Symbol]) }
      def self.children
        [:value]
      end
    end

    class Add < Node
      extend T::Sig

      sig { returns Node }
      attr_reader :left

      sig { returns Node }
      attr_reader :right

      sig { params(left: Node, right: Node).void }
      def initialize(left, right)
        @left = left
        @right = right
        freeze
      end

      sig { returns(T::Array[Symbol]) }
      def self.children
        [:left, :right]
      end

    end

    class Char < Node
      extend T::Sig

      sig { returns String }
      attr_reader :char

      sig { params(char: String).void }
      def initialize(char)
        @char = char
        freeze
      end

      sig { returns(T::Array[Symbol]) }
      def self.children
        [:char]
      end
    end

    class Send < Node
      extend T::Sig

      sig { returns(T::untyped) }
      attr_reader :receiver

      sig { returns(String) }
      attr_reader :message

      sig { params(receiver: T::untyped, message: String).void }
      def initialize(receiver, message)
        @receiver = receiver
        @message = message
        freeze
      end

      sig { returns(T::Array[Symbol]) }
      def self.children
        [:receiver, :message]
      end
    end

    class Asgn < Node
      sig { returns String }
      attr_reader :constant

      sig { returns Node }
      attr_reader :value

      sig { params(constant: String, value: Node).void }
      def initialize(constant, value)
        @constant = constant
        @value = value
        freeze
      end

      sig { returns(T::Array[Symbol]) }
      def self.children
        [:constant, :value]
      end
    end
  end
end
