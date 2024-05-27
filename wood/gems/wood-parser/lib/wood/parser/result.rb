# typed: strict

module Wood
  class Parser
    class Result
      extend T::Sig
      extend T::Generic

      Value = type_member
      Remainder = type_member

      sig { returns(T::untyped) }
      attr_reader :value

      sig { returns(T::Array[T::untyped]) }
      attr_reader :remainder

      sig { params(value: T::untyped, remainder: T::Array[T::untyped]).void }
      def initialize(value, remainder)
        @value = value
        @remainder = remainder
        freeze
      end

      sig { params(other: BasicObject).returns(T::Boolean) }
      def ==(other)
        case other
        when Result then T::cast(value == other.value && remainder == other.remainder, T::Boolean)
        else false
        end
      end
      alias eql? ==
    end
  end
end
