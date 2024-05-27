# typed: strict

module Wood
  class Maybe
    extend T::Sig

    protected

    sig { returns BasicObject }
    attr_reader :val

    public

    sig { params(val: BasicObject).returns(T::attached_class) }
    def self.return(val)
      new(val)
    end

    sig { params(val: BasicObject).void }
    def initialize(val)
      @val = T.let(val, T.untyped)
      freeze
    end


    sig { params(other: BasicObject).returns(T::Boolean) }
    def ==(other)
      case other
      when Maybe then val.equal? other.val
      else false
      end
    end

    sig { params(blk: T.untyped).returns(Wood::Maybe) }
    def bind(&blk)
      case val
      when nil then self
      else blk.call(val)
      end
    end

    sig {returns(String)}
    def to_s
      case val
      when nil then "None"
      else "Just(#{val})"
      end
    end
    alias_method :inspect, :to_s
  end
end
