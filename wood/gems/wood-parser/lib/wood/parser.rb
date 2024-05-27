# typed: strict

require 'wood/lexer'
require_relative 'node'
require_relative 'factories'
require_relative 'parser/result'

module Wood
  class Parser
    extend T::Sig
    extend Wood::Factories

    sig do
      params(blk: T::proc.params(arg0: T.untyped).returns(T::Array[Result]))
      .void
    end
    def initialize(&blk)
      @blk = blk
      freeze
    end

    class << self
      extend T::Sig

      # (++) :: Parser a -> Parser a -> Parser a
      sig do
        params(parser_a: Wood::Parser, parser_b: Wood::Parser)
        .returns(T.attached_class)
      end
      def and(parser_a, parser_b)
        new do |tokens|
          [parser_a, parser_b].flat_map { _1.parse(tokens) }
        end
      end

      # (>>=) :: Parser a -> (a -> Parser b) -> Parser b
      sig do
        params(parser: T.untyped, blk: T.untyped)
        .returns(T.attached_class)
      end
      def bind(parser, &blk)
        new do |tokens|
          parser.parse(tokens).flat_map do |result|
            blk.call(result.value).parse(result.remainder)
          end
        end
      end

      sig { params(str: T::any(String, T::Array[T::untyped])).returns(Node) }
      def call(str)
        [INT, CHAR, ADD, SEND]
          #SEND2, ASGN]
          .flat_map { _1.parse(str) }
          .sort_by { _1.remainder.length }
          .first
          .value
      end

      sig { params(o: BasicObject).returns(T.attached_class) }
      def obj(o)
        sat { _1 == o }
      end

      sig do
        params(value: T::untyped, remainder: T::Array[T::untyped])
        .returns(Result[T::untyped, T::untyped])
      end
      def result(value, remainder)
        Result::new(value, remainder)
      end

      sig { params(obj: BasicObject).returns(T.attached_class) }
      def ret(obj)
        new { |tokens| [result(obj, tokens)] }
      end

      sig do
        params(blk: T::proc.params(arg0: T::untyped).returns(T::Boolean))
        .returns(T::attached_class)
      end
      def sat(&blk)
        ITEM.bind do |obj|
          blk.call(obj) ? ret(obj) : ZERO
        end
      end

      sig { params(type: Symbol).returns(T.attached_class) }
      def tt(type)
        token_type = TokenType::deserialize(type.to_s)
        sat { _1.type == token_type }
      end
    end

    sig do
      params(tokens: T::any(T::Array[T::untyped], String))
      .returns(T::Array[T::untyped])
    end
    def parse(tokens)
      case tokens
      when String then @blk.call(Lexer::(tokens))
      else @blk.call(tokens)
      end
    end

    sig { params(parser: T.untyped, blk: T.untyped).returns(T.untyped) }
    def bind(parser = nil, &blk)
      if parser
        self.class.bind(self) { parser }
      else
        self.class.bind(self, &blk)
      end
    end

    require_relative 'parser/zero'
    require_relative 'parser/item'
    require_relative 'parser/int'
    require_relative 'parser/char'
    require_relative 'parser/id'
    require_relative 'parser/add'
    require_relative 'parser/send'

    SEND2 = T::let(ID.bind { |r| tt(:dot).bind { ID.bind { |m| ID.bind { |a0| ret(Send(r, m)) } } } }, Wood::Parser)
    ASGN  = T::let(
      ID.bind { |c| tt(:asgn).bind {
        INT.bind { |v| ret(Asgn(c, v)) }
      }},
      Wood::Parser
    )
  end
  P = Parser
end

__END__

class << self

  def plus_plus_plus(first, second)
    new { |tokens| result(*Array(plus_plus(first, second).parse(tokens).first)) }
  end

  def ppp(first, &second)
    new do |tokens|
      i = new do |ts|
        rs = first.parse(ts)
        if rs.first
          rs
        else
          rs + yield.parse(ts)
        end
      end.parse(tokens)
    end
  end

  def string(str)
    case str
    when [] then ret([])
    else char(str.first).bind { string(str[1..]).bind { ret(str)} }
    end
  end

  def many1(parser)
    parser.bind { |a| many(parser).bind { |as| ret([a, *as]) } }
  end

  def many(parser)
    plus_plus_plus(many1(parser), ret([]))
  end

  def sepby1(p, sep)
    p.bind { |a| many(sep.bind {p}).bind { |as| ret([a, *as]) } }
  end

  def sepby(first, second)
    plus_plus_plus(sepby1(first, second), ret([]))
  end

  def chainl1(p, op)
    rest = ->(a) { op.bind { |f| p.bind { |b| rest.(f.(a,b)) } } }

    p.bind { |a| rest.(a) }
  end

  def chainl(p, op, a)
    plus_plus_plus(chainl1(p, op), ret(a))
  end

  def space
    many(sat { _1 == ' ' })
  end

  def token(parser)
    parser.bind { |a| space.bind { ret(a) } }
  end

  def symb(tokens)
    token(string(tokens))
  end

  def apply(parser, tokens)
    space.bind { parser }.parse(tokens)
  end

  def type(t)
    sat { _1.type == t }
  end

  def s
    ppp(add) { int } #+ _call
  end

  def int
    sat { _1.type == :int }.bind { ret(Int(_1.lexeme.to_i)) }
  end

  def add
    ppp(int.bind { |a| type(:plus).bind { int.bind { |b| ret(Add(a, b)) } } }) do
      add.bind { |a| type(:plus).bind { int.bind { |b| ret(Add(a, b)) } } }
    end
  end

  def _call
    int.bind { |a| type(:dot).bind { _id.bind { |b| ret(Call(a, b)) } } }
  end
end

def +(other)
  self.class.plus_plus(self, other)
end

def |(other)
  self.class.plus_plus_plus(self, other)
end
