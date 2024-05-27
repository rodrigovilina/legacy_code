RSpec::describe Wood::Parser do
  include Wood::Factories

  describe '.call' do
    specify { expect(described_class::('1')).to eq(Int(1)) }
    specify { expect(described_class::('12')).to eq(Int(12)) }
    specify { expect(described_class::('1+')).to eq(Int(1)) } # FIXME
    specify { expect(described_class::('93+36')).to eq(Add(Int(93), Int(36))) }
    specify { expect(described_class::("'a'")).to eq(Char('a')) }
    specify { expect(described_class::('1.class')).to eq(Send(Int(1), 'class')) }
    specify { expect(described_class::('1.type')).to eq(Send(Int(1), 'type')) }
    specify { expect(described_class::('str.end_with? ending')).to eq(Send('str', 'end_with?')) }
    xspecify { expect(described_class::('main = 1')).to eq(Asgn('main', Int(1))) }
  end

  describe '.and' do
    specify do
      result = described_class::and(described_class::ITEM, described_class::ITEM).parse('1')
      expect(result).to eq(
        [Wood::Parser::Result::new(Token(:int, '1'), []),
         Wood::Parser::Result::new(Token(:int, '1'), [])]
      )
    end
  end

  describe '.bind' do
    def result(value, remainder)
      Wood::Parser::Result::new(value, remainder)
    end

    specify 'zero >>= f = zero' do
      zero = Wood::Parser::ZERO
      parser = zero.bind Wood::Parser::ITEM
      expect(parser.parse('1')).to eq(zero.parse('1'))
    end

    specify 'p >>= const zero = zero' do
      zero = Wood::Parser::ZERO
      parser = Wood::Parser::ITEM.bind zero
      expect(parser.parse('1')).to eq(zero.parse('1'))
    end

    specify do
      expect(described_class::ITEM.bind { |x|
        described_class::ITEM.bind { |y|
          described_class::ret(x + y) } }.parse(['1', '2'])).to eq([result('12', [])])
    end
  end

  describe '::ITEM' do
    def parse(str)
      Wood::Parser::ITEM.parse(str)
    end

    def result(value, tokens)
      Wood::Parser::Result::new(value, tokens)
    end

    specify { expect(parse([])).to eq([]) }
    specify { expect(parse([1])).to eq([result(1, [])]) }
    specify { expect(parse([1, 2])).to eq([result(1, [2])]) }
  end

  describe '.obj' do
    def obj(o) = W::Parser::obj(o)
    def result(value, tokens) = Wood::Parser::Result::new(value, tokens)

    let(:o) { Object.new }

    specify { expect(obj(o).parse([])).to eq([]) }
    specify { expect(obj(o).parse([1])).to eq([]) }
    specify { expect(obj(o).parse([o])).to eq([result(o, [])]) }
    specify { expect(obj(o).parse([o, 1])).to eq([result(o, [1])]) }
  end

  describe '.ret' do
    def ret(obj)
      Wood::Parser::ret(obj)
    end

    def result(value, tokens)
      Wood::Parser::Result::new(value, tokens)
    end

    let(:obj, &Object::method(:new))

    specify { expect(ret(obj).parse(''.chars)).to eq([result(obj, [])]) }
    specify { expect(ret(obj).parse('1'.chars)).to eq([result(obj, ['1'])]) }
    specify { expect(ret(obj).parse('12'.chars)).to eq([result(obj, ['1', '2'])]) }
  end

  describe '.sat' do
    def sat(&blk)
      Wood::Parser::sat(&blk)
    end

    def result(value, tokens)
      Wood::Parser::Result::new(value, tokens)
    end

    let(:obj) { Object.new }

    specify do
      expect(sat { _1.equal?(obj) }.parse([Object.new])).to eq([])
    end

    specify do
      expect(sat { _1.equal?(obj) }.parse([obj])).to eq([result(obj, [])])
    end

    specify do
      expect(sat { _1.equal?(obj) }.parse([obj, 1])).to eq([result(obj, [1])])
    end
  end

  describe '.tt' do
    def tt(type)
      Wood::Parser::tt(type)
    end

    specify do
      expect(tt(:int).parse('1')).to eq([W::P::Result::new(Token(:int, '1'), [])])
    end
  end

  describe '::ZERO' do
    def parse(str)
      Wood::Parser::ZERO.parse(str)
    end

    specify { expect(parse('1')).to eq([]) }
  end

  describe '#initialize' do
    specify do
      expect(described_class::new() { 1 }).to be_frozen
      expect(described_class::ret(1).parse('1')).to be_an(Array)
    end
  end

  describe '::INT' do
    def parse(str)
      Wood::Parser::INT.parse(str)
    end

    def result(value, tokens)
      Wood::Parser::Result::new(value, tokens)
    end

    specify { expect(parse('1')).to eq([result(Int(1), [])]) }
  end

  describe '::add' do
    specify 'zero ++ p = p' do
      expect(W::P::and(W::P::ZERO, W::P::ITEM).parse([1])).to eq(W::P::ITEM.parse([1]))
    end

    specify 'p ++ zero = p' do
      expect(W::P::and(W::P::ITEM, W::P::ZERO).parse([1])).to eq(W::P::ITEM.parse([1]))
    end

    specify 'p ++ (q ++ r) = (p ++ q) ++ r' do
    end

    specify '(p ++ q) >>= f = (p >>= f) ++ (q >>= f)' do
    end
  end
end
