RSpec::describe Wood::Node do
  describe '.children' do
    specify { expect(described_class::children).to eq([]) }
  end

  describe '#==' do
    specify do
      one = ->(){Wood::Node::Int.new(1)}
      two = ->(){Wood::Node::Int.new(2)}
      expect(described_class::Add.new(one.(), two.())).to eq(described_class::Add.new(one.(), two.()))
      expect(described_class::Add.new(one.(), two.())).not_to eq(described_class::Add.new(one.(), one.()))
      expect(described_class::Add.new(one.(), two.())).not_to eq(described_class::Add.new(two.(), two.()))
      expect(described_class::Add.new(one.(), two.()) == 1).to be(false)
    end
  end
end

RSpec::describe Wood::Node::Add do
  describe '#initialize' do
    specify do
      one = Wood::Node::Int.new(1)
      two = Wood::Node::Int.new(2)
      node = described_class.new(one, two)
      expect(node).to be_frozen
      expect(node.left).to be_frozen
      expect(node.right).to be_frozen
    end
  end

  describe '.children' do
    specify { expect(described_class::children).to eq([:left, :right]) }
  end
end

RSpec::describe Wood::Node::Asgn do
  describe '#initialize' do
    specify do
      one = Wood::Node::Int.new(1)
      node = described_class.new('constant', one)
      expect(node).to be_frozen
      expect(node.constant).to eq('constant')
      expect(node.value).to eq(one)
    end
  end

  describe '.children' do
    specify { expect(described_class::children).to eq([:constant, :value]) }
  end
end

RSpec::describe Wood::Node::Char do
  describe '#initialize' do
    specify do
      node = described_class.new('a')
      expect(node).to be_frozen
      expect(node.char).to eq('a')
    end
  end

  describe '.children' do
    specify { expect(described_class::children).to eq([:char]) }
  end
end

RSpec::describe Wood::Node::Send do
  describe '#initialize' do
    specify do
      node = described_class.new('receiver', 'message')
      expect(node).to be_frozen
      expect(node.receiver).to eq('receiver')
      expect(node.message).to eq('message')
    end
  end

  describe '.children' do
    specify { expect(described_class::children).to eq([:receiver, :message]) }
  end
end
