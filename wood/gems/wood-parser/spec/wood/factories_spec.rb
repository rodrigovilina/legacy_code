RSpec::describe Wood::Factories do
  let(:mod) { Module::new.tap { _1.extend described_class } }

  describe '#Add' do
    specify do
      expect(mod::Add(mod::Int(1), mod::Int(1))).to be_a(Wood::Node::Add)
    end
  end

  describe '#Asgn' do
    specify do
      expect(mod::Asgn('x', mod::Int(1))).to be_a(Wood::Node::Asgn)
    end
  end

  describe '#Char' do
    specify do
      x = mod::Char('x')
      expect(x).to be_a(Wood::Node::Char)
      expect(mod::Char("'x'")).to eq(x)
    end
  end

  describe '#Send' do
    specify do
      expect(mod::Send(nil, 'message')).to be_a(Wood::Node::Send)
      expect(mod::Send('receiver', 'message').receiver).to eq('receiver')
    end
  end
end
