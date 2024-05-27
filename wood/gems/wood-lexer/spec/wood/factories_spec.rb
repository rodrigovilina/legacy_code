RSpec::describe Wood::Factories do
  describe '#Token' do
    specify do
      mod = Module.new
      mod.extend described_class
      expect(mod.Token(:int, '1')).to be_a(Wood::Token)
      expect(mod.Token(Wood::TokenType::Int, '1')).to be_a(Wood::Token)
    end
  end
end
