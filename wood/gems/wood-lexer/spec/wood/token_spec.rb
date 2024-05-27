RSpec::describe Wood::Token do
  let(:token_a) { described_class::new(Wood::TokenType::ID, '1') }

  describe '#initialize' do
    specify { expect(token_a).to be_frozen }
    specify { expect(token_a.lexeme).to eq('1') }
    specify { expect(token_a.type).to be(Wood::TokenType::ID) }
  end

  describe '#==' do
    let(:token_b) { described_class::new(Wood::TokenType::Int, '1') }
    let(:token_c) { described_class::new(Wood::TokenType::ID, '2') }

    specify do
      expect(token_a).to eq(token_a.dup)
      expect(token_a).not_to eq(token_b)
      expect(token_a).not_to eq(token_c)
    end
  end

  describe '#to_s' do
    specify do
      expect(token_a.to_s).to eq('<1:id>')
    end
  end
end
