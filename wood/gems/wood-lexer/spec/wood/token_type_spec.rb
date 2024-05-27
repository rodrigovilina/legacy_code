RSpec::describe Wood::TokenType do
  describe '.for_char' do
    def for_char(char)
      described_class::for_char(char)
    end

    context 'when given more than one char' do
      specify { expect { for_char('  ') }.to raise_error RuntimeError }
      specify { expect { for_char('') }.to raise_error RuntimeError }
      specify { expect { for_char('aa') }.to raise_error RuntimeError }
    end

    specify { expect(for_char('=')).to eq(described_class::Asgn) }
    specify { expect(for_char('.')).to eq(described_class::Dot) }
    specify { expect(for_char("'")).to eq(described_class::IChar) }
    specify { expect(for_char('a')).to eq(described_class::ID) }
    specify { expect(for_char('z')).to eq(described_class::ID) }
    specify { expect(for_char('0')).to eq(described_class::Int) }
    specify { expect(for_char('9')).to eq(described_class::Int) }
    specify { expect(for_char('+')).to eq(described_class::Plus) }
    specify { expect(for_char('`')).to eq(nil) }
  end

  describe '.for_string' do
    def for_string(string)
      described_class::for_string(string)
    end

    specify do
      expect(for_string(' ')).to eq(nil)
      expect(for_string('=')).to eq(described_class::Asgn)
      expect(for_string("'a'")).to eq(described_class::Char)
      expect(for_string("'a")).to eq(described_class::IChar)
      expect(for_string('.')).to eq(described_class::Dot)
      expect(for_string('1')).to eq(described_class::Int)
      expect(for_string('a')).to eq(described_class::ID)
      expect(for_string('+')).to eq(described_class::Plus)
    end
  end

  describe '#to_s' do
    specify do
      expect(described_class::Int.to_s).to eq('int')
    end
  end
end
