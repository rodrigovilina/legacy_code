
RSpec::describe Wood::Maybe do
  describe '.new' do
    specify do
      expect(described_class::new(nil)).to be_frozen
    end
  end
  describe '.return' do
    context 'when given nil' do
      specify { expect(described_class::return(nil)).to eq(described_class::new(nil)) }
    end

    context 'when given anything else' do
      specify { expect(described_class::return(1)).to eq(described_class::new(1)) }
    end
  end

  describe '#bind' do
    context 'when None' do
      specify do
        expect(described_class::return(nil).bind { |i| 1 }).to eq(described_class::return(nil))
        expect(described_class::return(nil).bind { |i| 1 }).not_to eq(described_class::return(1))
        expect(described_class::return(nil).bind { |i| 1 }).not_to eq(nil)
        expect(described_class::return(1).bind { |i| Wood::Maybe::return(i + 1) }).to eq(described_class::return(2))
      end
    end
  end

  describe '#==' do
    specify do
      expect(described_class::return(1) == described_class::return(1)).to eq(true)
      expect(described_class::return(1) == described_class::return(2)).to eq(false)
      expect(described_class::return(1) == 2).to eq(false)
    end
  end

  describe '#to_s' do
    specify do
      expect(described_class::return(nil).to_s).to eq('None')
      expect(described_class::return(1).to_s).to eq('Just(1)')
    end
  end
end
