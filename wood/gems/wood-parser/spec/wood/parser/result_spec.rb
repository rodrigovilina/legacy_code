RSpec::describe Wood::Parser::Result do
  describe '#initialize' do
    specify do
      result = described_class.new(1, [])
      expect(result).to be_frozen
      expect(result.value).to eq(1)
      expect(result.remainder).to eq([])
    end
  end

  describe '#==' do
    specify do
      first_result  = described_class.new(1, [])
      second_result = described_class.new(1, [])
      third_result  = described_class.new(2, [])
      fourth_result = described_class.new(1, [1])
      expect(first_result).to eq(second_result)
      expect(first_result).not_to eq(third_result)
      expect(first_result).not_to eq(fourth_result)
      expect(first_result == 1).to be(false)
    end
  end
end
