
RSpec.describe Brainfuck::InstructionPointer do
  describe '.default' do
    specify do
      expect(described_class.default.to_i).to be(0)
    end
  end

  describe '.new' do
    context 'when given a non Integer value' do
      it 'raises TypeError' do
        expect { described_class.new(0.0) }.to raise_error TypeError
      end
    end

    context 'when negative' do
      it 'raises ArgumentError' do
        expect { described_class.new(-1) }.to raise_error ArgumentError
      end
    end
  end

  describe '#to_i' do
  end

  describe '#to_s' do
  end

  describe '#advance' do
    specify do
      expect(described_class.new(0).advance).to eq(described_class.new(1))
    end
  end
end
