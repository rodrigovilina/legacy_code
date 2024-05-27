
RSpec.describe Brainfuck::DataPointer do
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

    context 'when not between 0, 29_999' do
      it 'raises ArgumentError', aggregate_failures: true do
        expect { described_class.new(-1) }.to raise_error ArgumentError
        expect { described_class.new(30_000) }.to raise_error ArgumentError
      end
    end
  end

  describe '#to_i' do
  end

  describe '#to_s' do
  end

  describe '#left' do
    context 'when 0' do
      specify do
        expect(described_class.new(0).left).to eq(described_class.new(29_999))
      end
    end

    context 'else' do
      specify do
        expect(described_class.new(1).left).to eq(described_class.new(0))
      end
    end
  end

  describe '#right' do
    context 'when 29_999' do
      specify do
        expect(described_class.new(29_999).right).to eq(described_class.new(0))
      end
    end

    context 'else' do
      specify do
        expect(described_class.new(0).right).to eq(described_class.new(1))
      end
    end
  end
end
