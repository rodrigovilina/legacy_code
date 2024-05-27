RSpec.describe Brainfuck::Compiler::Bfa do
  describe '.call' do
    context 'when input = +-' do
      specify do
        input = '+-<>,.'
        tokens = Brainfuck::Lexer.(input)
        program = Brainfuck::Parser.(tokens).abstract
        output = described_class.(program)
        expect(output).to eq(<<~BFA)
          INC
          DEC
          LEFT
          RIGHT
          READ
          WRITE
        BFA
      end
    end
  end
end
