RSpec.describe Brainfuck::Lexer do
  describe '.call' do
    specify do
      tokens = described_class.('+-<>,.')
      expect(tokens).to eq([
        Brainfuck::Token::Plus.new,
        Brainfuck::Token::Minus.new,
        Brainfuck::Token::Left.new,
        Brainfuck::Token::Right.new,
        Brainfuck::Token::Comma.new,
        Brainfuck::Token::Dot.new,
      ])
    end
  end
end
