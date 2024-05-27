RSpec.describe Brainfuck::Parser do
  describe '.call' do
    specify 'plus' do
      tokens = Brainfuck::Lexer.('+-<>,.')
      node = described_class.(tokens)
      expect(node).to eq(
        Brainfuck::ConcreteNode::Program.new([
          Brainfuck::ConcreteNode::Plus.new,
          Brainfuck::ConcreteNode::Minus.new,
          Brainfuck::ConcreteNode::Left.new,
          Brainfuck::ConcreteNode::Right.new,
          Brainfuck::ConcreteNode::Comma.new,
          Brainfuck::ConcreteNode::Dot.new,
        ])
      )
    end
  end
end
