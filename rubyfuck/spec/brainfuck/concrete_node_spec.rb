RSpec.describe Brainfuck::ConcreteNode do
end

RSpec.describe Brainfuck::ConcreteNode::Program do
  describe '#abstract' do
    specify do
      input = '+-<>,.'
      tokens = Brainfuck::Lexer.(input)
      program = Brainfuck::Parser.(tokens).abstract
      expect(program).to eq(
        Brainfuck::AbstractNode::Program.new([
          Brainfuck::AbstractNode::Plus.new(),
          Brainfuck::AbstractNode::Minus.new(),
          Brainfuck::AbstractNode::Left.new(),
          Brainfuck::AbstractNode::Right.new(),
          Brainfuck::AbstractNode::Comma.new(),
          Brainfuck::AbstractNode::Dot.new(),
        ])
      )
    end
  end
end
