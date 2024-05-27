# frozen_string_literal: true

RSpec::describe Wood::Lexer do
  include Wood::Factories

  describe '::for_char' do
    def for_char(char)
      described_class::for_char(char)
    end

    specify { expect(for_char(' ')).to eq(nil) }
    specify { expect(for_char('1')).to eq(Token(:int, '1')) }
    specify { expect(for_char('+')).to eq(Token(:plus, '+')) }
    specify { expect(for_char("'")).to eq(Token(:ichar, "'")) }
    specify { expect(for_char('a')).to eq(Token(:id, 'a')) }
    specify { expect(for_char('z')).to eq(Token(:id, 'z')) }
    specify { expect(for_char('.')).to eq(Token(:dot, '.')) }
    specify { expect { for_char('  ') }.to raise_error RuntimeError }
    specify { expect { for_char('aa') }.to raise_error RuntimeError }
  end

  describe '::for_string' do
    def for_string(string)
      described_class::for_string(string)
    end

    specify { expect(for_string('1')).to eq(Token(:int, '1')) }
    specify { expect(for_string('+')).to eq(Token(:plus, '+')) }
    specify { expect(for_string("'")).to eq(Token(:ichar, "'")) }
    specify { expect(for_string('12')).to eq(Token(:int, '12')) }
    specify { expect(for_string("'a")).to eq(Token(:ichar, "'a")) }
    specify { expect(for_string("'a'")).to eq(Token(:char, "'a'")) }
    specify { expect(for_string('a')).to eq(Token(:id, 'a')) }
    specify { expect(for_string('.')).to eq(Token(:dot, '.')) }
  end

  describe '::call' do
    let(:klass) { described_class }
    specify { expect(klass::('1')).to eq([Token(:int, '1')]) }
    specify { expect(klass::('1+')).to eq([Token(:int, '1'), Token(:plus, '+')]) }
    specify { expect(klass::('1+1')).to eq([Token(:int, '1'), Token(:plus, '+'), Token(:int, '1')]) }
    specify { expect(klass::('92+36')).to eq([Token(:int, '92'), Token(:plus, '+'), Token(:int, '36')]) }
    specify { expect(klass::("'a'")).to eq([Token(:char, "'a'")]) }
    specify { expect(klass::('1.class')).to eq([Token(:int, '1'), Token(:dot, '.'), Token(:id, 'class')]) }
    specify { expect(klass::('1.type')).to eq([Token(:int, '1'), Token(:dot, '.'), Token(:id, 'type')]) }
    # specify { expect(klass::('str.end_with? ending')).to eq([Token(:id, 'str'), Token(:dot, '.'), Token(:id, 'end_with?'), Token(:id, 'ending')]) }
  end
end
