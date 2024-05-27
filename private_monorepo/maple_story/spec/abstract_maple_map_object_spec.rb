# frozen_string_literal: true

RSpec.describe AbstractMapleMapObject do
  let(:klass) { Class.new(described_class) }

  describe '#nullify_position' do
    specify do
      maple_map_object = klass.new(Point.new(0, 0), 0)
      maple_map_object.nullify_position
      expect(maple_map_object.instance_variable_get(:@position)).to be_nil
    end
  end
end
