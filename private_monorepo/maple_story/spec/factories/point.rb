# typed: false
# frozen_string_literal: true

FactoryBot.define do
  factory :point do
    x { 0 }
    y { 0 }

    initialize_with { new(x, y) }
  end
end
