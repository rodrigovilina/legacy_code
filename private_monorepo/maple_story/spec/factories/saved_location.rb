# frozen_string_literal: true

FactoryBot.define do
  factory :saved_location do
    map_id { 0 }
    portal { 0 }

    initialize_with { new(map_id, portal) }
  end
end
