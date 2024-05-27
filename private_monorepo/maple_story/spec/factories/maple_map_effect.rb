# frozen_string_literal: true

FactoryBot.define do
  factory :maple_map_effect do
    message { 'MyString' }
    item_id { 1 }

    initialize_with { new(message, item_id) }
  end
end
