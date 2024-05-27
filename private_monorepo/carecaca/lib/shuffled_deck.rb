class ShuffledDeck
  def initialize(cards, back_color)
    @cards = cards
    @back_color = back_color
    freeze
  end

  attr_reader :cards
end
