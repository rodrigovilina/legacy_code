class Deck
  def initialize(back_color)
    @cards = [:heart, :spades, :diamonds, :clubs].product([*2..10, :J, :Q, :K, :A]).map do |suite, rank|
      PlayCard.new(suite, rank, back_color)
    end + [Joker.new(:red, back_color), Joker.new(:black, back_color)]
    @back_color = BackColor(back_color)
    freeze
  end

  attr_reader :cards

  def shuffle
    ShuffledDeck.new(@cards.shuffle, @back_color)
  end
end
