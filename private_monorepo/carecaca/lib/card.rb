class Card
  def back_color
  end

  def rank
  end

  def suite
  end

  def color
  end
end

class PlayCard < Card
  def initialize(suite, rank, back_color)
    @suite = Suite(suite)
    @rank = Rank(rank)
    @back_color = BackColor(back_color)
    @color = CardColor(suite == :heart || suite == :diamonds ? :red : :black)
    freeze
  end
end

class Joker < Card
  def initialize(color, back_color)
    @color = CardColor(color)
    @rank = Rank(:joker)
    @suite = Suite(:joker)
    @back_color = BackColor(back_color)
    freeze
  end
end
