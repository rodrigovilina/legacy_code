class AbstractItem
  BRIE = 'Aged Brie'
  PASS = 'Backstage passes to a TAFKAL80ETC concert'
  SULFURAS = 'Sulfuras, Hand of Ragnaros'

  attr_reader :name, :days_remaining, :quality

  def initialize(name:, days_remaining:, quality:)
    @name = name
    @days_remaining = days_remaining
    @quality = quality
  end
  
  def tick
    raise
  end

  def past_due?
    @days_remaining < 0
  end

  def quality_below_50
    @quality < 50
  end
end

module GildedRose
  def self.new(name:, days_remaining:, quality:)
    self.for(name: name)
        .new(name: name, days_remaining: days_remaining, quality: quality)
  end

  def self.for(name:)
    case
    when name == Item::BRIE then Brie
    when name == Item::PASS then Pass
    when name == Item::SULFURAS then Sulfuras
    else Item
    end
  end

end

class Item < AbstractItem
  def tick
    @days_remaining = @days_remaining - 1
    @quality = @quality - 1 if @quality > 0 
    @quality = @quality - 1 if @quality > 0 && past_due? 
  end
end

class Sulfuras < AbstractItem
  def tick
  end
end

class Pass < AbstractItem
  def tick
    case
    when @days_remaining < 6 && quality_below_50 then @quality = @quality + 3
    when @days_remaining < 11 && quality_below_50 then @quality = @quality + 2
    when quality_below_50 then @quality = @quality + 1
    end
    @days_remaining = @days_remaining - 1
    @quality = 0 if past_due? 
  end
end

class Brie < AbstractItem
  def tick
    @days_remaining = @days_remaining - 1
    @quality = @quality + 1 if @quality < 50 
    @quality = @quality + 1 if @quality < 50 && @days_remaining < 0 
  end
end
