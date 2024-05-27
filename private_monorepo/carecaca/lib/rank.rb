class Rank
  def initialize(rank)
    @rank = rank
    freeze
  end

  @instances ||= {
    2 => new(:two),
    3 => new(:three),
    4 => new(:four),
    5 => new(:five),
    6 => new(:six),
    7 => new(:seven),
    8 => new(:eight),
    9 => new(:nine),
    10 => new(:ten),
    J: new(:jack),
    Q: new(:queen),
    K: new(:king),
    A: new(:ace),
    joker: new(:joker),
  }

  def self.instances
    @instances.values
  end

  def self.instance(rank)
    @instances.fetch(rank)
  end

  private_class_method :new
end
