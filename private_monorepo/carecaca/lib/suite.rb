class Suite
  def initialize(suite)
    @suite = suite
    freeze
  end

  @instances ||= {
    heart: new(:heart),
    diamonds: new(:diamonds),
    spades: new(:spades),
    clubs: new(:clubs),
    joker: new(:joker),
  }

  def self.instances
    @instances.values
  end

  def self.instance(suite)
    @instances.fetch(suite)
  end

  private_class_method :new
end
