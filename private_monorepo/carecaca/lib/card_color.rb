class CardColor
  def initialize(color)
    @color = color
    freeze
  end

  @instances ||= {
    red: new(:red),
    black: new(:black),
  }
  def self.instances
    @instances.values
  end

  def self.instance(color)
    @instances.fetch(color)
  end

  private_class_method :new
end
