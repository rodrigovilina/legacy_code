# frozen_string_literal: true

require 'zeitwerk'

module CareCaca
  def self.eager_load
    loader = Zeitwerk::Loader.new
    loader.push_dir(__dir__)

    loader.enable_reloading # you need to opt-in before setup
    loader.setup
    loader.eager_load
  end
end

module Kernel
  def Suite(suite)
    Suite.instance(suite)
  end

  def Rank(rank)
    Rank.instance(rank)
  end

  def BackColor(color)
    BackColor.instance(color)
  end

  def CardColor(color)
    CardColor.instance(color)
  end
end

CareCaca.eager_load
