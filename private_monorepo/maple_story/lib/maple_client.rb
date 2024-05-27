# typed: strict
# frozen_string_literal: true

class MapleClient
  extend T::Sig

  sig { params(data: T::Array[Byte]).void }
  def announce(data); end
end
