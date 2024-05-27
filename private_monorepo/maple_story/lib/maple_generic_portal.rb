# typed: strict
# frozen_string_literal: true

class MapleGenericPortal
  extend T::Sig
  include MaplePortal

  sig { params(type: Integer).void }
  def initialize(type)
    @type = type

    @id = T.let(0, Integer)
    @name = T.let('', String)
    @portal_state = T.let(false, T::Boolean)
    @portal_status = T.let(false, T::Boolean)
    @script_name = T.let('', String)
    @target = T.let('', String)
    @target_map_id = T.let(0, Integer)
    @position = T.let(Point.new(0, 0), Point)
  end

  sig { override.returns(Integer) }
  attr_reader :type

  sig { override.returns(Integer) }
  attr_reader :id

  sig { override.returns(Point) }
  attr_reader :position

  sig { override.returns(String) }
  attr_reader :name

  sig { override.returns(String) }
  attr_reader :target

  sig { override.returns(String) }
  attr_reader :script_name

  sig { override.returns(T::Boolean) }
  attr_accessor :portal_status

  sig { override.returns(Integer) }
  attr_reader :target_map_id

  sig { override.returns(T::Boolean) }
  attr_accessor :portal_state

  sig { override.params(_client: MapleClient).void }
  def enter_portal(_client); end

  sig { override.params(script_name: String).void }
  def script_name=(script_name); end
end
