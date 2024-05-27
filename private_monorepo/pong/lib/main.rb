# frozen_string_literal: true

require 'awesome_print'
require 'socket'

class Field
  def initialize(size)
    @size = size
    freeze
  end
end

class Paddle
  def initialize(position)
    @position = position
    @size = 10
    freeze
  end

  def move_up
    self.class.new(@position.move_up)
  end
end

class Vector
  def initialize(x, y)
    @x = x
    @y = y
    freeze
  end

  def move_up
    self.class.new(@x, @y - 1)
  end

  def move_down
    self.class.new(@x, @y + 1)
  end
end

class Ball
  def initialize(position, velocity)
    @position = position
    @velocity = velocity
    freeze
  end
end

class Game
  def initialize(field:, left_paddle:, right_paddle:, ball:, score: [0, 0])
    @field = field
    @left_paddle = left_paddle
    @right_paddle = right_paddle
    @ball = ball
    @score = score
    freeze
  end

  attr_reader :field, :left_paddle, :right_paddle, :ball, :score

  def with(field: @field, left_paddle: @left_paddle, right_paddle: @right_paddle, ball: @ball, score: @score)
    self.class.new(field:, left_paddle:, right_paddle:, ball:, score:)
  end
end

module Events
  class MoveLeftPlayerUp
    def self.call(game)
      game.with(left_paddle: game.left_paddle.move_up)
    end
  end

  class MoveRightPlayerUp
    def self.call(game)
      game.with(right_paddle: game.right_paddle.move_up)
    end
  end

  class MoveLeftPlayerDown
    def self.call(game)
      game.with(left_paddle: game.left_paddle.move_down)
    end
  end

  class MoveRightPlayerDown
    def self.call(game)
      game.with(right_paddle: game.right_paddle.move_down)
    end
  end

  class LeftScore
    def self.call(game)
      game.with(score: game.score.map.with_index { |score, index| index == 0 ? score + 1 : score })
    end
  end

  class RightScore
    def self.call(game)
      game.with(score: game.score.map.with_index { |score, index| index == 1 ? score + 1 : score })
    end
  end

  class MoveBall
    def self.call(game)
      game.with(ball: game.ball.move)
    end
  end
end


$game = Game.new(
  field: Field.new(Vector.new(80, 80)),
  left_paddle: Paddle.new(Vector.new(10, 35)),
  right_paddle: Paddle.new(Vector.new(70, 35)),
  ball: Ball.new(Vector.new(40, 40), Vector.new(1, 1))
)

ap Events::MoveLeftPlayerUp.call($game)

Server = Object.new

def Server.start
  server = TCPServer.new(5678)

  while (session = server.accept)
    request = session.gets
    puts request

    session.puts "Server received: #{request}"
    session.close
  end
end

Client = Object.new

def Client.start
  socket = TCPSocket.new('localhost', 5678)

  socket.puts('Hello, server!')
  response = socket.gets

  puts "Server responded: #{response}"

  socket.close
end

spid = fork do
  Server.start
end

cpid = fork do
  Client.start
end

puts spid
puts cpid

Process.wait(spid)
Process.wait(cpid)
