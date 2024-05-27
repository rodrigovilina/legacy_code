# typed: strict
# frozen_string_literal: true

require 'socket'
require 'sorbet-runtime'
require 'zeitwerk'

module MapleStory
  def self.eager_load
    loader = Zeitwerk::Loader.new
    loader.push_dir(__dir__)

    loader.enable_reloading # you need to opt-in before setup
    loader.setup
    loader.eager_load
  end
end

MapleStory.eager_load

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
