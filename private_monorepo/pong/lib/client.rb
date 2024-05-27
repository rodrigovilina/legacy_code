require 'logger'
require 'socket'

client = TCPSocket.new('localhost', 1234)
logger = Logger.new(STDOUT)
logger.info 'client started'
message = client.gets.chomp

raise unless message == 'left' || message == 'right'
client.puts "#{message} ok"

player = message
message = nil

logger.info({ player: player }.inspect)

frame = 0

loop do
  logger.info "frame: #{frame}"

  message = "Hello from client at #{Time.now}"
  logger.info "Sending to server: #{message}"
  client.puts message

  response = client.gets.chomp
  logger.info "Received from server: #{response}"

  puts 'sleeping'
  sleep 5
  frame += 1
end

logger.info 'closing client'
client.close
