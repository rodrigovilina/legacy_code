require 'logger'
require 'socket'

mutex = Mutex.new

a = 0

server = TCPServer.new(1234)
logger = Logger.new(STDOUT)

def left_thread_block(client, logger, mutex)
  dir = 'left'
  logger.info("#{dir} thread started")
  client.puts(dir)
  message = client.gets.chomp
  raise "#{dir} not acknowledged" unless message == "#{dir} ok"

  loop do
    message = client.gets.chomp
    mutex.synchronize do
      a += 1
      logger.info "a: #{a} from #{dir}"
    end
    logger.info "Received from client #{dir}: #{message}"

    response = "Hello from server at #{Time.now}"
    logger.info "Sending to client #{dir}: #{response}"
    client.puts response
  rescue
  end

  logger.info "closing client #{dir}"
  client.close
end

def right_thread_block(client, logger, mutex)
  logger.info 'right thread started'
  client.puts('right')
  message = client.gets.chomp
  raise 'right not acknowledged' unless message == 'right ok'

  loop do
    message = client.gets.chomp
    mutex.synchronize do
      a += 1
      logger.info "a: #{a} from right"
    end
    logger.info "Received from client: #{message}"

    response = "Hello from server at #{Time.now}"
    logger.info "Sending to client: #{response}"
    client.puts response

  rescue
  end

  logger.info 'closing client'
  client.close
end

left_thread = Thread.start(server.accept) { |client| left_thread_block(client, logger, mutex) }
right_thread = Thread.start(server.accept) { |client| right_thread_block(client, logger, mutex) }

sleep

left_thread.join
right_thread.join
