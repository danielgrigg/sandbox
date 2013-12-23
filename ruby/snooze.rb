require 'socket'
port = ARGV[0] || "8080"
server = TCPServer.new port.to_i

while true
  puts "listening on #{port}"
  s = server.accept
  request = s.gets
  puts "Got request: #{request}"
  if request =~ /get\s+\/(\S+)\s+/i
    s.puts "HTTP/1.1 200 OK"
    s.puts
    s.puts "G'day #{$1}! The time here is #{Time.now}."
  else
    s.puts "HTTP/1.1 404 Not Found"
    s.puts
    s.puts "Give me your name like this buddy, http://localhost/Bob"
  end
    sleep 1
    s.close
end
