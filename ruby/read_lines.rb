counter = 1
file = File.new("read_lines.rb", "r")
while (line = file.gets)
  puts "#{counter}: #{line}"
  counter = counter + 1
end
file.close

File.open("read_lines.rb", "r") do |infile|
  while (line = infile.gets)
    puts "#{counter}: #{line}"
    counter = counter + 1
  end
end

counter = 1
begin
  file = File.new("read_lines.rb", "r")
  while (line = file.gets)
    puts "#{counter}: #{line}"
    counter = counter + 1
  end
  file.close
rescue => err
  puts "Exception: #{err}"
  err
end
