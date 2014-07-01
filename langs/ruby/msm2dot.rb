#!/usr/bin/env ruby
#

def parse_transition_table(infile)
  puts "digraph transition_table {"
  while (line = infile.gets)
    line.gsub!(/\s+/,'')
    if (line =~ %r{Row<(\w+),(.+),(\w+),(\w+),(\w+)})
      if ($1 != $3)
        guard = ($5 != "none" ? "[#{$5}]" : "")
        print "#{$1} -> #{$3} [label=\"#{$2}";
        puts "#{guard}\"]"
      end
    elsif (line =~ /\{\};/)
      break
    end
  end
  puts "}"
  nil
end

cppFile = ARGV[0]

File.open(cppFile, "r") do |infile|
  while (line = infile.gets)
    if (line =~ /transition_table/)
      parse_transition_table(infile)
    end
  end
end

