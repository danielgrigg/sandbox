contact = {}; STDIN.each {|line| contact[$1] = $2 if line =~ /(\w*):(.*)/ }

puts "contact:\n#{contact.reduce("") {|s, (k,v)| s << "#{k} -> #{v}\n" }}"


