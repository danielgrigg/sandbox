
#ruby -ne 'puts $1 if $_ =~ /<div class="rank"\D+(\d+)|<div class="score"\D+(\d+)/'

suburbs = File.read("suburbs.txt").split
suburbs.each do |burbName|

  File.open("#{burbName}.html", "r") do |profile|
    print "#{burbName},"
    while (line = profile.gets)
      print "rank=#{$1}," if line =~ /<div class="rank"\D+(\d+)/
      print "score=#{$1}," if line =~ /<div class="score"\D+(\d+\.\d+)/
    end
    puts
  end
end
