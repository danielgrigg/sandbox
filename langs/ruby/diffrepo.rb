require 'find'

def matchSource
  %r{\.(rb|xml)$}
end

def filesMatching(dir, pattern)
  Find.find(dir) do |path|
    if (!File.directory?(path) && 
        !%w{. ..}.include?(path) &&
        path =~ pattern)
      yield path
    end
  end
end

def usage
  puts 'diffrepoo <left> <right>'
end

if ARGV.length < 2
  usage
  exit!
end

repoRoot = Dir.pwd
leftRoot = ((ARGV[0].split('/')).drop(1)).join('/')
rightRoot = ((ARGV[1].split('/')).drop(1)).join('/')
repoLeft = ARGV[0].split('/').first
repoRight = ARGV[1].split('/').first
puts "leftRoot: #{leftRoot}, rightRoot: #{rightRoot}"
puts "leftRepo: #{repoLeft}, rightRepo: #{repoRight}"

matches = {}
filesMatching(ARGV[0], matchSource) {|f|
  path = (f.split('/').drop 1).join('/')
  matches[path] = 'l'
}

filesMatching(ARGV[1], matchSource) {|f|
  path = (f.split('/').drop 1).join('/')
  if (!matches.has_key?(path))
    matches[path] = 'r'
  else
    matches[path] = 'd'
  end
}

matches.each {|k,v| puts "#{k}: #{v}"}

matches.each {|path, status|
  puts ">> #{path}" if status == 'r'
  puts ">> #{path}" if status == 'l'
  if (status == 'd')
    left = File.join(repoLeft, path)
    right = File.join(repoRight,path)
    leftDigest = (`md5 #{left}`.split(' '))[3]
    rightDigest = (`md5 #{right}`.split(' '))[3]
#    puts "leftDigest: #{leftDigest}, rightDigest: #{rightDigest}"
    if (leftDigest != rightDigest)
      puts path
      puts '=' * 60  
      system("diff #{left} #{right}")
      puts '=' * 60  
      puts
    end
  end
}
