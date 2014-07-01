require 'find'
require 'fileutils'

# 
def findFiles(dir, pattern)
  Enumerator.new do |yielder|
    Find.find(dir) do |path|
      if (!File.directory?(path) && 
          !%w{. ..}.include?(path) &&
          path =~ pattern)
        yielder.yield path
      end
    end
  end
end

schemes = ['Debug', 'Release']#, 'MinSizeRel']
platforms = [:common, :linux, :apple, :windows]

package_boost = {
  :name => "BOOST",
  :required => true,
  :optional_cmake => ""  # Insert package missing-handler
};

package_foobar = {
  :name => "FOOBAR",
  :required => false,
  :optional_cmake => "# FOOBAR not found"  # Insert package missing-handler
};


project = {
  :name => "lexi",
  :cmake_version => "2.6",
  :targets => [{
      :name => "lexi",
      :install => true,
      :sources => "lexi/Source/",
      :common => {
        :packages => [package_boost],
        :definitions => [ "IS_COMMON"],
        :include_dirs => ["~/local/include"],
        :link_dirs => ["~/local/lib"]
      },
      :apple => {
        :packages => [package_foobar],
        :definitions => ["IS_APPLE"],
        :include_dirs => ["/usr/local/include/", "/usr/X11/include/"],
        :link_dirs => ["/usr/local/lib", "/usr/X11/lib"]
      }
    }
  ]
}

def generatePlatform(platform)
platform[:include_dirs].map {|d| "include_directories(#{d})" }
.concat platform[:link_dirs].map {|d| "link_directories(#{d})" }
.concat platform[:definitions].map {|d| "add_definitions(#{d})" }
.concat platform[:packages].map {|p| 
  result = "FIND_PACKAGE(#{p[:name]} "
  result << (p[:required] ?  "REQUIRED)\n" : "OPTIONAL)\n#{p[:optional_cmake]}")
}
end

def generateSources(sourceRoot)
  sources = ["set(SOURCES)"]
  sources.concat findFiles(sourceRoot, %r{\.cpp$}).map {|s|
    "set(SOURCES $\{SOURCES\} #{s}"
  }
  sources
end

def generateInstalls(sourceRoot, projectName)
  headers = []
  headers.concat findFiles(sourceRoot, %r{\.(h|hpp)$}).map {|h|
    "install (FILES #{h} DESTINATION include/#{projectName})"
  }
end

def generateSourceGroups(sourceRoot)
  findFiles(sourceRoot, %r{\.(cpp|h|hpp)$}).
    group_by {|s| File.dirname(s) }.
    map {|group,files| "source_group(#{group} FILES #{files.join ' '})" }
end

def generateCMake(project, platform, scheme)
  contents = []
  contents << "cmake_minimum_required(VERSION #{project[:cmake_version]})"
  contents << "project(#{project[:name]})"
  project[:targets].each {|x|
    contents.concat generatePlatform(x[:common])
    contents.concat generatePlatform(x[platform])
    contents.concat generateSources(x[:sources])
    contents.concat generateSourceGroups(x[:sources])
    contents.concat generateInstalls(x[:sources], project[:name]) if x[:install]
  }
  contents
end

def hostPlatform
  RUBY_PLATFORM.intern unless RUBY_PLATFORM.downcase =~ /(darwin|mswin|linux)/
    {'darwin' => :apple, 'mswin' => :windows, 'linux' => :linux}[$1]
end

def buildPath(platform, scheme)
end

cmd = ARGV.shift # get the subcommand
case cmd
when "--help"
  abort "Usage: #{__FILE__} [debug|release]"
when "clean" 
else
#  scheme = cmd.nil? ? "Release" : cmd.capitalize
#  abort "Unknown scheme: #{scheme} [#{schemes}]" unless schemes.include? scheme
#  cmd = ARGV.shift

  platform = cmd.nil? ? hostPlatform : cmd.intern
  abort("Unknown platform: #{platform} [#{platforms}]") unless platforms.include? platform

  rootDir = Dir.pwd
  schemes.each {|scheme|
    buildPath = "build/#{platform}-#{scheme.downcase}"
    $stderr.print "Generating #{buildPath}..."
    cmakeContents = generateCMake(project, platform, scheme)

    FileUtils.rm "#{buildPath}/CMakeCache.txt", :force=>true
    FileUtils.mkdir_p buildPath unless Dir.exists? buildPath
    FileUtils.cd buildPath

    File.open("CMakeLists.txt", "w") {|f| f.puts cmakeContents }

    $stderr.puts "done"
    if (platform == hostPlatform)
      $stderr.print "Configuring #{buildPath}..."
      system("cmake -DCMAKE_BUILD_TYPE=#{scheme} ../..")
      $stderr.puts "done"
    end
    FileUtils.cd rootDir
  }
end

